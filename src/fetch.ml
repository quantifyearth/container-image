open Container_image_spec

let registry_base = "https://registry-1.docker.io"
let auth_base = "https://auth.docker.io"
let auth_service = "registry.docker.io"
let uri fmt = Fmt.kstr Uri.of_string fmt

type response = {
  content_type : Media_type.t;
  content_length : int64 option;
  content_digest : Digest.t option;
  body : Eio.Flow.source_ty Eio.Resource.t;
}

(* TODO: manage [Range] headers *)
let rec get client ~sw ?token ?(extra_headers = []) uri out =
  Logs.debug (fun l -> l "GET %a\n%!" Uri.pp uri);
  let headers =
    Cohttp.Header.of_list
    @@ (match token with
       | Some token -> [ ("Authorization", "Bearer " ^ token) ]
       | None -> [])
    @ extra_headers
  in
  let resp, body = Cohttp_eio.Client.get ~headers client ~sw uri in
  let headers = Cohttp.Response.headers resp in
  let content_type =
    match Cohttp.Header.get headers "Content-Type" with
    | Some m -> (
        match Media_type.of_string m with
        | Ok m -> m
        | Error (`Msg e) -> Fmt.failwith "invalid content-type: %s - %s" m e)
    | None -> failwith "missing content-type"
  in
  let content_length =
    match Cohttp.Header.get headers "Content-Length" with
    | Some l -> Some (Int64.of_string l)
    | None -> None
  in
  let content_digest =
    match Cohttp.Header.get headers "Docker-Content-Digest" with
    | Some s -> (
        match Digest.of_string s with
        | Ok s -> Some s
        | Error (`Msg e) -> Fmt.failwith "%s: invalid digest header: %s" s e)
    | None -> None
  in
  match Cohttp.Response.status resp with
  | `OK -> out { content_length; content_type; content_digest; body }
  | `Temporary_redirect -> (
      match Cohttp.Header.get (Cohttp.Response.headers resp) "location" with
      | Some new_url ->
          let new_uri = Uri.of_string new_url in
          get client ~sw ?token ~extra_headers new_uri out
      | None -> Fmt.failwith "Redirect without location!")
  | err ->
      Fmt.failwith "@[<v2>%a error: %s@,%s@]" Uri.pp uri
        (Cohttp.Code.string_of_status err)
        (Eio.Flow.read_all body)

let get_content_length = function
  | None -> failwith "missing content-length headers"
  | Some s -> s

let get_content_digest = function
  | None -> failwith "missing content-digest headers"
  | Some s -> s

let get_manifest client ~progress ~sw ~token image =
  let name = Image.full_name image in
  let reference = Image.reference image in
  let uri = uri "%s/v2/%s/manifests/%s" registry_base name reference in
  let extra_headers =
    [
      ("Accept", "application/vnd.docker.distribution.manifest.v2+json");
      ("Accept", "application/vnd.docker.distribution.manifest.list.v2+json");
      ("Accept", "application/vnd.docker.distribution.manifest.v1+json");
    ]
  in
  let out { content_length; content_type; content_digest; body; _ } =
    let length = get_content_length content_length in
    let digest = get_content_digest content_digest in
    let flow = Flow.source ~progress ~length ~digest body in
    let body = Flow.read_all flow in
    let err e =
      Fmt.failwith "Docker.get_manifest: error %s (Content-Type: %s)\n%s" e
        (Media_type.to_string content_type)
        body
    in
    match Blob.of_string ~media_type:content_type body with
    | Ok b -> (
        match Blob.v b with
        | Docker (Image_manifest m) -> `Docker_manifest m
        | Docker (Image_manifest_list m) -> `Docker_manifest_list m
        | _ -> err "")
    | Error (`Msg e) -> err e
  in
  get client ~token ~sw ~extra_headers uri out

let get_blob client ~progress ~sw ~token ~cache image d =
  let size = Descriptor.size d in
  let digest = Descriptor.digest d in
  let media_type = Descriptor.media_type d in
  let name = Image.full_name image in
  let uri = uri "%s/v2/%s/blobs/%a" registry_base name Digest.pp digest in
  let out { content_length; content_digest; body; _ } =
    let content_length = get_content_length content_length in
    let () =
      if size <> content_length then failwith "invalid length header";
      match content_digest with
      | None -> ()
      | Some d -> if digest <> d then failwith "invalid digest header"
    in
    (if Cache.Blob.exists ~size:content_length cache digest then
       (* FIXME: int64/int conversion *)
       progress (Int64.to_int content_length)
     else
       let flow = Flow.source ~progress ~length:content_length ~digest body in
       Cache.Blob.add ~sw cache digest flow);
    match media_type with
    | Docker Image_config -> (
        let body = Cache.Blob.get_string cache digest in
        match Config.Docker.of_string body with
        | Ok b ->
            (* Fmt.pr "get image config:\n%s\n%!" body; *)
            `Config b
        | Error e ->
            Fmt.failwith "Docker.get_blob: error %s (Content-Type: %s)\n%s" e
              (Media_type.to_string media_type)
              body)
    | Docker Layer_tar_gzip ->
        let fd = Cache.Blob.get_fd cache digest in
        `Layer fd
    | _ ->
        Fmt.epr "invalid media-type: %a\n" Media_type.pp media_type;
        `Invalid (media_type, body)
  in
  get client ~sw ~token uri out

let get_token client ~sw image =
  let name = Image.full_name image in
  let uri =
    uri "%s/token?service=%s&scope=repository:%s:pull" auth_base auth_service
      name
  in
  let out { body; _ } =
    match Auth.of_yojson (Yojson.Safe.from_string (Eio.Flow.read_all body)) with
    | Ok t -> t.token
    | Error e -> Fmt.failwith "@[<v2>%s parsing errors: %s@]" auth_base e
  in
  get client ~sw uri out

let fetch ?platform ~cache ~client ~domain_mgr image =
  let token = Eio.Switch.run @@ fun sw -> get_token client ~sw image in
  let display = Display.init_fetch ?platform image in
  let progress reporter i =
    Progress.Reporter.report reporter (Int64.of_int i)
  in
  let get_blob ~sw d =
    let bar = Display.line_of_descriptor d in
    Display.with_line ~display bar @@ fun r ->
    get_blob client ~progress:(progress r) ~sw ~token ~cache image d
  in
  let get_manifest ~sw image =
    let reference' =
      match (Image.tag image, Image.digest image) with
      | Some t, None -> t
      | _, Some d -> Digest.encoded_hash d
      | None, None -> "latest"
    in
    let bar =
      Display.line ~color:(Display.next_color ()) ~total:100L
        ("manifest:" ^ reference')
    in
    Display.with_line ~display bar @@ fun r ->
    get_manifest client ~progress:(progress r) ~sw ~token image
  in
  let platform =
    match platform with
    | None -> None
    | Some p -> (
        match Platform.of_string p with
        | Ok p -> Some p
        | Error (`Msg e) -> Fmt.failwith "Fetch.fetch: %s" e)
  in

  (* start dowload here *)
  let manifest =
    if Cache.Manifest.exists cache image then Cache.Manifest.resolve cache image
    else Eio.Switch.run @@ fun sw -> get_manifest ~sw image
  in
  let () =
    match manifest with
    | `Docker_manifest_list m ->
        let ds = Manifest_list.manifests m in
        Logs.info (fun l ->
            let platforms = List.filter_map Descriptor.platform ds in
            l "supported platforms: %a" Fmt.Dump.(list Platform.pp) platforms);
        let download ~sw d =
          let digest = Descriptor.digest d in
          let manifest =
            let name = Image.full_name image in
            get_manifest ~sw (Image.v ~digest name)
          in
          match manifest with
          | `Docker_manifest m -> (
              let config = Manifest.Docker.config m in
              let descriptor_platform = Descriptor.platform d in
              match (platform, descriptor_platform) with
              | Some p, Some p' when p <> p' ->
                  (* Fmt.epr "XXX SKIP platform=%a\n%!" Platform.pp p'; *)
                  ()
              | _ ->
                  let layers = Manifest.Docker.layers m in
                  let _config = get_blob ~sw config in
                  let _layers = List.map (get_blob ~sw) layers in
                  (* Fmt.epr "XXX CONFIG=%a\n%!" pp config; *)
                  (* List.iter (fun l -> Fmt.epr "XXX LAYER=%a\n" pp l) layers) *)
                  ())
          | _ -> Fmt.epr "XXX ERROR\n%!"
        in
        Eio.Fiber.List.iter
          (fun d ->
            Eio.Domain_manager.run domain_mgr (fun () ->
                Eio.Switch.run @@ fun sw -> download ~sw d))
          ds
    | `Docker_manifest _ -> Fmt.epr "XXX TODO\n%!"
  in
  Display.finalise display
