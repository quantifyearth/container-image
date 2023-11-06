open Container_image_spec
open Optint

module API = struct
  let registry_base = "https://registry-1.docker.io"
  let auth_base = "https://auth.docker.io"
  let uri fmt = Fmt.kstr Uri.of_string fmt

  type response = {
    content_type : Media_type.t;
    content_length : Int63.t option;
    content_digest : Digest.t option;
    body : Eio.Flow.source_ty Eio.Resource.t;
  }

  type verb = GET | POST

  let pp_verb ppf t =
    Fmt.string ppf (match t with GET -> "GET" | POST -> "POST")

  let meth = function GET -> `GET | POST -> `POST

  (* TODO: manage [Range] headers *)
  let rec call verb client ~sw ?(accept = []) ?token ?body uri out =
    Logs.debug (fun l -> l "%a %a\n%!" pp_verb verb Uri.pp uri);
    let headers =
      Cohttp.Header.of_list
      @@ (match token with
         | Some token -> [ ("Authorization", "Bearer " ^ token) ]
         | None -> [])
      @ List.map (fun m -> ("Accept", Media_type.to_string m)) accept
    in
    let resp, body =
      Cohttp_eio.Client.call ~headers ?body client ~sw (meth verb) uri
    in
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
      | Some l -> Some (Int63.of_string l)
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
            call verb client ~sw ?token ~accept new_uri out
        | None -> Fmt.failwith "Redirect without location!")
    | err ->
        Fmt.failwith "@[<v2>%a error: %s@,%s@]" Uri.pp uri
          (Cohttp.Code.string_of_status err)
          (Eio.Flow.read_all body)

  let get client ~sw ?accept ?token uri out =
    call GET client ~sw ?accept ?token uri out

  let post client ~sw ?accept ?token ?body uri out =
    call POST client ~sw ?accept ?token ?body uri out

  let get_content_length = function
    | None -> failwith "missing content-length headers"
    | Some s -> s

  let get_content_digest = function
    | None -> failwith "missing content-digest headers"
    | Some s -> s

  let manifest_of_fd ~media_type fd =
    let str = Flow.read_all fd in
    match Manifest.of_string ~media_type str with
    | Ok m -> m
    | Error (`Msg e) -> Fmt.failwith "Fetch.manifest_of_string: %s" e

  let get_manifest client ~progress ~sw ~token image =
    let name = Image.full_name image in
    let reference = Image.reference image in
    let uri = uri "%s/v2/%s/manifests/%s" registry_base name reference in
    let out { content_length; content_type; content_digest; body; _ } =
      let length = get_content_length content_length in
      let digest = get_content_digest content_digest in
      let fd = Flow.source ~progress ~length ~digest body in
      manifest_of_fd ~media_type:content_type fd
    in
    let accept =
      Media_type.
        [
          Docker Image_manifest;
          Docker Image_manifest_list;
          OCI Image_index;
          OCI Image_manifest;
        ]
    in
    get client ~accept ~token ~sw uri out

  let get_blob client ~progress ~sw ~token image d =
    let size = Descriptor.size d in
    let digest = Descriptor.digest d in
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
      Flow.source ~progress ~length:content_length ~digest body
    in
    get client ~sw ~token uri out

  type credential = { username : string; password : string }

  let get_token client ~sw ?credentials image =
    let name = Image.full_name image in
    let queries =
      [
        ("service", [ "registry.docker.io" ]);
        ("client_id", [ "image" ]);
        ("scope", [ "repository:" ^ name ^ ":pull" ]);
      ]
    in
    let extra_queries =
      match credentials with
      | None -> []
      | Some { username; password } ->
          [
            ("grant_type", [ "password" ]);
            ("username", [ username ]);
            ("password", [ password ]);
          ]
    in
    let queries = Uri.encoded_of_query (queries @ extra_queries) in
    let uri = uri "%s/token?%s" auth_base queries in
    let out { body; _ } =
      let body = Eio.Flow.read_all body in
      match Auth.of_yojson (Yojson.Safe.from_string body) with
      | Ok t -> Auth.token t
      | Error e -> Fmt.failwith "@[<v2>%s parsing errors: %s@]" auth_base e
    in
    match credentials with
    | None -> get client ~sw uri out
    | Some _ -> post client ~sw uri out
end

type t = {
  display : Display.t;
  client : Cohttp_eio.Client.t;
  cache : Cache.t;
  token : string;
  image : Image.t;
}

let show_blob d =
  match Descriptor.media_type d with
  | OCI
      ( Layer_tar | Layer_tar_gzip | Layer_tar_zstd
      | Layer_non_distributable_tar | Layer_non_distributable_tar_gzip
      | Layer_non_distributable_tar_zstd )
  | Docker (Layer_tar_gzip | Layer_non_distributable_tar_gzip) ->
      true
  | _ -> false

let get_blob ~sw t d =
  let size = Descriptor.size d in
  let digest = Descriptor.digest d in
  let aux progress =
    let progress i = progress (Int63.of_int i) in
    let fd = API.get_blob t.client ~progress ~sw ~token:t.token t.image d in
    Cache.Blob.add_fd ~sw t.cache digest fd
  in
  let () =
    Cache.Blob.if_exists t.cache ~size digest
      ~then_:(fun () ->
        Logs.info (fun l ->
            l "Blob %a is already in the cache" Digest.pp digest);
        ())
      ~else_:(fun () ->
        if show_blob d then
          let bar = Display.line_of_descriptor d in
          Display.with_line ~display:t.display bar (fun r ->
              aux (Progress.Reporter.report r))
        else aux ignore)
  in
  Cache.Blob.get_fd ~sw t.cache digest

let get_root_manifest ~sw t =
  let go () =
    let bar =
      let color = Display.next_color () in
      let name =
        match (Image.tag t.image, Image.digest t.image) with
        | Some t, None -> "index:" ^ t
        | None, None -> "index:latest"
        | _, Some d -> "manifest:" ^ Digest.encoded_hash d
      in
      Display.line ~color ~total:(Int63.of_int 100) name
    in
    Display.with_line ~display:t.display bar @@ fun r ->
    let progress i = Progress.Reporter.report r (Int63.of_int i) in
    let m = API.get_manifest t.client ~progress ~sw ~token:t.token t.image in
    Cache.Manifest.add ~sw t.cache t.image m
  in
  Cache.Manifest.if_exists t.cache t.image
    ~then_:(fun () ->
      Logs.info (fun l -> l "Index %a is already in the cache" Image.pp t.image))
    ~else_:go;
  Cache.Manifest.get t.cache t.image

let get_manifest ?(show = false) ~sw t d =
  let digest = Descriptor.digest d in
  let image = Image.v ~digest (Image.full_name t.image) in
  let size = Descriptor.size d in
  let aux progress =
    let () =
      Cache.Manifest.if_exists t.cache image
        ~then_:(fun () ->
          Logs.info (fun l ->
              l "Manifest %a is already in the cache" Digest.pp digest);
          progress size)
        ~else_:(fun () ->
          let progress i = progress (Int63.of_int i) in
          let m =
            API.get_manifest t.client ~progress ~sw ~token:t.token image
          in
          Cache.Manifest.add ~sw t.cache image m)
    in
    Cache.Manifest.get t.cache image
  in
  if show then
    let bar =
      let name = "manifest:" ^ Digest.encoded_hash digest in
      let color = Display.next_color () in
      Display.line ~color ~total:size name
    in
    Display.with_line ~display:t.display bar (fun r ->
        aux (Progress.Reporter.report r))
  else aux ignore

let fetch ?platform ~cache ~client ~domain_mgr:_ ?username ?password image =
  let display = Display.init_fetch ?platform image in
  let credentials =
    match (username, password) with
    | None, None -> None
    | Some u, Some p -> Some { API.username = u; password = p }
    | _ -> invalid_arg "invalid credentials"
  in
  let token =
    Eio.Switch.run @@ fun sw -> API.get_token client ~sw ?credentials image
  in
  let t = { token; display; cache; client; image } in
  let platform =
    match platform with
    | None -> None
    | Some p -> (
        match Platform.of_string p with
        | Ok p -> Some p
        | Error (`Msg e) -> Fmt.failwith "Fetch.fetch: %s" e)
  in
  let my_platform = platform in
  let rec fetch_manifest_descriptor ~sw d =
    let platform = Descriptor.platform d in
    let manifest = get_manifest ~sw t d in
    fetch_manifest ~sw ?platform manifest
  and fetch_manifest ~sw ?platform = function
    | `Docker_manifest m -> (
        let config = Manifest.Docker.config m in
        match (my_platform, platform) with
        | Some p, Some p' when p <> p' ->
            (* Fmt.epr "XXX SKIP platform=%a\n%!" Platform.pp p'; *)
            ()
        | _ ->
            let layers = Manifest.Docker.layers m in
            let _config = get_blob ~sw t config in
            let _layers =
              Eio.Fiber.List.map
                (fun d ->
                  (*Eio.Domain_manager.run domain_mgr @@ fun () -> *)
                  Eio.Switch.run @@ fun sw -> get_blob ~sw t d)
                layers
            in
            (* Fmt.epr "XXX CONFIG=%a\n%!" pp config; *)
            (* List.iter (fun l -> Fmt.epr "XXX LAYER=%a\n" pp l) layers) *)
            ())
    | `Docker_manifest_list m ->
        let ds = Manifest_list.manifests m in
        Logs.info (fun l ->
            let platforms = List.filter_map Descriptor.platform ds in
            l "supported platforms: %a" Fmt.Dump.(list Platform.pp) platforms);
        Eio.Fiber.List.iter (fetch_manifest_descriptor ~sw) ds
    | `OCI_index i ->
        let ds = Index.manifests i in
        Logs.info (fun l ->
            let platforms = List.filter_map Descriptor.platform ds in
            l "supported platforms: %a" Fmt.Dump.(list Platform.pp) platforms);
        Eio.Fiber.List.iter (fetch_manifest_descriptor ~sw) ds
    | `OCI_manifest m -> (
        let config = Manifest.OCI.config m in
        match (my_platform, platform) with
        | Some p, Some p' when p <> p' ->
            (* Fmt.epr "XXX SKIP platform=%a\n%!" Platform.pp p'; *)
            ()
        | _ ->
            let layers = Manifest.OCI.layers m in
            let _config = get_blob ~sw t config in
            let _layers =
              Eio.Fiber.List.map
                (fun d ->
                  (*                  Eio.Domain_manager.run domain_mgr @@ fun () -> *)
                  Eio.Switch.run @@ fun sw -> get_blob ~sw t d)
                layers
            in
            (* Fmt.epr "XXX CONFIG=%a\n%!" pp config; *)
            (* List.iter (fun l -> Fmt.epr "XXX LAYER=%a\n" pp l) layers) *)
            ())
  in

  Eio.Switch.run (fun sw ->
      let root = get_root_manifest t ~sw in
      fetch_manifest ~sw ?platform root);
  Display.finalise display
