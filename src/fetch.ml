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

  let manifest_of_fd src =
    match Manifest.of_string (Flow.read_all src) with
    | Ok m -> m
    | Error (`Msg e) -> Fmt.failwith "Fetch.manifest_of_string: %s" e

  let get_manifest client ~progress ~token image =
    Eio.Switch.run @@ fun sw ->
    let name = Image.repository image in
    let reference = Image.reference image in
    let uri = uri "%s/v2/%s/manifests/%s" registry_base name reference in
    let out { content_length; content_type; content_digest; body; _ } =
      let length = get_content_length content_length in
      let digest = get_content_digest content_digest in
      let fd = Flow.source ~progress ~length ~digest body in
      let m = manifest_of_fd fd in
      assert (content_type = Manifest.media_type m);
      m
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

  let get_blob client ~sw ~progress ~token image d =
    let size = Descriptor.size d in
    let digest = Descriptor.digest d in
    let name = Image.repository image in
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

  let get_token client ?credentials image =
    Eio.Switch.run @@ fun sw ->
    let name = Image.repository image in
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

let get_blob ?(show = true) ~sw t d =
  let size = Descriptor.size d in
  let digest = Descriptor.digest d in
  let show = show && show_blob d in
  let () =
    Cache.Blob.if_exists t.cache ~size digest
      ~then_:(fun () ->
        Logs.info (fun l ->
            l "Blob %a is already in the cache" Digest.pp digest);
        ())
      ~else_:(fun () ->
        let bar = Display.line_of_descriptor d in
        Display.with_line ~show ~display:t.display bar (fun r ->
            let progress = Display.report_int r in
            Eio.Switch.run @@ fun sw ->
            let fd =
              API.get_blob ~sw t.client ~progress ~token:t.token t.image d
            in
            Cache.Blob.add_fd t.cache digest fd))
  in
  Cache.Blob.get_fd ~sw t.cache digest

let get_root_manifest ?(show = true) t =
  Cache.Manifest.if_exists t.cache t.image
    ~then_:(fun () ->
      Logs.info (fun l ->
          l "Manifest %a is already in the cache" Image.pp t.image))
    ~else_:(fun () ->
      let line = Display.line_of_image t.image in
      Display.with_line ~show ~display:t.display line @@ fun r ->
      let progress = Display.report_int r in
      let m = API.get_manifest t.client ~progress ~token:t.token t.image in
      Cache.Manifest.add t.cache t.image m);
  Cache.Manifest.get t.cache t.image

let get_manifest ?(show = false) t d =
  let digest = Descriptor.digest d in
  let image = Image.with_digest digest t.image in
  let size = Descriptor.size d in
  let line = Display.line_of_descriptor d in
  Display.with_line ~show ~display:t.display line (fun r ->
      Cache.Manifest.if_exists t.cache image
        ~then_:(fun () ->
          Logs.info (fun l ->
              l "Manifest %a is already in the cache" Digest.pp digest);
          Display.report r size)
        ~else_:(fun () ->
          let progress = Display.report_int r in
          let m = API.get_manifest t.client ~progress ~token:t.token image in
          Cache.Manifest.add t.cache image m));
  Cache.Manifest.get t.cache image

let fetch ?(show_progress = true) ?platform ~cache ~client ~domain_mgr:_
    ?username ?password image =
  Eio.Switch.run @@ fun sw ->
  let display = Display.init ?platform ~sw image in
  let credentials =
    match (username, password) with
    | None, _ -> None
    | Some u, Some p -> Some { API.username = u; password = p }
    | Some u, _ ->
        Fmt.invalid_arg
          "missing credentials for user %s. Use `-p' or set IMAGE_TOKEN." u
  in
  let token = API.get_token client ?credentials image in
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
  (* let pool =
       Eio.Executor_pool.create ~sw ~domain_count:4 ~domain_concurrency:25
         domain_mgr
     in
     let get_blob t d =
       Eio.Executor_pool.submit_exn pool (fun () ->
           Eio.Switch.run @@ fun sw -> get_blob ~sw t d)
           in
  *)
  let get_blob t d =
    Eio.Switch.run @@ fun sw -> get_blob ~show:show_progress t ~sw d
  in
  let get_manifest t d = get_manifest ~show:show_progress t d in
  let rec fetch_manifest_descriptor d =
    let platform = Descriptor.platform d in
    let manifest = get_manifest t d in
    fetch_manifest ~platform manifest
  and fetch_layers ~platform config layers =
    match (my_platform, platform) with
    | Some p, Some p' when p <> p' ->
        (* Fmt.epr "XXX SKIP platform=%a\n%!" Platform.pp p'; *)
        ()
    | _ ->
        let _config = get_blob t config in
        let _layers = Eio.Fiber.List.map (get_blob t) layers in
        (* Fmt.epr "XXX CONFIG=%a\n%!" pp config; *)
        (* List.iter (fun l -> Fmt.epr "XXX LAYER=%a\n" pp l) layers) *)
        ()
  and fetch_descriptors ds =
    Logs.info (fun l ->
        let platforms = List.filter_map Descriptor.platform ds in
        l "supported platforms: %a" Fmt.Dump.(list Platform.pp) platforms);
    Eio.Fiber.List.iter fetch_manifest_descriptor ds
  and fetch_manifest ~platform = function
    | `Docker_manifest m ->
        let config = Manifest.Docker.config m in
        let layers = Manifest.Docker.layers m in
        fetch_layers ~platform config layers
    | `Docker_manifest_list m ->
        let ds = Manifest_list.manifests m in
        fetch_descriptors ds
    | `OCI_index i ->
        let ds = Index.manifests i in
        fetch_descriptors ds
    | `OCI_manifest m ->
        let config = Manifest.OCI.config m in
        let layers = Manifest.OCI.layers m in
        fetch_layers ~platform config layers
  in

  let root = get_root_manifest ~show:show_progress t in
  fetch_manifest ~platform root;
  Display.finalise display
