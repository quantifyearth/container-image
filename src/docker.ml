open Container_image_spec

let registry_base = "https://registry-1.docker.io"
let auth_base = "https://auth.docker.io"
let auth_service = "registry.docker.io"
let read_all flow = Eio.Buf_read.(parse_exn take_all) ~max_size:max_int flow
let uri fmt = Fmt.kstr Uri.of_string fmt
let parent (c, p) = (c, Filename.dirname p)

let exists ~sw file =
  try
    let fd = Eio.Path.open_out ~sw ~create:`Never file in
    Eio.Flow.close fd;
    true
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _)
  | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _)
  ->
    false

let rec mkdir_rec ~perm dir =
  try Eio.Path.mkdir ~perm dir with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ()
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
      mkdir_rec ~perm (parent dir);
      Eio.Path.mkdir ~perm dir

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
  let media_type =
    match Cohttp.Header.get (Cohttp.Response.headers resp) "Content-Type" with
    | Some m -> (
        match Media_type.of_string m with
        | Ok m -> m
        | Error (`Msg e) -> Fmt.failwith "invalid content-type: %s - %s" m e)
    | None -> failwith "missing content-type"
  in
  match Cohttp.Response.status resp with
  | `OK -> out media_type body
  | `Temporary_redirect -> (
      match Cohttp.Header.get (Cohttp.Response.headers resp) "location" with
      | Some new_url ->
          let new_uri = Uri.of_string new_url in
          get client ~sw ?token ~extra_headers new_uri out
      | None -> Fmt.failwith "Redirect without location!")
  | err ->
      Fmt.failwith "@[<v2>%a error: %s@,%s@]" Uri.pp uri
        (Cohttp.Code.string_of_status err)
        (read_all body)

type image = { image : string; reference : string }

let get_manifest client ~sw ~token { image; reference } =
  let uri = uri "%s/v2/%s/manifests/%s" registry_base image reference in
  let extra_headers =
    [
      ("Accept", "application/vnd.docker.distribution.manifest.v2+json");
      ("Accept", "application/vnd.docker.distribution.manifest.list.v2+json");
      ("Accept", "application/vnd.docker.distribution.manifest.v1+json");
    ]
  in
  let out media_type body =
    let body = read_all body in
    match Blob.of_string ~media_type body with
    | Ok b -> b
    | Error e ->
        Fmt.failwith "Docker.get_manifest: error %s (Content-Type: %s)\n%s" e
          (Media_type.to_string media_type)
          body
  in
  get client ~token ~sw ~extra_headers uri out

let get_blob client ~sw ~token ~root image d =
  let digest = Descriptor.digest d in
  let uri = uri "%s/v2/%s/blobs/%a" registry_base image Digest.pp digest in
  let out _ body =
    let media_type = Descriptor.media_type d in
    let algo = Digest.string_of_algorithm (Digest.algorithm digest) in
    let hash = Digest.encoded_hash digest in
    let target_file = Eio.Path.(root / "blobs" / algo / hash) in
    if not (exists ~sw target_file) then (
      mkdir_rec ~perm:0o700 (parent target_file);
      let fd = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) target_file in
      Eio.Flow.copy body fd;
      Eio.Flow.close fd);
    let body =
      (* FIXME: this is ineficient - could we mix this the above copy
         to compute the digest by slices *)
      let str = Eio.Path.with_open_in target_file read_all in
      if Int64.of_int (String.length str) <> Descriptor.size d then
        Fmt.failwith "%a: invalid size" Eio.Path.pp target_file;
      (match Digest.validate digest str with
      | Ok () -> ()
      | Error e ->
          Fmt.failwith "%a: invalid digest: %s" Eio.Path.pp target_file e);
      str
    in
    match media_type with
    | Docker Image_config -> (
        match Config.Docker.of_string body with
        | Ok b ->
            Fmt.pr "get image config:\n%s\n%!" body;
            `Config b
        | Error e ->
            Fmt.failwith "Docker.get_blob: error %s (Content-Type: %s)\n%s" e
              (Media_type.to_string media_type)
              body)
    | Docker Layer_tar_gzip -> `Layer target_file
    | _ ->
        Fmt.epr "invalid media-type: %a\n" Media_type.pp media_type;
        `Invalid (media_type, body)
  in
  get client ~sw ~token uri out

let get_token client ~sw { image; _ } =
  let uri =
    uri "%s/token?service=%s&scope=repository:%s:pull" auth_base auth_service
      image
  in
  let out _ body =
    match Auth.of_yojson (Yojson.Safe.from_string (read_all body)) with
    | Ok t -> t.token
    | Error e -> Fmt.failwith "@[<v2>%s parsing errors: %s@]" auth_base e
  in
  get client ~sw uri out

let split_image str =
  match String.split_on_char ':' str with
  | [] -> invalid_arg "empty name"
  | [ h ] -> { image = h; reference = "latest" }
  | h :: t -> { image = h; reference = String.concat ":" t }

let pp ppf = function
  | `Config c -> Fmt.pf ppf "config: %a" Config.Docker.pp c
  | `Layer f -> Fmt.pf ppf "layer: %a" Eio.Path.pp f
  | `Invalid (m, _) -> Fmt.pf ppf "invalid: %a" Media_type.pp m

let image_to_file str = String.map (function '/' -> '-' | c -> c) str

let fetch ~root ~client image =
  let image = split_image image in
  let root = Eio.Path.(root / image_to_file image.image) in
  Eio.Switch.run @@ fun sw ->
  let token = get_token client ~sw image in
  let get_blob d = get_blob client ~sw ~token ~root image.image d in
  let get_manifest reference =
    get_manifest client ~sw ~token { image with reference }
  in
  let manifest = get_manifest image.reference in
  match Blob.v manifest with
  | Docker (Image_manifest_list m) ->
      let ds = Manifest_list.manifests m in
      List.iter
        (fun d ->
          let digest = Digest.to_string (Descriptor.digest d) in
          let blob = get_manifest digest in
          match Blob.v blob with
          | Docker (Image_manifest m) ->
              let config = Manifest.Docker.config m in
              let layers = Manifest.Docker.layers m in
              let config = get_blob config in
              let layers = List.map get_blob layers in
              Fmt.epr "XXX CONFIG=%a\n%!" pp config;
              List.iter (fun l -> Fmt.epr "XXX LAYER=%a\n" pp l) layers
          | _ -> Fmt.epr "XXX ERROR\n%!")
        ds
  | _ -> Fmt.epr "XXX TODO\n%!"
