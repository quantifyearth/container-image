let registry_base = "https://registry-1.docker.io"
let auth_base = "https://auth.docker.io"
let auth_service = "registry.docker.io"
let read_all flow = Eio.Buf_read.(parse_exn take_all) ~max_size:max_int flow
let uri fmt = Fmt.kstr Uri.of_string fmt

let get client ~sw ?token ?(extra_headers = []) uri out =
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
        | Error e -> Fmt.failwith "invalid content-type: %s - %s" m e)
    | None -> failwith "missing content-type"
  in
  match Cohttp.Response.status resp with
  | `OK -> out media_type body
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

let _get_blob client ~sw ~token ~target_file { image; _ } digest =
  let uri = uri "%s/v2/%s/blobs/%s" registry_base image digest in
  let out _ body =
    (* TODO: do something with the media-type *)
    let fd = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) target_file in
    Eio.Flow.copy body fd
  in
  get client ~sw ~token uri out

module Auth = struct
  type t = { token : string } [@@deriving yojson { strict = false }]

  let get_token client ~sw { image; _ } =
    let uri =
      uri "%s/token?service=%s&scope=repository:%s:pull" auth_base auth_service
        image
    in
    let out _ body =
      match of_yojson (Yojson.Safe.from_string (read_all body)) with
      | Ok t -> t.token
      | Error e -> Fmt.failwith "@[<v2>%s parsing errors: %s@]" auth_base e
    in
    get client ~sw uri out
end

let null_auth ?ip:_ ~host:_ _ =
  Ok None (* Warning: use a real authenticator in your code! *)

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let split_image str =
  match String.split_on_char ':' str with
  | [] -> invalid_arg "empty name"
  | [ h ] -> { image = h; reference = "latest" }
  | h :: t -> { image = h; reference = String.concat ":" t }

let _image_to_file str = String.map (function '/' -> '-' | c -> c) str

let get image =
  let image = split_image image in
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client =
    Cohttp_eio.Client.make
      ~https:(Some (https ~authenticator:null_auth))
      (Eio.Stdenv.net env)
  in
  Eio.Switch.run @@ fun sw ->
  let token = Auth.get_token client ~sw image in
  Fmt.epr "XXX TOKEN=%s\n%!" token;
  (* let root = Eio.Stdenv.cwd env in
     let target_file = Eio.Path.(root / (image ^ "." ^ digest ^ ".tar.gz")) in *)
  let manifest = get_manifest client ~sw ~token image in
  match Blob.v manifest with
  | Docker (Image_manifest_list m) ->
      let ds = Manifest_list.manifests m in
      List.iter
        (fun d -> Fmt.epr "XXX MANIFEST=%a\n%!" Digest.pp (Descriptor.digest d))
        ds
  | _ -> Fmt.epr "XXX TODO\n%!"
