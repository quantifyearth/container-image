open Container_image_spec

let registry_base = "https://registry-1.docker.io"
let auth_base = "https://auth.docker.io"
let auth_service = "registry.docker.io"
let progress = Mutex.create ()

let with_lock ~lock fn =
  Mutex.lock lock;
  let finally () = Mutex.unlock lock in
  Fun.protect ~finally fn

module Flow = struct
  module Progress = struct
    type t = {
      flow : Eio.Flow.source_ty Eio.Resource.t;
      progress : int -> unit;
    }

    let read_methods = []

    let single_read (t : t) buf =
      let i = Eio.Flow.single_read t.flow buf in
      with_lock ~lock:progress (fun () -> t.progress i);
      i
  end

  module Digest = struct
    type t = {
      flow : Eio.Flow.source_ty Eio.Resource.t;
      feed : ?off:int -> ?len:int -> Cstruct.t -> unit;
      length : int64;
      read : int64 ref;
    }

    let read_methods = []

    let finalise ctx =
      (* FIXME: make it work for SHA512 too *)
      let hash = Digestif.SHA256.(get !ctx) in
      let hex = Digestif.SHA256.(to_hex hash) in
      Digest.sha256 hex

    let single_read (t : t) buf =
      let i = Eio.Flow.single_read t.flow buf in
      t.read := Int64.add !(t.read) (Int64.of_int i);
      if !(t.read) > t.length then failwith "stream too long";
      t.feed ~off:0 ~len:i buf;
      i
  end

  module Gzip = struct
    type state = Read | Flush of int

    type t = {
      flow : Eio.Flow.source_ty Eio.Resource.t;
      mutable decoder : Gz.Inf.decoder;
      i : De.bigstring;
      o : De.bigstring;
      mutable state : state;
    }

    let read_methods = []

    let rec single_read t buf =
      match t.state with
      | Flush rem ->
          let io_len = De.io_buffer_size in
          let off = io_len - rem in
          let len = min io_len (Cstruct.length buf) in
          Cstruct.blit (Cstruct.of_bigarray ~off t.o) 0 buf 0 len;
          let rem = rem - len in
          if rem = 0 then (
            let decoder = Gz.Inf.flush t.decoder in
            t.decoder <- decoder;
            t.state <- Read)
          else t.state <- Flush rem;
          len
      | Read -> (
          match Gz.Inf.decode t.decoder with
          | `Await decoder ->
              let i = Eio.Flow.single_read t.flow (Cstruct.of_bigarray t.i) in
              let decoder = Gz.Inf.src decoder t.i 0 i in
              t.decoder <- decoder;
              single_read t buf
          | `Flush decoder ->
              t.state <- Flush (De.io_buffer_size - Gz.Inf.dst_rem decoder);
              single_read t buf
          | `Malformed err -> Fmt.failwith "Gzip.single_read: Error %s" err
          | `End decoder ->
              t.state <- Flush (De.io_buffer_size - Gz.Inf.dst_rem decoder);
              single_read t buf)
  end

  let progress = Eio.Flow.Pi.source (module Progress)
  let digest = Eio.Flow.Pi.source (module Digest)
  let gzip = Eio.Flow.Pi.source (module Gzip)
end

let with_progress ~progress flow =
  Eio.Resource.T (Flow.Progress.{ flow; progress }, Flow.progress)

let with_digest ~ctx ~read ~length flow =
  (* FIXME: make it work for SHA512 too *)
  let feed ?off ?len buf =
    let arr = Cstruct.to_bigarray buf in
    ctx := Digestif.SHA256.feed_bigstring !ctx ?off ?len arr
  in
  Eio.Resource.T (Flow.Digest.{ flow; feed; length; read }, Flow.digest)

let with_gzip flow =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let decoder = Gz.Inf.decoder `Manual ~o in
  Eio.Resource.T (Flow.Gzip.{ flow; decoder; i; o; state = Read }, Flow.gzip)

let mk_flow ?(gzip = false) ~progress ~length flow =
  let ctx = ref (Digestif.SHA256.init ()) in
  let read = ref 0L in
  let with_gzip flow = if gzip then with_gzip flow else flow in
  let flow =
    flow
    |> with_digest ~read ~ctx ~length
    |> with_progress ~progress
    |> with_gzip
  in
  let finalise () =
    if !read <> length then failwith "invalid length";
    Flow.Digest.finalise ctx
  in
  (finalise, flow)

let read_all_raw flow = Eio.Buf_read.(parse_exn take_all) ~max_size:max_int flow

let read_all ?gzip ~progress ~length ~digest flow =
  let finalise, f = mk_flow ?gzip ~progress ~length flow in
  let str = read_all_raw f in
  let d = finalise () in
  if d <> digest then failwith "invalid digest";
  str

let copy ?gzip ~progress ~length ~digest src dst =
  let finalise, f = mk_flow ?gzip ~progress ~length src in
  Eio.Flow.copy f dst;
  let d = finalise () in
  if d <> digest then failwith "invalid digest";
  ()

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
        (read_all_raw body)

type image = { image : string; reference : string }

let pp_image ppf t = Fmt.pf ppf "%s:%s" t.image t.reference

let get_content_length = function
  | None -> failwith "missing content-length headers"
  | Some s -> s

let get_content_digest = function
  | None -> failwith "missing content-digest headers"
  | Some s -> s

let get_manifest client ~progress ~sw ~token { image; reference } =
  let uri = uri "%s/v2/%s/manifests/%s" registry_base image reference in
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
    let body = read_all ~progress ~length ~digest body in
    match Blob.of_string ~media_type:content_type body with
    | Ok b -> b
    | Error (`Msg e) ->
        Fmt.failwith "Docker.get_manifest: error %s (Content-Type: %s)\n%s" e
          (Media_type.to_string content_type)
          body
  in
  get client ~token ~sw ~extra_headers uri out

let get_blob client ?gzip ~progress ~sw ~token ~root image d =
  let size = Descriptor.size d in
  let digest = Descriptor.digest d in
  let media_type = Descriptor.media_type d in
  let uri = uri "%s/v2/%s/blobs/%a" registry_base image Digest.pp digest in
  let out { content_length; content_digest; body; _ } =
    let content_length = get_content_length content_length in
    let () =
      if size <> content_length then failwith "invalid length header";
      match content_digest with
      | None -> ()
      | Some d -> if digest <> d then failwith "invalid digest header"
    in
    let algo = Digest.string_of_algorithm (Digest.algorithm digest) in
    let hash = Digest.encoded_hash digest in
    let target_file = Eio.Path.(root / "blobs" / algo / hash) in
    if exists ~sw target_file then progress (Int64.to_int content_length)
    else (
      mkdir_rec ~perm:0o700 (parent target_file);
      let fd = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) target_file in
      copy ?gzip ~progress ~length:content_length ~digest body fd;
      Eio.Flow.close fd);
    match media_type with
    | Docker Image_config -> (
        let body = Eio.Path.with_open_in target_file read_all_raw in
        match Config.Docker.of_string body with
        | Ok b ->
            (* Fmt.pr "get image config:\n%s\n%!" body; *)
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
  let out { body; _ } =
    match Auth.of_yojson (Yojson.Safe.from_string (read_all_raw body)) with
    | Ok t -> t.token
    | Error e -> Fmt.failwith "@[<v2>%s parsing errors: %s@]" auth_base e
  in
  get client ~sw uri out

let split_image str =
  match String.split_on_char ':' str with
  | [] -> invalid_arg "empty name"
  | [ h ] -> { image = h; reference = "latest" }
  | h :: t -> { image = h; reference = String.concat ":" t }

let _pp ppf = function
  | `Config c -> Fmt.pf ppf "config: %a" Config.Docker.pp c
  | `Layer f -> Fmt.pf ppf "layer: %a" Eio.Path.pp f
  | `Invalid (m, _) -> Fmt.pf ppf "invalid: %a" Media_type.pp m

let image_to_file str = String.map (function '/' -> '-' | c -> c) str

let bar ~color ~total message =
  let message = String.sub message 0 (min 21 (String.length message)) in
  let open Progress.Line.Using_int64 in
  list
    [
      rpad 22 (const message);
      bytes;
      bytes_per_sec;
      bar ~color ~style:`UTF8 total;
      percentage_of total ++ const " ";
    ]

let colors =
  let a =
    [
      "#1996f3";
      "#06aeed";
      "#10c6e6";
      "#27dade";
      "#3dead5";
      "#52f5cb";
      "#66fcc2";
      "#7dffb6";
      "#92fda9";
      "#a8f79c";
      "#bced8f";
      "#d2de81";
      "#e8cb72";
      "#feb562";
      "#ff9b52";
      "#ff8143";
      "#ff6232";
      "#ff4121";
    ]
  in
  Array.map Progress.Color.hex (Array.of_list (a @ List.rev a))

let color_picker =
  let count = ref (-1) in
  fun () ->
    count := succ !count mod Array.length colors;
    colors.(!count)

let bar_of_descriptor d =
  let total = Descriptor.size d in
  let color = color_picker () in
  let txt =
    let digest = Digest.encoded_hash (Descriptor.digest d) in
    let ty =
      match Descriptor.media_type d with
      | Docker Image_manifest -> "manifest:"
      | Docker Image_config -> "config:"
      | Docker _ -> "layer:"
      | _ -> "?:"
    in
    ty ^ digest
  in
  bar ~color ~total txt

let init_display ?platform image =
  let image_name =
    Progress.Line.(
      spacer 4
      ++ constf "🐫 Fetching %a" Fmt.(styled `Bold pp_image) image
      ++
      match platform with
      | None -> const ""
      | Some p -> constf "%a" Fmt.(styled `Faint (brackets string)) p)
  in
  let make_formatter oc =
    Format.make_formatter (output_substring oc) (fun () -> flush oc)
  in
  let config = Progress.Config.v ~ppf:(make_formatter stdout) () in
  Progress.Display.start ~config Progress.Multi.(line image_name)

let with_line ~display bar f =
  let reporter = Progress.Display.add_line display bar in
  let r = f reporter in
  Progress.Reporter.finalise reporter;
  r

let fetch ?platform ~root ~client ~domain_mgr image =
  let image = split_image image in
  let root = Eio.Path.(root / image_to_file image.image) in
  let token = Eio.Switch.run @@ fun sw -> get_token client ~sw image in
  let display = init_display ?platform image in
  let get_blob ?gzip ~sw d =
    let bar = bar_of_descriptor d in
    with_line ~display bar @@ fun reporter ->
    let progress i = Progress.Reporter.report reporter (Int64.of_int i) in
    get_blob client ?gzip ~progress ~sw ~token ~root image.image d
  in
  let get_manifest ~sw reference =
    let image' =
      match reference with
      | `Tag t -> { image with reference = t }
      | `Digest d -> { image with reference = Digest.to_string d }
    in
    let reference' =
      match reference with `Tag t -> t | `Digest d -> Digest.encoded_hash d
    in
    let bar =
      bar ~color:(color_picker ()) ~total:100L ("manifest:" ^ reference')
    in
    with_line ~display bar @@ fun reporter ->
    let progress i = Progress.Reporter.report reporter (Int64.of_int i) in
    get_manifest client ~progress ~sw ~token image'
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
    Eio.Switch.run @@ fun sw -> get_manifest ~sw (`Tag image.reference)
  in
  let () =
    match Blob.v manifest with
    | Docker (Image_manifest_list m) ->
        let ds = Manifest_list.manifests m in
        Logs.info (fun l ->
            let platforms = List.filter_map Descriptor.platform ds in
            l "supported platforms: %a" Fmt.Dump.(list Platform.pp) platforms);
        let download ~sw d =
          let digest = Descriptor.digest d in
          let blob = get_manifest ~sw (`Digest digest) in
          match Blob.v blob with
          | Docker (Image_manifest m) -> (
              let config = Manifest.Docker.config m in
              let descriptor_platform = Descriptor.platform d in
              match (platform, descriptor_platform) with
              | Some p, Some p' when p <> p' ->
                  (* Fmt.epr "XXX SKIP platform=%a\n%!" Platform.pp p'; *)
                  ()
              | _ ->
                  let layers = Manifest.Docker.layers m in
                  let _config = get_blob ~sw config in
                  let _layers = List.map (get_blob ~gzip:false ~sw) layers in
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
    | _ -> Fmt.epr "XXX TODO\n%!"
  in
  Progress.Display.finalise display
