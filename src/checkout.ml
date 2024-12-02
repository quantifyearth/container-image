open Container_image_spec

(* FIXME: code duplication *)
let ( let* ) x f = match x with Ok x -> f x | Error e -> Error e
let ( let+ ) x f = match x with Ok x -> Ok (f x) | Error e -> Error e
let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB" |]
let ( / ) = Eio.Path.( / )
let mkdirs dir = Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir

let mkdir_parent file =
  match Eio.Path.split file with
  | None -> ()
  | Some (parent, _) -> mkdirs parent

let bytes_to_size ?(decimals = 2) ppf = function
  | 0L -> Format.fprintf ppf "0 byte"
  | n ->
      let n = Int64.to_float n in
      let i = Float.floor (Float.log n /. Float.log 1024.) in
      let r = n /. Float.pow 1024. i in
      Format.fprintf ppf "%.*f %s" decimals r sizes.(int_of_float i)

let ignore_if_already_exists f v =
  try f v with Eio.Exn.Io (Eio.Fs.E (Already_exists _), _) -> ()

(* TODO: Upstream patches to Eio for fchmodat, fchownat, utimensat *)
let checkout_layer ~sw ~cache layer dir =
  let ( let* ) = Tar.( let* ) in
  let src = Cache.Blob.get_fd ~sw cache layer in
  Eio.traceln "Extracting layer %a:\n%!" Digest.pp layer;
  let go ?global:_ (hdr : Tar.Header.t) _ =
    let path = dir / hdr.file_name in
    let* () =
      let file_mode = 0o777 land hdr.file_mode in
      match hdr.link_indicator with
      | Directory ->
          ignore_if_already_exists (Eio.Path.mkdir ~perm:file_mode) path;
          Tar.return (Ok ())
      | Symbolic ->
          ignore_if_already_exists
            (Eio.Path.symlink ~link_to:hdr.link_name)
            path;
          Tar.return (Ok ())
      | Normal ->
          Eio.Path.with_open_out ~append:false ~create:(`If_missing file_mode)
            path
          @@ fun dst ->
          Eio.Flow.copy src dst;
          Tar.return (Ok ())
      | _ -> Tar.return (Ok ())
    in
    (* Updating ownership, permission and times if root user *)
    if Unix.getuid () = 0 then (
      Eio.Path.chown ~follow:true ~uid:(Int64.of_int hdr.user_id)
        ~gid:(Int64.of_int hdr.group_id)
        path;
      if hdr.link_indicator <> Symbolic then
        Eio.Path.chmod path ~follow:false ~perm:hdr.file_mode;
      Eio_unix.run_in_systhread ~label:"utimes" (fun () ->
          let access_time =
            Option.value ~default:0.
            @@ Option.bind hdr.extended (fun e ->
                   Option.map Int64.to_float e.access_time)
          in
          let mod_time = hdr.mod_time |> Int64.to_float in
          if hdr.link_indicator <> Symbolic then
            Unix.utimes (Eio.Path.native_exn path) access_time mod_time));
    Tar.return (Ok ())
  in
  match Tar_eio.run (Tar_gz.in_gzipped (Tar.fold go ())) (File src) with
  | Ok () -> ()
  | Error (`Eof | `Unexpected_end_of_file) ->
      failwith "Unexpected end of file when untarring"
  | Error (`Msg m) -> failwith m
  | Error (`Fatal e) -> Fmt.failwith "%a" Tar.pp_error e
  | Error (`Gz g) -> failwith g

let checkout_layers ~sw ~cache ~dir layers =
  List.iteri
    (fun i layer ->
      let dir = Eio.Path.(dir / string_of_int i) in
      let d = Descriptor.digest layer in
      checkout_layer ~sw ~cache d dir)
    layers

let checkout_docker_manifest ~sw ~cache ~dir m =
  checkout_layers ~sw ~cache ~dir (Manifest.Docker.layers m)

let checkout_oci_manifest ~sw ~cache ~dir m =
  checkout_layers ~sw ~cache ~dir (Manifest.OCI.layers m)

let checkout_docker_manifests ~sw ~cache ~dir img ds =
  let ms =
    List.map
      (fun d ->
        let digest = Descriptor.digest d in
        let img = Image.v ~digest img in
        let manifest = Cache.Manifest.get cache img in
        match manifest with
        | `Docker_manifest mani -> mani
        | _ -> failwith "Exptected single docker manifest")
      ds
  in
  List.iteri
    (fun i m ->
      let dir = dir / string_of_int i in
      checkout_docker_manifest ~sw ~cache ~dir m)
    ms

let checkout_oci_manifests ~sw ~cache ~dir ds =
  let ms =
    List.map
      (fun d ->
        let digest = Descriptor.digest d in
        let str = Cache.Blob.get_string cache digest in
        match Manifest.OCI.of_string str with
        | Ok m -> m
        | Error (`Msg e) -> failwith e)
      ds
  in
  List.iteri
    (fun i m ->
      let dir = dir / string_of_int i in
      checkout_oci_manifest ~sw ~cache ~dir m)
    ms

let checkout_docker_manifest_list ~sw ~cache ~dir img l =
  checkout_docker_manifests ~sw ~cache ~dir img (Manifest_list.manifests l)

let checkout_oci_index ~sw ~cache ~dir i =
  checkout_oci_manifests ~sw ~cache ~dir (Index.manifests i)

let checkout ~cache ~root i =
  let dir = root / Image.to_string i in
  Eio.Switch.run @@ fun sw ->
  match Cache.Manifest.get cache i with
  | `Docker_manifest m -> checkout_docker_manifest ~sw ~cache ~dir m
  | `Docker_manifest_list m ->
      checkout_docker_manifest_list ~sw ~cache ~dir (Image.repository i) m
  | `OCI_index i -> checkout_oci_index ~sw ~cache ~dir i
  | `OCI_manifest m -> checkout_oci_manifest ~sw ~cache ~dir m
