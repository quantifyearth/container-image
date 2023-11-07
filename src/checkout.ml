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

let checkout_layer ~sw ~cache layer dir =
  let fd = Cache.Blob.get_fd ~sw cache layer in
  let fd = Tar_eio_gz.of_source fd in
  Fmt.epr "Extracting layer %a:\n%!" Digest.pp layer;
  Tar_eio_gz.fold
    ~filter:(fun _ -> `Header_and_file)
    (fun hdr src () ->
      let path = dir / hdr.file_name in
      mkdir_parent path;
      Eio.Switch.run @@ fun sw ->
      let dst =
        Eio.Path.open_out ~sw ~append:false ~create:(`If_missing hdr.file_mode)
          path
      in
      Eio.Flow.copy src dst;
      Fmt.pr "%s (%s, %a)\n%!" hdr.file_name
        (Tar.Header.Link.to_string hdr.link_indicator)
        (bytes_to_size ~decimals:2)
        hdr.file_size)
    fd ()

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

let checkout_docker_manifests ~sw ~cache ~dir ds =
  let ms =
    List.map
      (fun d ->
        let digest = Descriptor.digest d in
        let str = Cache.Blob.get_string cache digest in
        match Manifest.Docker.of_string str with
        | Ok m -> m
        | Error (`Msg e) -> failwith e)
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

let checkout_docker_manifest_list ~sw ~cache ~dir l =
  checkout_docker_manifests ~sw ~cache ~dir (Manifest_list.manifests l)

let checkout_oci_index ~sw ~cache ~dir i =
  checkout_oci_manifests ~sw ~cache ~dir (Index.manifests i)

let checkout ~cache ~root i =
  let dir = root / Image.to_string i in
  Eio.Switch.run @@ fun sw ->
  match Cache.Manifest.get cache i with
  | `Docker_manifest m -> checkout_docker_manifest ~sw ~cache ~dir m
  | `Docker_manifest_list m -> checkout_docker_manifest_list ~sw ~cache ~dir m
  | `OCI_index i -> checkout_oci_index ~sw ~cache ~dir i
  | `OCI_manifest m -> checkout_oci_manifest ~sw ~cache ~dir m
