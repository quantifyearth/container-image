open Container_image_spec

let ( let* ) x f = match x with Ok x -> f x | Error e -> Error e
let ( let+ ) x f = match x with Ok x -> Ok (f x) | Error e -> Error e
let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB" |]

let bytes_to_size ?(decimals = 2) ppf = function
  | 0L -> Format.fprintf ppf "0 byte"
  | n ->
      let n = Int64.to_float n in
      let i = Float.floor (Float.log n /. Float.log 1024.) in
      let r = n /. Float.pow 1024. i in
      Format.fprintf ppf "%.*f %s" decimals r sizes.(int_of_float i)

let checkout_layer ~sw ~cache layer =
  let fd = Cache.Blob.get_fd ~sw cache layer in
  let fd = Tar_eio_gz.of_source fd in
  Tar_eio_gz.fold
    (fun hdr () ->
      Fmt.pr "%s (%s, %a)\n%!" hdr.Tar.Header.file_name
        (Tar.Header.Link.to_string hdr.link_indicator)
        (bytes_to_size ~decimals:2)
        hdr.Tar.Header.file_size)
    fd ()

let checkout_layers ~sw ~cache layers =
  List.iter
    (fun layer ->
      let d = Descriptor.digest layer in
      Fmt.epr "LAYER %a\n%!" Digest.pp d;
      checkout_layer ~sw ~cache d)
    layers

let checkout_docker_manifest ~sw ~cache m =
  checkout_layers ~sw ~cache (Manifest.Docker.layers m)

let checkout_oci_manifest ~sw ~cache m =
  checkout_layers ~sw ~cache (Manifest.OCI.layers m)

let checkout_docker_manifests ~sw ~cache ds =
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
  List.iter (checkout_docker_manifest ~sw ~cache) ms

let checkout_oci_manifests ~sw ~cache ds =
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
  List.iter (checkout_oci_manifest ~sw ~cache) ms

let checkout_docker_manifest_list ~sw ~cache l =
  checkout_docker_manifests ~sw ~cache (Manifest_list.manifests l)

let checkout_oci_index ~sw ~cache i =
  checkout_oci_manifests ~sw ~cache (Index.manifests i)

let checkout ~cache i =
  Eio.Switch.run @@ fun sw ->
  match Cache.Manifest.get cache i with
  | `Docker_manifest m -> checkout_docker_manifest ~sw ~cache m
  | `Docker_manifest_list m -> checkout_docker_manifest_list ~sw ~cache m
  | `OCI_index i -> checkout_oci_index ~sw ~cache i
  | `OCI_manifest m -> checkout_oci_manifest ~sw ~cache m
