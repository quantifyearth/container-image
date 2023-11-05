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
  let c = ref 0 in
  Tar_eio_gz.fold
    (fun _hdr () ->
      incr c;
      if !c mod 100 = 0 then Printf.printf ".%!"
        (* Format.printf "%s (%s, %a)\n%!" hdr.Tar.Header.file_name
           (Tar.Header.Link.to_string hdr.link_indicator)
           (bytes_to_size ~decimals:2)
           hdr.Tar.Header.file_size; *))
    fd ()

let checkout_manifest ~sw ~cache m =
  let layers = Manifest.Docker.layers m in
  List.iter
    (fun layer ->
      let d = Descriptor.digest layer in
      Fmt.epr "LAYER %a\n%!" Digest.pp d;
      checkout_layer ~sw ~cache d)
    layers

let checkout ~cache i =
  Eio.Switch.run @@ fun sw ->
  match Cache.Manifest.get cache i with
  | `Docker_manifest_list m ->
      let ds = Manifest_list.manifests m in
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
      List.iter (checkout_manifest ~sw ~cache) ms
  | `Docker_manifest m -> checkout_manifest ~sw ~cache m
