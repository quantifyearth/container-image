open Container_image_spec

let checkout_layer ~sw ~cache layer =
  let file = Cache.Blob.get_fd ~sw cache layer in
  let layer = Flow.with_gzip file in
  let files = Tar_eio.Archive.list layer in
  List.iter
    (fun file ->
      let file = file.Tar.Header.file_name in
      Fmt.epr "XXXX FILE=%s\n%!" file)
    files

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
      Eio.Fiber.List.iter (checkout_manifest ~sw ~cache) ms
  | `Docker_manifest m -> checkout_manifest ~sw ~cache m
