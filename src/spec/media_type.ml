open Common
module Content_type = Content_type

module OCI = struct
  type t =
    | Empty
    | Descriptor
    | Layout_header
    | Image_index
    | Image_manifest
    | Image_config
    | Layer_tar
    | Layer_tar_gzip
    | Layer_tar_zstd
    | Layer_non_distributable_tar
    | Layer_non_distributable_tar_gzip
    | Layer_non_distributable_tar_zstd
    | Trust
    | Other of Content_type.t

  let of_string = function
    | "application/vnd.oci.descriptor.v1+json" -> Ok Descriptor
    | "application/vnd.oci.layout.header.v1+json" -> Ok Layout_header
    | "application/vnd.oci.image.index.v1+json" -> Ok Image_index
    | "application/vnd.oci.image.manifest.v1+json" -> Ok Image_manifest
    | "application/vnd.oci.image.config.v1+json" -> Ok Image_config
    | "application/vnd.oci.image.layer.v1.tar" -> Ok Layer_tar
    | "application/vnd.oci.image.layer.v1.tar+gzip" -> Ok Layer_tar_gzip
    | "application/vnd.oci.image.layer.v1.tar+zstd" -> Ok Layer_tar_zstd
    | "application/vnd.oci.empty.v1+json" -> Ok Empty
    | "application/vnd.oci.image.layer.nondistributable.v1.tar" ->
        Ok Layer_non_distributable_tar
    | "application/vnd.oci.image.layer.nondistributable.v1.tar+gzip" ->
        Ok Layer_non_distributable_tar_gzip
    | "application/vnd.oci.image.layer.nondistributable.v1.tar+zstd" ->
        Ok Layer_non_distributable_tar_zstd
    | "application/vnd.in-toto+json" -> Ok Trust
    | s -> (
        match Content_type.of_string s with
        | Ok s -> Ok (Other s)
        | Error e -> Error e)

  let to_string = function
    | Descriptor -> "application/vnd.oci.descriptor.v1+json"
    | Layout_header -> "application/vnd.oci.layout.header.v1+json"
    | Image_index -> "application/vnd.oci.image.index.v1+json"
    | Image_manifest -> "application/vnd.oci.image.manifest.v1+json"
    | Image_config -> "application/vnd.oci.image.config.v1+json"
    | Layer_tar -> "application/vnd.oci.image.layer.v1.tar"
    | Layer_tar_gzip -> "application/vnd.oci.image.layer.v1.tar+gzip"
    | Layer_tar_zstd -> "application/vnd.oci.image.layer.v1.tar+zstd"
    | Empty -> "application/vnd.oci.empty.v1+json"
    | Layer_non_distributable_tar ->
        "application/vnd.oci.image.layer.nondistributable.v1.tar"
    | Layer_non_distributable_tar_gzip ->
        "application/vnd.oci.image.layer.nondistributable.v1.tar+gzip"
    | Layer_non_distributable_tar_zstd ->
        "application/vnd.oci.image.layer.nondistributable.v1.tar+zstd"
    | Trust -> "application/vnd.in-toto+json"
    | Other e -> Content_type.to_string e
end

module Docker = struct
  type t =
    | Image_manifest
    | Image_manifest_list
    | Image_config
    | Layer_tar_gzip
    | Layer_non_distributable_tar_gzip
    | Plugin_config

  let of_string = function
    | "application/vnd.docker.distribution.manifest.v2+json" ->
        Some Image_manifest
    | "application/vnd.docker.distribution.manifest.list.v2+json" ->
        Some Image_manifest_list
    | "application/vnd.docker.container.image.v1+json" -> Some Image_config
    | "application/vnd.docker.image.rootfs.diff.tar.gzip" -> Some Layer_tar_gzip
    | "application/vnd.docker.image.rootfs.foreign.diff.tar.gzip" ->
        Some Layer_non_distributable_tar_gzip
    | "application/vnd.docker.plugin.v1+json" -> Some Plugin_config
    | _ -> None

  let to_string = function
    | Image_manifest -> "application/vnd.docker.distribution.manifest.v2+json"
    | Image_manifest_list ->
        "application/vnd.docker.distribution.manifest.list.v2+json"
    | Image_config -> "application/vnd.docker.container.image.v1+json"
    | Layer_tar_gzip -> "application/vnd.docker.image.rootfs.diff.tar.gzip"
    | Layer_non_distributable_tar_gzip ->
        "application/vnd.docker.image.rootfs.foreign.diff.tar.gzip"
    | Plugin_config -> "application/vnd.docker.plugin.v1+json"
end

type t = OCI of OCI.t | Docker of Docker.t

let of_string str =
  match Docker.of_string str with
  | Some t -> Ok (Docker t)
  | None -> (
      match OCI.of_string str with
      | Ok t -> Ok (OCI t)
      | Error e -> Error (`Msg e))

let to_string = function
  | Docker t -> Docker.to_string t
  | OCI t -> OCI.to_string t

let of_yojson = function
  | `String s -> (
      match of_string s with Ok t -> Ok t | Error (`Msg e) -> Error e)
  | _ -> Error "invalid mediaType"

let to_yojson t = `String (to_string t)

let guess str =
  if str = "" then None
  else
    match str.[0] with
    | '{' -> (
        let m =
          let* json = json_of_string str in
          let* m = json / "mediaType" in
          of_yojson m
        in
        match m with Ok r -> Some r | Error _ -> None)
    | _ -> None

let pp = Fmt.of_to_string to_string
