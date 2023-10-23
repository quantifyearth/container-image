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
  | Other e -> Content_type.to_string e

let of_yojson = function
  | `String s -> of_string s
  | _ -> Error "invalid mediaType"

let to_yojson s = `String (to_string s)
