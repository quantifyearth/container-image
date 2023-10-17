open Astring

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
  | Custom of string

let of_yojson = function
  | `String "application/vnd.oci.descriptor.v1+json" -> Ok Descriptor
  | `String "application/vnd.oci.layout.header.v1+json" -> Ok Layout_header
  | `String "application/vnd.oci.image.index.v1+json" -> Ok Image_index
  | `String "application/vnd.oci.image.manifest.v1+json" -> Ok Image_manifest
  | `String "application/vnd.oci.image.config.v1+json" -> Ok Image_config
  | `String "application/vnd.oci.image.layer.v1.tar" -> Ok Layer_tar
  | `String "application/vnd.oci.image.layer.v1.tar+gzip" -> Ok Layer_tar_gzip
  | `String "application/vnd.oci.image.layer.v1.tar+zstd" -> Ok Layer_tar_zstd
  | `String "application/vnd.oci.empty.v1+json" -> Ok Empty
  | `String "application/vnd.oci.image.layer.nondistributable.v1.tar" ->
      Ok Layer_non_distributable_tar
  | `String "application/vnd.oci.image.layer.nondistributable.v1.tar+gzip" ->
      Ok Layer_non_distributable_tar_gzip
  | `String "application/vnd.oci.image.layer.nondistributable.v1.tar+zstd" ->
      Ok Layer_non_distributable_tar_zstd
  | `String s -> (
      match String.cut ~sep:"application/" s with
      | Some (_, s) -> Ok (Custom s)
      | _ -> Error "invalid mediaType")
  | _ -> Error "invalid mediaType"

let to_yojson = function
  | Descriptor -> `String "application/vnd.oci.descriptor.v1+json"
  | Layout_header -> `String "application/vnd.oci.layout.header.v1+json"
  | Image_index -> `String "application/vnd.oci.image.index.v1+json"
  | Image_manifest -> `String "application/vnd.oci.image.manifest.v1+json"
  | Image_config -> `String "application/vnd.oci.image.config.v1+json"
  | Layer_tar -> `String "application/vnd.oci.image.layer.v1.tar"
  | Layer_tar_gzip -> `String "application/vnd.oci.image.layer.v1.tar+gzip"
  | Layer_tar_zstd -> `String "application/vnd.oci.image.layer.v1.tar+zstd"
  | Empty -> `String "application/vnd.oci.empty.v1+json"
  | Layer_non_distributable_tar ->
      `String "application/vnd.oci.image.layer.nondistributable.v1.tar"
  | Layer_non_distributable_tar_gzip ->
      `String "application/vnd.oci.image.layer.nondistributable.v1.tar+gzip"
  | Layer_non_distributable_tar_zstd ->
      `String "application/vnd.oci.image.layer.nondistributable.v1.tar+zstd"
  | Custom s -> `String ("application/" ^ s)
