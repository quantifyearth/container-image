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
  | Custom of string * string
[@@deriving yojson]

val to_string : t -> string
