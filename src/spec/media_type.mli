module Content_type = Content_type

module OCI : sig
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

  val to_string : t -> string
end

module Docker : sig
  type t =
    | Image_manifest
    | Image_manifest_list
    | Image_config
    | Layer_tar_gzip
    | Layer_non_distributable_tar_gzip
    | Plugin_config

  val to_string : t -> string
end

type t = OCI of OCI.t | Docker of Docker.t [@@deriving yojson]

val pp : t Fmt.t
val to_string : t -> string
val of_string : string -> (t, [ `Msg of string ]) result
val guess : string -> t option
