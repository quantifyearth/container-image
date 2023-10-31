module OCI : sig
  type t =
    | Empty
    | Descriptor of Descriptor.t
    | Layout_header of Layout.t
    | Image_index of Index.t
    | Image_manifest of Manifest.OCI.t
    | Image_config of Config.OCI.t
    | Raw of string
end

module Docker : sig
  type t =
    | Image_manifest of Manifest.Docker.t
    | Image_manifest_list of Manifest_list.t
    | Image_config of Config.Docker.t
    | Plugin_config of Yojson.Safe.t
    | Raw of string
end

type v = OCI of OCI.t | Docker of Docker.t
type t

val pp : t Fmt.t

val of_string :
  media_type:Media_type.t -> string -> (t, [ `Msg of string ]) result

val media_type : t -> Media_type.t
val v : t -> v
val of_descriptor : Descriptor.t -> string -> (t, [ `Msg of string ]) result
