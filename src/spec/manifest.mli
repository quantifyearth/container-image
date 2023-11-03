module OCI : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val to_string : t -> string
  val media_type : t -> Media_type.OCI.t
end

module Docker : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val of_string : string -> (t, [ `Msg of string ]) result
  val to_string : t -> string
  val config : t -> Descriptor.t
  val layers : t -> Descriptor.t list
  val media_type : t -> Media_type.Docker.t
  val to_descriptor : t -> Descriptor.t
end

type t =
  [ `Docker_manifest of Docker.t | `Docker_manifest_list of Manifest_list.t ]

val of_string :
  media_type:Media_type.t -> string -> (t, [ `Msg of string ]) result
