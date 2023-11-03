module OCI : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val to_string : t -> string
  val media_type : t -> Media_type.OCI.t
end

module Docker : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val to_string : t -> string
  val config : t -> Descriptor.t
  val layers : t -> Descriptor.t list
  val media_type : t -> Media_type.Docker.t
  val to_descriptor : t -> Descriptor.t
end
