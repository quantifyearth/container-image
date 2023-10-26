module OCI : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val media_type : t -> Media_type.OCI.t
end

module Docker : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val config : t -> Descriptor.t
  val layers : t -> Descriptor.t list
  val media_type : t -> Media_type.Docker.t
end
