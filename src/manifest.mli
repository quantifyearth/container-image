module OCI : sig
  type t [@@deriving yojson]

  val media_type : t -> Media_type.OCI.t
end

module Docker : sig
  type t [@@deriving yojson]

  val media_type : t -> Media_type.Docker.t
end
