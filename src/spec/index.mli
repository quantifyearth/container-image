type t [@@deriving yojson]

val pp : t Fmt.t
val to_string : t -> string
val to_descriptor : t -> Descriptor.t
val manifests : t -> Descriptor.t list
