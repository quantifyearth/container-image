type t [@@deriving yojson]

val pp : t Fmt.t
val to_string : t -> string
val manifests : t -> Descriptor.t list
