type t [@@deriving yojson]

val manifests : t -> Descriptor.t list
