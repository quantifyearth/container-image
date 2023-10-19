type t [@@deriving yojson]

val empty : t
val media_type : t -> Media_type.t
