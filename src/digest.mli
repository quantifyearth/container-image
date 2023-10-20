type t [@@deriving yojson]

val sha256 : string -> t
val sha512 : string -> t
