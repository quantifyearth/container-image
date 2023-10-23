type algorithm = SHA256 | SHA512 | Unregistered of string list
type t [@@deriving yojson]

val v : algorithm -> string -> (t, string) result
val algorithm : t -> algorithm
val string_of_algorithm : algorithm -> string
val sha256 : string -> t
val sha512 : string -> t
val validate : t -> string -> (unit, string) result
