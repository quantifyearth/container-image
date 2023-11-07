type algorithm = SHA256 | SHA512 | Unregistered of string list
type t [@@deriving yojson]

val v : algorithm -> string -> (t, [ `Msg of string ]) result
val unsafe_v : algorithm -> string -> t
val algorithm : t -> algorithm
val string_of_algorithm : algorithm -> string
val algorithm_of_string : string -> (algorithm, [ `Msg of string ]) result
val sha256 : string -> t
val sha512 : string -> t
val validate : t -> string -> (unit, [ `Msg of string ]) result
val pp : t Fmt.t
val to_string : t -> string
val of_string : string -> (t, [ `Msg of string ]) result
val equal : t -> t -> bool
val chain : algorithm -> t list -> t list
val chain_id : algorithm -> t list -> t
val encoded_hash : t -> string
val digest_string : algorithm -> string -> t
