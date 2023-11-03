open Optint

type t [@@deriving yojson]

val pp : t Fmt.t
val to_string : t -> string
val digest : t -> Digest.t
val size : t -> Int63.t
val empty : t
val media_type : t -> Media_type.t
val platform : t -> Platform.t option
