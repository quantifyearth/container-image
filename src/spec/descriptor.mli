type t [@@deriving yojson]

val pp : t Fmt.t
val digest : t -> Digest.t
val size : t -> int64
val empty : t
val media_type : t -> Media_type.t
val platform : t -> Platform.t option
