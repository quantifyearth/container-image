type t

val pp : t Fmt.t
val of_string : media_type:Media_type.t -> string -> (t, string) result
val media_type : t -> Media_type.t
