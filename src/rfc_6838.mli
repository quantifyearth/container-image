type t [@@deriving yojson]

val of_string : string -> (t, string) result
val to_string : t -> string
