type t [@@deriving yojson { strict = false }]

val token : t -> string
