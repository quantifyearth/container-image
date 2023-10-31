type t [@@deriving yojson]

val pp : t Fmt.t
val of_string : string -> (t, [ `Msg of string ]) result
val arch : t -> Arch.t
val os : t -> OS.t
val check : t -> unit
