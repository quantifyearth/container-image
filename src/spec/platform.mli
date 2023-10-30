type t [@@deriving yojson]

val arch : t -> Arch.t
val os : t -> OS.t
val check : t -> unit
