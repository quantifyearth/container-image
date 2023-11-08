type t [@@deriving yojson]

val v :
  ?os_version:string ->
  ?os_features:string list ->
  ?variant:Arch.variant ->
  Arch.t ->
  OS.t ->
  t

val unknown : t
val pp : t Fmt.t
val dump : t Fmt.t
val to_string : t -> string
val of_string : string -> (t, [ `Msg of string ]) result
val arch : t -> Arch.t
val os : t -> OS.t
val check : t -> unit
