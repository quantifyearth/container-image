open Container_image_spec

type t

val v : ?digest:Digest.t -> ?tag:string -> string -> t
(** [v fullname] *)

val pp : t Fmt.t
val of_string : string -> (t, [ `Msg of string ]) result
val reference : t -> string
val full_name : t -> string
val org : t -> string
val name : t -> string
val digest : t -> Digest.t option
val tag : t -> string option
