open Container_image_spec

type t

val v : ?digest:Digest.t -> ?tag:string -> string -> t
(** [v repository] *)

val pp : t Fmt.t
val to_string : t -> string
val of_string : string -> (t, [ `Msg of string ]) result
val reference : t -> string
val org : t -> string
val name : t -> string

val repository : t -> string
(** [repository t] is [org t ^ "/" ^ name t] *)

val digest : t -> Digest.t option
val tag : t -> string option
val with_tag : string -> t -> t
val with_digest : Digest.t -> t -> t
