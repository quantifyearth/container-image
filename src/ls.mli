open Container_image_spec

type t

val list : cache:Cache.t -> t list
val repository : t -> string
val tags : t -> string list
val digest : t -> Digest.t
val platform : t -> Platform.t option
val size : t -> string
