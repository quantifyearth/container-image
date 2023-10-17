type t
(** Variant type representing different kinds of OCI image annotations. *)

val to_string : t -> string
(** [to_string a] converts an annotation variant to its corresponding string. *)

val of_string : string -> t option
(** [of_string s] tries to convert a string [s] to its corresponding annotation
    variant. Returns [None] if the string does not match any known annotation. *)
