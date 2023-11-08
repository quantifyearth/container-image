(** Variant type representing different kinds of OCI image annotations. *)
type t =
  | Created
  | Authors
  | Url
  | Documentation
  | Source
  | Version
  | Revision
  | Vendor
  | Licenses
  | Ref_name
  | Title
  | Description
  | Base_image_digest
  | Base_image_name
  | Reference_digest
  | Reference_type
  | Other of string
[@@deriving yojson]

val to_string : t -> string
(** [to_string a] converts an annotation variant to its corresponding string. *)

val of_string : string -> t
(** [of_string s] tries to convert a string [s] to its corresponding annotation
    variant. Returns [None] if the string does not match any known annotation. *)
