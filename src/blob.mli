type t =
  | Empty
  | Descriptor of Descriptor.t
  | Layout_header of Layout.t
  | Image_index of Index.t
  | Image_manifest of Manifest.t
  | Image_config of Config.t
  | Raw of string

val pp : t Fmt.t
val of_string : Media_type.t -> string -> (t, string) result
