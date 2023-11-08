open Optint

module OCI : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val of_string : string -> (t, [ `Msg of string ]) result
  val to_string : t -> string
  val media_type : t -> Media_type.OCI.t
  val config : t -> Descriptor.t
  val layers : t -> Descriptor.t list
end

module Docker : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val of_string : string -> (t, [ `Msg of string ]) result
  val to_string : t -> string
  val config : t -> Descriptor.t
  val layers : t -> Descriptor.t list
  val media_type : t -> Media_type.Docker.t
end

type t =
  [ `Docker_manifest of Docker.t
  | `Docker_manifest_list of Manifest_list.t
  | `OCI_index of Index.t
  | `OCI_manifest of OCI.t ]
[@@deriving yojson]

val pp : t Fmt.t
val to_string : t -> string
val of_string : string -> (t, [ `Msg of string ]) result
val size : t -> Int63.t option
val media_type : t -> Media_type.t
val platform : (Descriptor.t -> Config.t) -> t -> Platform.t option
val manifests : t -> Descriptor.t list
