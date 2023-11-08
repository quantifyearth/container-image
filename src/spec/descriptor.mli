open Common
open Optint

type t [@@deriving yojson]

val v :
  ?platform:Platform.t ->
  ?data:string ->
  media_type:Media_type.t ->
  size:z ->
  Container_image_spec__Digest.t ->
  t

val pp : t Fmt.t
val to_string : t -> string
val digest : t -> Digest.t
val size : t -> Int63.t
val empty : t
val media_type : t -> Media_type.t
val platform : t -> Platform.t option
val decoded_data : t -> (string, [ `Msg of string ]) result
val attestation_manifest : t -> bool
