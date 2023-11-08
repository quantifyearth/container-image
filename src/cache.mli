open Optint
open Container_image_spec

type t

val v : [ `Dir ] Eio.Path.t -> t
val init : t -> unit

module Blob : sig
  val if_exists :
    t ->
    size:Int63.t ->
    ?then_:(unit -> unit) ->
    ?else_:(unit -> unit) ->
    Digest.t ->
    unit

  val add_fd : t -> Digest.t -> Eio.Flow.source_ty Flow.t -> unit
  val get_fd : sw:Eio.Switch.t -> t -> Digest.t -> Eio.File.ro_ty Eio.Resource.t
  val add_string : t -> Digest.t -> string -> unit
  val get_string : t -> Digest.t -> string
end

module Manifest : sig
  val if_exists :
    t -> ?then_:(unit -> unit) -> ?else_:(unit -> unit) -> Image.t -> unit

  val get : t -> Image.t -> Manifest.t
  val add : t -> Image.t -> Manifest.t -> unit
  val list : t -> Image.t list
  val list_tags : t -> Image.t list
  val list_digests : t -> Image.t list
  val guess : t -> string -> Image.t
end
