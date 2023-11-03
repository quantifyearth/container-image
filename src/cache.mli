open Optint
open Container_image_spec

type t

val v : [ `Dir ] Eio.Path.t -> t
val init : t -> unit

module Blob : sig
  val exists : t -> size:Int63.t -> Digest.t -> bool

  val add :
    sw:Eio.Switch.t -> t -> Digest.t -> Eio.Flow.source_ty Flow.t -> unit

  val get_string : t -> Digest.t -> string
  val get_fd : sw:Eio.Switch.t -> t -> Digest.t -> Eio.File.ro_ty Eio.Resource.t
end

module Manifest : sig
  val exists : t -> Image.t -> bool
  val get : t -> Image.t -> Descriptor.t
  val add : sw:Eio.Switch.t -> t -> Image.t -> Descriptor.t -> unit

  val resolve :
    t ->
    Image.t ->
    [ `Docker_manifest of Manifest.Docker.t
    | `Docker_manifest_list of Manifest_list.t ]
end
