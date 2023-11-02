open Container_image_spec

type 'a t

val source :
  progress:(int -> unit) ->
  length:int64 ->
  digest:Digest.t ->
  Eio.Flow.source_ty Eio.Resource.t ->
  Eio.Flow.source_ty t

val copy : Eio.Flow.source_ty t -> [> Eio.Flow.sink_ty ] Eio.Resource.t -> unit
val read_all : Eio.Flow.source_ty t -> string

(** Flow wrappers *)

type ctx

val ctx : length:int64 -> ctx

val with_digest :
  ctx:ctx ->
  Eio.Flow.source_ty Eio.Resource.t ->
  Eio.Flow.source_ty Eio.Resource.t

val with_progress :
  progress:(int -> unit) ->
  Eio.Flow.source_ty Eio.Resource.t ->
  Eio.Flow.source_ty Eio.Resource.t

val with_gzip :
  Eio.Flow.source_ty Eio.Resource.t -> Eio.Flow.source_ty Eio.Resource.t
