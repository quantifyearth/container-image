module Spec = Container_image_spec
module Cache = Cache
module Image = Image
module List = Ls

val fetch :
  ?show_progress:bool ->
  ?platform:string ->
  cache:Cache.t ->
  client:Cohttp_eio.Client.t ->
  domain_mgr:Eio.Domain_manager.ty Eio.Resource.t ->
  ?username:string ->
  ?password:string ->
  Image.t ->
  unit

val list : cache:Cache.t -> List.t list
val checkout : cache:Cache.t -> root:[ `Dir ] Eio.Path.t -> Image.t -> unit
val show : cache:Cache.t -> Image.t -> unit
