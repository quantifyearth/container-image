module Spec = Container_image_spec
module Cache = Cache
module Image = Image

val fetch :
  ?platform:string ->
  cache:Cache.t ->
  client:Cohttp_eio.Client.t ->
  domain_mgr:Eio.Domain_manager.ty Eio.Resource.t ->
  Image.t ->
  unit
