module Spec = Container_image_spec

val fetch :
  ?platform:string ->
  root:Eio.Fs.dir_ty Eio.Path.t ->
  client:Cohttp_eio.Client.t ->
  domain_mgr:Eio.Domain_manager.ty Eio.Resource.t ->
  string ->
  unit
