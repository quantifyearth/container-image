open Eio

val fetch :
  ?platform:string ->
  root:Fs.dir_ty Path.t ->
  client:Cohttp_eio.Client.t ->
  domain_mgr:Eio.Domain_manager.ty Eio.Resource.t ->
  string ->
  unit
