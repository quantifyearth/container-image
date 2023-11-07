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
