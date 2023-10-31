open Eio

val fetch :
  ?platform:string ->
  root:Fs.dir_ty Path.t ->
  client:Cohttp_eio.Client.t ->
  string ->
  unit
