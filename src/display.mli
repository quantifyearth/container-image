open Optint

type t
type line
type reporter

val report : reporter -> Int63.t -> unit
val report_int : reporter -> int -> unit
val init : ?platform:string -> sw:Eio.Switch.t -> Image.t -> t
val finalise : t -> unit
val line : color:Terminal.Color.t -> total:Int63.t -> string -> line
val line_of_descriptor : Container_image_spec.Descriptor.t -> int -> line
val line_of_image : Image.t -> int -> line

val with_line :
  display:t -> ?show:bool -> (int -> line) -> (reporter -> 'b) -> 'b
