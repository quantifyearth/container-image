open Optint

type t
type line
type reporter

val report : reporter -> Int63.t -> unit
val report_int : reporter -> int -> unit
val next_color : unit -> Terminal.Color.t
val init : ?platform:string -> sw:Eio.Switch.t -> Image.t -> t
val finalise : t -> unit
val line : color:Terminal.Color.t -> total:Int63.t -> string -> line
val line_of_descriptor : Container_image_spec.Descriptor.t -> line
val line_of_image : Image.t -> line
val with_line : display:t -> ?show:bool -> line -> (reporter -> 'b) -> 'b
