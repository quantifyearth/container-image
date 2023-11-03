open Optint

type t

val next_color : unit -> Terminal.Color.t
val init_fetch : ?platform:string -> Image.t -> t
val finalise : t -> unit

val line :
  color:Terminal.Color.t -> total:Int63.t -> string -> Int63.t Progress.Line.t

val line_of_descriptor :
  Container_image_spec.Descriptor.t -> Int63.t Progress.Line.t

val with_line :
  display:t -> 'a Progress.Line.t -> ('a Progress.Reporter.t -> 'b) -> 'b
