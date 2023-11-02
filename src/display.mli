type t

val next_color : unit -> Terminal.Color.t
val init_fetch : ?platform:string -> Image.t -> t
val finalise : t -> unit

val line :
  color:Terminal.Color.t -> total:int64 -> string -> int64 Progress.Line.t

val line_of_descriptor :
  Container_image_spec.Descriptor.t -> int64 Progress.Line.t

val with_line :
  display:t -> 'a Progress.Line.t -> ('a Progress.Reporter.t -> 'b) -> 'b
