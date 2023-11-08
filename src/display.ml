module Int63 = Optint.Int63
open Container_image_spec

(* FIXME: the None type is probably not needed with switch cancellation *)
type t = {
  stream : (unit -> unit) option Eio.Stream.t;
  display : ((unit -> unit) -> unit, unit) Progress.Display.t;
}

type line = Int63.t Progress.Line.t

type reporter = {
  stream : (unit -> unit) option Eio.Stream.t;
  reporter : Int63.t Progress.Reporter.t option;
}

let report r i =
  match r.reporter with
  | None -> ()
  | Some reporter ->
      Eio.Stream.add r.stream
        (Some (fun () -> Progress.Reporter.report reporter i))

let report_int r i = report r (Int63.of_int i)

let line ~color ~total message =
  let message = String.sub message 0 (min 21 (String.length message)) in
  let open Progress.Line.Using_int63 in
  list
    [
      rpad 22 (const message);
      bytes;
      bytes_per_sec;
      bar ~color ~style:`UTF8 total;
      percentage_of total ++ const " ";
    ]

let colors =
  let a =
    [
      "#1996f3";
      "#06aeed";
      "#10c6e6";
      "#27dade";
      "#3dead5";
      "#52f5cb";
      "#66fcc2";
      "#7dffb6";
      "#92fda9";
      "#a8f79c";
      "#bced8f";
      "#d2de81";
      "#e8cb72";
      "#feb562";
      "#ff9b52";
      "#ff8143";
      "#ff6232";
      "#ff4121";
    ]
  in
  Array.map Progress.Color.hex (Array.of_list (a @ List.rev a))

let next_color i = colors.(i mod Array.length colors)

let line_of_descriptor d i =
  let total = Descriptor.size d in
  let color = next_color i in
  let txt =
    let digest = Digest.encoded_hash (Descriptor.digest d) in
    let ty =
      match Descriptor.media_type d with
      | Docker Image_manifest_list | OCI Image_index -> "index:"
      | Docker Image_manifest | OCI Image_manifest -> "manifest:"
      | OCI Image_config | Docker Image_config -> "config:"
      | OCI
          ( Layer_tar | Layer_tar_gzip | Layer_tar_zstd
          | Layer_non_distributable_tar | Layer_non_distributable_tar_gzip
          | Layer_non_distributable_tar_zstd )
      | Docker (Layer_tar_gzip | Layer_non_distributable_tar_gzip) ->
          "layer:"
      | Docker Plugin_config -> "plugin:"
      | OCI Trust -> "trust:"
      | _ -> "?:"
    in
    ty ^ digest
  in
  line ~color ~total txt

let line_of_image i n =
  let color = next_color n in
  let image =
    match (Image.tag i, Image.digest i) with
    | None, None -> Image.with_tag "latest" i
    | _ -> i
  in
  let name = Image.to_string image in
  line ~color ~total:(Int63.of_int 100) name

let rec apply_stream ~sw stream =
  Eio.Switch.check sw;
  match Eio.Stream.take stream with
  | Some f ->
      f ();
      apply_stream ~sw stream
  | None -> ()

let init ?platform ~sw image : t =
  let image_name =
    Progress.Line.(
      spacer 4
      ++ constf "🐫 Fetching %a" Fmt.(styled `Bold Image.pp) image
      ++
      match platform with
      | None -> const ""
      | Some p -> constf "%a" Fmt.(styled `Faint (brackets string)) p)
  in
  let stream = Eio.Stream.create max_int in
  let display = Progress.Display.start Progress.Multi.(line image_name) in
  Eio.Fiber.fork ~sw (fun () -> apply_stream ~sw stream);
  { stream; display }

let rec empty_stream stream =
  match Eio.Stream.take_nonblocking stream with
  | None | Some None -> ()
  | Some (Some f) ->
      f ();
      empty_stream stream

let finalise { stream; display } =
  Eio.Stream.add stream None;
  empty_stream stream;
  Progress.Display.finalise display

let lines = ref 0

let with_line ~display ?(show = true) bar f =
  let reporter =
    if show then (
      let r = Progress.Display.add_line display.display (bar !lines) in
      incr lines;
      Some r)
    else None
  in
  let finally () =
    match reporter with
    | None -> ()
    | Some r ->
        Eio.Stream.add display.stream
          (Some (fun () -> Progress.Reporter.finalise r))
  in
  Fun.protect ~finally (fun () -> f { reporter; stream = display.stream })
