open Container_image_spec

type t = ((unit -> unit) -> unit, unit) Progress.Display.t

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

let next_color =
  let count = ref (-1) in
  fun () ->
    count := succ !count mod Array.length colors;
    colors.(!count)

let line_of_descriptor d =
  let total = Descriptor.size d in
  let color = next_color () in
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

let init_fetch ?platform image : t =
  let image_name =
    Progress.Line.(
      spacer 4
      ++ constf "🐫 Fetching %a" Fmt.(styled `Bold Image.pp) image
      ++
      match platform with
      | None -> const ""
      | Some p -> constf "%a" Fmt.(styled `Faint (brackets string)) p)
  in
  Progress.Display.start Progress.Multi.(line image_name)

let finalise = Progress.Display.finalise

let with_line ~display bar f =
  let reporter = Progress.Display.add_line display bar in
  let r = f reporter in
  Progress.Reporter.finalise reporter;
  r
