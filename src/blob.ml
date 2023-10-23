type t =
  | Empty
  | Descriptor of Descriptor.t
  | Layout_header of Layout.t
  | Image_index of Index.t
  | Image_manifest of Manifest.t
  | Image_config of Config.t
  | Raw of string

let pp_json = Yojson.Safe.pp

let pp ppf = function
  | Empty -> Fmt.string ppf ""
  | Descriptor d -> pp_json ppf (Descriptor.to_yojson d)
  | Layout_header h -> pp_json ppf (Layout.to_yojson h)
  | Image_index i -> pp_json ppf (Index.to_yojson i)
  | Image_manifest m -> pp_json ppf (Manifest.to_yojson m)
  | Image_config c -> pp_json ppf (Config.to_yojson c)
  | Raw s -> Fmt.string ppf s

let json_of_string str =
  match Yojson.Safe.from_string str with
  | json -> Ok json
  | exception Yojson.Json_error str -> Error str

let ( let* ) x f = match x with Ok x -> f x | Error e -> Error e
let ( let+ ) x f = match x with Ok x -> Ok (f x) | Error e -> Error e

let descriptor str =
  let* json = json_of_string str in
  let+ d = Descriptor.of_yojson json in
  Descriptor d

let layout_header str =
  let* json = json_of_string str in
  let+ l = Layout.of_yojson json in
  Layout_header l

let image_index str =
  let* json = json_of_string str in
  let+ i = Index.of_yojson json in
  Image_index i

let image_manifest str =
  let* json = json_of_string str in
  let+ m = Manifest.of_yojson json in
  Image_manifest m

let image_config str =
  let* json = json_of_string str in
  let+ c = Config.of_yojson json in
  Image_config c

let layer str = Ok (Raw str) (* TODO *)

let of_string ty str =
  match ty with
  | Media_type.Empty -> Ok Empty
  | Descriptor -> descriptor str
  | Layout_header -> layout_header str
  | Image_index -> image_index str
  | Image_manifest -> image_manifest str
  | Image_config -> image_config str
  | Layer_tar -> layer str
  | Layer_tar_gzip -> layer str
  | Layer_tar_zstd -> layer str
  | Layer_non_distributable_tar -> layer str
  | Layer_non_distributable_tar_gzip -> layer str
  | Layer_non_distributable_tar_zstd -> layer str
  | Other _ -> Ok (Raw str)
