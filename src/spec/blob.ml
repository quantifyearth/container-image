open Common

module OCI = struct
  type t =
    | Empty
    | Descriptor of Descriptor.t
    | Layout_header of Layout.t
    | Image_index of Index.t
    | Image_manifest of Manifest.OCI.t
    | Image_config of Config.OCI.t
    | Raw of string

  let pp ppf = function
    | Empty -> Fmt.string ppf ""
    | Descriptor d -> pp_json ppf (Descriptor.to_yojson d)
    | Layout_header h -> pp_json ppf (Layout.to_yojson h)
    | Image_index i -> pp_json ppf (Index.to_yojson i)
    | Image_manifest m -> pp_json ppf (Manifest.OCI.to_yojson m)
    | Image_config c -> pp_json ppf (Config.OCI.to_yojson c)
    | Raw _ -> Fmt.string ppf "<raw>"

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
    let+ m = Manifest.OCI.of_yojson json in
    Image_manifest m

  let image_config str =
    let* json = json_of_string str in
    let+ c = Config.OCI.of_yojson json in
    Image_config c

  let trust str = Ok (Raw str) (* TODO *)
  let layer str = Ok (Raw str) (* TODO *)

  let of_string ty str =
    wrap
    @@
    match ty with
    | Media_type.OCI.Empty -> Ok Empty
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
    | Trust -> trust str
    | Other _ -> Ok (Raw str)
end

module Docker = struct
  type t =
    | Image_manifest of Manifest.Docker.t
    | Image_manifest_list of Manifest_list.t
    | Image_config of Config.Docker.t
    | Plugin_config of Yojson.Safe.t
    | Raw of string

  let pp ppf = function
    | Image_manifest m -> pp_json ppf (Manifest.Docker.to_yojson m)
    | Image_manifest_list l -> pp_json ppf (Manifest_list.to_yojson l)
    | Image_config c -> pp_json ppf (Config.Docker.to_yojson c)
    | Plugin_config j -> pp_json ppf j
    | Raw _ -> Fmt.string ppf "<raw>"

  let image_manifest str =
    let* json = json_of_string str in
    let+ m = Manifest.Docker.of_yojson json in
    Image_manifest m

  let image_manifest_list str =
    let* json = json_of_string str in
    let+ m = Manifest_list.of_yojson json in
    Image_manifest_list m

  let image_config str =
    let* json = json_of_string str in
    let+ c = Config.Docker.of_yojson json in
    Image_config c

  let plugin str =
    let+ json = json_of_string str in
    Plugin_config json

  let layer str = Ok (Raw str) (* TODO *)

  let of_string ty str =
    wrap
    @@
    match ty with
    | Media_type.Docker.Image_manifest -> image_manifest str
    | Image_manifest_list -> image_manifest_list str
    | Image_config -> image_config str
    | Plugin_config -> plugin str
    | Layer_tar_gzip -> layer str
    | Layer_non_distributable_tar_gzip -> layer str
end

type v = OCI of OCI.t | Docker of Docker.t
type t = { media_type : Media_type.t; v : v }

let v t = t.v

let pp ppf t =
  match t.v with OCI t -> OCI.pp ppf t | Docker t -> Docker.pp ppf t

let of_string ~media_type str =
  let+ v =
    match media_type with
    | Media_type.OCI m ->
        let+ t = OCI.of_string m str in
        OCI t
    | Docker m ->
        let+ t = Docker.of_string m str in
        Docker t
  in
  { media_type; v }

let media_type t = t.media_type

let err_size e g =
  error_msg "Blob.of_descriptor: invalid size: expected %a, got %d" Int63.pp e g

let of_descriptor d body =
  let digest = Descriptor.digest d in
  let expected_size = Descriptor.size d in
  let got_size = String.length body in
  if Int63.of_int got_size <> expected_size then err_size expected_size got_size
  else
    match Digest.validate digest body with
    | Ok () ->
        let media_type = Descriptor.media_type d in
        of_string ~media_type body
    | Error e -> Error e
