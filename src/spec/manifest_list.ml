open Common

type media_type = Manifest

let media_type_s = Media_type.to_string (Docker Image_manifest_list)
let media_type_of_yojson = const_of_yojson Manifest media_type_s
let media_type_to_yojson Manifest = `String media_type_s

type t = {
  version : v2; [@key "schemaVersion"]
  media_type : media_type; [@key "mediaType"]
  manifests : Descriptor.t list;
}
[@@deriving yojson]

let pp ppf t = pp_json ppf (to_yojson t)
let to_string = Fmt.to_to_string pp
let manifests t = t.manifests
