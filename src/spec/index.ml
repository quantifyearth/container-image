open Common

type media_type = Index

let media_type_s = Media_type.to_string (OCI Image_index)
let media_type_of_yojson = const_of_yojson Index media_type_s
let media_type_to_yojson Index = `String media_type_s

type t = {
  version : v2; [@key "schemaVersion"]
  media_type : media_type; [@key "mediaType"] [@default Index]
  artifact_type : rfc_6838 option; [@key "artifactType"] [@default None]
  manifests : Descriptor.t list;
  platform : Platform.t option; [@default None]
  subject : Descriptor.t option; [@default None]
  annotations : (Annotation.t, string) map; [@key "annotations"] [@default []]
}
[@@deriving yojson]

let pp ppf t = pp_json ppf (to_yojson t)
let to_string = Fmt.to_to_string pp
let manifests t = t.manifests
let platform t = t.platform
