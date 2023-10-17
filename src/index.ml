open Common

type media_type = Index

let media_type_of_yojson = function
  | `String "application/vnd.oci.image.index.v1+json" -> Ok Index
  | _ -> Error "mediaType"

let media_type_to_yojson Index =
  `String "application/vnd.oci.image.index.v1+json"

type rfc_6838 = string (* TODO write a proper parser *) [@@deriving yojson]

type t = {
  schema_version : int; [@key "schemaVersion"]
  media_type : media_type; [@key "mediaType"]
  artifact_type : rfc_6838 option; [@key "artifactType"] [@default None]
  manifests : Descriptor.t list;
  platform : Platform.t option; [@default None]
  subject : Descriptor.t option; [@default None]
  annotations : string map; [@key "annotations"] [@default []]
}
[@@deriving yojson]
