open Common

type media_type = Manifest

let media_type_s = Media_type.to_string Image_manifest
let media_type_of_yojson = const_of_yojson Manifest media_type_s
let media_type_to_yojson Manifest = `String media_type_s

type t = {
  version : v2; [@key "schemaVersion"]
  media_type : media_type; [@key "mediaType"]
  artifact_type : string option; [@key "artifactType"]
  config : Descriptor.t;
  layers : Descriptor.t list;
  subject : Descriptor.t option; [@default None]
  annotations : Annotation.t map; [@default []]
}
[@@deriving yojson]

exception Break of string

let break fmt = Fmt.kstr (fun s -> raise (Break s)) fmt

let check t =
  let () =
    match Descriptor.media_type t.config with
    | Empty -> (
        match t.artifact_type with
        | None ->
            break
              "artifactType MUST be set when config.mediaType is set to the \
               empty value."
        | Some _ -> (* TODO: check this is compliant with RFC 6838 *) ())
    | _ -> ()
  in
  let () =
    match t.layers with
    | [] -> break "For portability, layers SHOULD have at least one entry."
    | _ -> ()
  in
  ()

let of_yojson t =
  match of_yojson t with
  | Error _ as e -> e
  | Ok t -> (
      try
        check t;
        Ok t
      with Break e -> Error e)
