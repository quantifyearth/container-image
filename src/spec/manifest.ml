open Common

module OCI = struct
  type media_type = Manifest

  let media_type_s = Media_type.to_string (OCI Image_manifest)
  let media_type_of_yojson = const_of_yojson Manifest media_type_s
  let media_type_to_yojson Manifest = `String media_type_s

  type t = {
    version : v2; [@key "schemaVersion"]
    media_type : media_type; [@key "mediaType"]
    artifact_type : string option; [@key "artifactType"] [@default None]
    config : Descriptor.t;
    layers : Descriptor.t list;
    subject : Descriptor.t option; [@default None]
    annotations : Annotation.t map; [@default []]
  }
  [@@deriving yojson]

  let pp ppf t = pp_json ppf (to_yojson t)
  let to_string = Fmt.to_to_string pp
  let media_type _ = Media_type.OCI.Image_manifest
  let layers t = t.layers
  let config t = t.config

  let of_string s =
    wrap
    @@ let* json = json_of_string s in
       of_yojson json

  exception Break of string

  let break fmt = Fmt.kstr (fun s -> raise (Break s)) fmt

  let check t =
    let () =
      match Descriptor.media_type t.config with
      | OCI Empty -> (
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

  let to_descriptor t =
    let str = to_string t in
    let digest = Digest.digest_string SHA256 str in
    let size = Int63.of_int (String.length str) in
    let media_type = Media_type.OCI Image_manifest in
    Descriptor.v ~media_type ~data:str ~size digest
end

module Docker = struct
  type media_type = Manifest

  let media_type_s = Media_type.to_string (Docker Image_manifest)
  let media_type_of_yojson = const_of_yojson Manifest media_type_s
  let media_type_to_yojson Manifest = `String media_type_s

  type t = {
    version : v2; [@key "schemaVersion"]
    media_type : media_type; [@key "mediaType"]
    config : Descriptor.t;
    layers : Descriptor.t list;
  }
  [@@deriving yojson]

  let layers t = t.layers
  let config t = t.config
  let pp ppf t = pp_json ppf (to_yojson t)
  let to_string = Fmt.to_to_string pp
  let media_type _ = Media_type.Docker.Image_manifest

  let of_string s =
    wrap
    @@ let* json = json_of_string s in
       of_yojson json

  let to_descriptor t =
    let str = to_string t in
    let digest = Digest.digest_string SHA256 str in
    let size = Int63.of_int (String.length str) in
    let media_type = Media_type.Docker Image_manifest in
    Descriptor.v ~media_type ~data:str ~size digest
end

type t =
  [ `Docker_manifest of Docker.t
  | `Docker_manifest_list of Manifest_list.t
  | `OCI_index of Index.t
  | `OCI_manifest of OCI.t ]

let docker_manifest str =
  let* json = json_of_string str in
  let+ m = Docker.of_yojson json in
  `Docker_manifest m

let docker_manifest_list str =
  let* json = json_of_string str in
  let+ m = Manifest_list.of_yojson json in
  `Docker_manifest_list m

let oci_index str =
  let* json = json_of_string str in
  let+ m = Index.of_yojson json in
  `OCI_index m

let oci_manifest str =
  let* json = json_of_string str in
  let+ m = OCI.of_yojson json in
  `OCI_manifest m

let of_string ~media_type body =
  let err () =
    Fmt.failwith "Manifest.of_string: invalid media-type: %s"
      (Media_type.to_string media_type)
  in
  match media_type with
  | Docker Image_manifest -> wrap (docker_manifest body)
  | Docker Image_manifest_list -> wrap (docker_manifest_list body)
  | OCI Image_index -> wrap (oci_index body)
  | OCI Image_manifest -> wrap (oci_manifest body)
  | _ -> err ()

let to_string = function
  | `Docker_manifest m -> Docker.to_string m
  | `Docker_manifest_list l -> Manifest_list.to_string l
  | `OCI_index i -> Index.to_string i
  | `OCI_manifest m -> OCI.to_string m

let to_descriptor = function
  | `Docker_manifest m -> Docker.to_descriptor m
  | `Docker_manifest_list l -> Manifest_list.to_descriptor l
  | `OCI_index i -> Index.to_descriptor i
  | `OCI_manifest m -> OCI.to_descriptor m
