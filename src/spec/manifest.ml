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
    annotations : (Annotation.t, string) map; [@default []]
  }
  [@@deriving yojson]

  let pp ppf t = pp_json ppf (to_yojson t)
  let to_string = Fmt.to_to_string pp
  let media_type _ = Media_type.OCI.Image_manifest
  let layers t = t.layers
  let config t = t.config

  let size t =
    List.fold_left
      (fun acc d -> Int63.add acc (Descriptor.size d))
      (Descriptor.size t.config) t.layers

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

  let size t =
    List.fold_left
      (fun acc d -> Int63.add acc (Descriptor.size d))
      (Descriptor.size t.config) t.layers

  let of_string s =
    wrap
    @@ let* json = json_of_string s in
       of_yojson json
end

type t =
  [ `Docker_manifest of Docker.t
  | `Docker_manifest_list of Manifest_list.t
  | `OCI_index of Index.t
  | `OCI_manifest of OCI.t ]

let docker_manifest json =
  let+ m = Docker.of_yojson json in
  `Docker_manifest m

let docker_manifest_list json =
  let+ m = Manifest_list.of_yojson json in
  `Docker_manifest_list m

let oci_index json =
  let+ m = Index.of_yojson json in
  `OCI_index m

let oci_manifest json =
  let+ m = OCI.of_yojson json in
  `OCI_manifest m

let of_yojson json =
  let module U = Yojson.Safe.Util in
  let* media_type = json / "mediaType" in
  let media_type =
    match media_type with
    | `Null ->
        (* of course some OCI image indexes do not fill that field
           as this is an optional field in the spec. *)
        Ok (Media_type.OCI Image_index)
    | s -> Media_type.of_yojson s
  in
  match media_type with
  | Ok (Docker Image_manifest) -> docker_manifest json
  | Ok (Docker Image_manifest_list) -> docker_manifest_list json
  | Ok (OCI Image_index) -> oci_index json
  | Ok (OCI Image_manifest) -> oci_manifest json
  | Ok m -> error "Manifest.of_yojson: invalid media-type: %a" Media_type.pp m
  | Error e -> error "Manifest.of_yojson: %s" e

let of_string body =
  wrap
  @@
  let* json = json_of_string body in
  of_yojson json

let to_string = function
  | `Docker_manifest m -> Docker.to_string m
  | `Docker_manifest_list l -> Manifest_list.to_string l
  | `OCI_index i -> Index.to_string i
  | `OCI_manifest m -> OCI.to_string m

let to_yojson = function
  | `Docker_manifest m -> Docker.to_yojson m
  | `Docker_manifest_list l -> Manifest_list.to_yojson l
  | `OCI_index i -> Index.to_yojson i
  | `OCI_manifest m -> OCI.to_yojson m

let pp = Fmt.of_to_string to_string

let size = function
  | `Docker_manifest m -> Some (Docker.size m)
  | `Docker_manifest_list _ -> None
  | `OCI_index _ -> None
  | `OCI_manifest m -> Some (OCI.size m)

let media_type = function
  | `Docker_manifest _ -> Media_type.Docker Image_manifest
  | `Docker_manifest_list _ -> Media_type.Docker Image_manifest_list
  | `OCI_index _ -> Media_type.OCI Image_index
  | `OCI_manifest _ -> Media_type.OCI Image_manifest

let platform read = function
  | `Docker_manifest m ->
      let config = Docker.config m in
      let c = read config in
      Some (Config.platform c)
  | `Docker_manifest_list _ -> None
  | `OCI_index i -> Index.platform i
  | `OCI_manifest m ->
      let config = OCI.config m in
      let c = read config in
      Some (Config.platform c)

let manifests = function
  | `Docker_manifest _ -> []
  | `Docker_manifest_list l -> Manifest_list.manifests l
  | `OCI_index i -> Index.manifests i
  | `OCI_manifest _ -> []
