open Common

module Uri = struct
  type t = Uri.t

  let of_yojson = function
    | `String s -> (
        let u = Uri.of_string s in
        match Uri.Absolute_http.of_uri u with
        | Ok _ -> Ok u
        | Error (`Msg e) -> Error e)
    | _ -> Error "urls"

  let to_yojson u = `String (Uri.to_string u)
end

type t = {
  media_type : Media_type.t; [@key "mediaType"]
  digest : Digest.t; [@key "digest"]
  size : z; [@key "size"]
  urls : Uri.t list; [@key "urls"] [@default []]
  annotations : (Annotation.t, string) map; [@default []]
  data : Base64.t option; [@key "data"] [@default None]
  platform : Platform.t option; [@default None]
  artifact_type : Content_type.t option; [@key "artifactType"] [@default None]
}
[@@deriving yojson]

let v ?platform ?data ~media_type ~size digest =
  let data =
    match data with None -> None | Some d -> Some (Base64.encode d)
  in
  {
    media_type;
    size;
    digest;
    urls = [];
    annotations = [];
    data;
    platform;
    artifact_type = None;
  }

let pp ppf t = pp_json ppf (to_yojson t)
let to_string = Fmt.to_to_string pp
let media_type t = t.media_type
let platform t = t.platform

let empty =
  {
    media_type = OCI Empty;
    size = Int63.of_int 2;
    digest =
      Digest.sha256
        "44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a";
    data = Some (Base64.of_raw "e30=");
    annotations = [];
    urls = [];
    platform = None;
    artifact_type = None;
  }

let size t = t.size
let digest t = t.digest

let decoded_data t =
  match t.data with None -> Error (`Msg "no data") | Some d -> Base64.decode d

let check t =
  match t.data with
  | None -> Ok ()
  | Some data -> (
      match Base64.decode data with
      | Error e -> Error e
      | Ok data ->
          if t.size = Int63.of_int (String.length data) then
            Digest.validate t.digest data
          else
            error_msg "Descriptor.check: invalid size: expected %a, got %d"
              Int63.pp t.size (String.length data))

let of_yojson json =
  let result = of_yojson json in
  match result with
  | Error _ -> result
  | Ok t -> ( match unwrap (check t) with Ok () -> result | Error _ as e -> e)

let attestation_manifest d =
  match List.assoc_opt Annotation.Reference_type d.annotations with
  | Some "attestation-manifest" -> true
  | _ -> false
