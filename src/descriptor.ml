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

module Base64 = struct
  type t = string

  exception Break of string

  let break fmt = Fmt.kstr (fun s -> raise (Break s)) fmt

  let assert_b64 = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/' | '=' -> ()
    | c -> break "data: %c" c

  let of_yojson = function
    | `String s -> (
        try
          String.iter assert_b64 s;
          Ok s
        with Break e -> Error e)
    | _ -> Error "urls"

  let to_yojson u = `String u
end

type t = {
  media_type : Media_type.t; [@key "mediaType"]
  digest : Digest.t; [@key "digest"]
  size : z; [@key "size"]
  urls : Uri.t list; [@key "urls"] [@default []]
  annotations : Annotation.t map; [@default []]
  data : Base64.t; [@key "data"] [@default ""]
  platform : Platform.t option; [@default None]
  artifact_type : Rfc_6838.t option; [@key "artifactType"] [@default None]
}
[@@deriving yojson]

let media_type t = t.media_type

let empty =
  {
    media_type = Empty;
    size = 2L;
    digest =
      Digest.sha256
        "44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a";
    data = "";
    annotations = [];
    urls = [];
    platform = None;
    artifact_type = None;
  }
