open Common

type z = int64 [@@deriving yojson]

let z_of_yojson n =
  match z_of_yojson n with
  | Error e -> Error e
  | Ok n -> if n < 0L then Error "negative int" else Ok n

type t = {
  media_type : Media_type.t; [@key "mediaType"]
  size : z; [@key "size"]
  digest : digest; [@key "digest"]
  urls : string list; [@key "urls"] [@default []]
  data : string; [@key "data"] [@default ""]
  platform : Platform.t option; [@default None]
  artifact_type : string option; [@key "artifactType"] [@default None]
}
[@@deriving yojson]

let media_type t = t.media_type

let empty =
  {
    media_type = Empty;
    size = 2L;
    digest =
      SHA256 "44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a";
    data = "";
    urls = [];
    platform = None;
    artifact_type = None;
  }
