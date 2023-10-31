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

module Base64 : sig
  type t [@@deriving yojson]

  val of_raw : string -> t
  val to_string : t -> (string, [ `Msg of string ]) result
end = struct
  type t = string

  exception Break of string

  let of_raw x = x
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
  let to_string u = Base64.decode u
end

type t = {
  media_type : Media_type.t; [@key "mediaType"]
  digest : Digest.t; [@key "digest"]
  size : z; [@key "size"]
  urls : Uri.t list; [@key "urls"] [@default []]
  annotations : Annotation.t map; [@default []]
  data : Base64.t option; [@key "data"] [@default None]
  platform : Platform.t option; [@default None]
  artifact_type : Content_type.t option; [@key "artifactType"] [@default None]
}
[@@deriving yojson]

let pp ppf t = pp_json ppf (to_yojson t)
let media_type t = t.media_type
let platform t = t.platform

let empty =
  {
    media_type = OCI Empty;
    size = 2L;
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

let check t =
  match t.data with
  | None -> Ok ()
  | Some data -> (
      match Base64.to_string data with
      | Error e -> Error e
      | Ok data ->
          if t.size = Int64.of_int (String.length data) then
            Digest.validate t.digest data
          else
            error_msg "Descriptor.check: invalid size: expected %Ld, got %d"
              t.size (String.length data))

let of_yojson json =
  let result = of_yojson json in
  match result with
  | Error _ -> result
  | Ok t -> ( match unwrap (check t) with Ok () -> result | Error _ as e -> e)
