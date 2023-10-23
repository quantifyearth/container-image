(*
  module type Blob = sig
  type root
  type t

  val find : root -> Digest.algorithm -> string -> (t option, string) result
end

module Unix = struct
  let ( / ) = Filename.concat

  type root = string
  type t = string

  let find root algo encoded =
    match Digest.v algo encoded with
    | Error e -> Error e
    | Ok digest -> (
        let path = root / "blobs" / Digest.string_of_algorithm algo / encoded in
        if not (Sys.file_exists path) then Ok None
        else
          let ic = open_in path in
          let file = really_input_string ic (in_channel_length ic) in
          match Digest.validate digest file with
          | Ok () -> Ok (Some file)
          | Error e -> Error e)
end

let oci_layout = {|
{
    "imageLayoutVersion": "1.0.0"
}
|}
 *)

let file = "oci-layout"
let version = "1.0.0"
let index = "index.json"
let blobs = "blobs"

type t = { version : int [@key "imageLayoutVersion"] } [@@deriving yojson]
