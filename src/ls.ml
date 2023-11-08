(*
open Container_image_spec
open Optint
type t = {
  repository : string;
  tag : string option;
  digest : Digest.t option;
  platform : Platform.t option;
  size : Int63.t;
}

let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB" |]

let bytes_to_size ?(decimals = 2) n =
  if n = Int63.zero then Fmt.str "0 byte"
  else
    let n = Int63.to_float n in
    let i = Float.floor (Float.log n /. Float.log 1024.) in
    let r = n /. Float.pow 1024. i in
    Fmt.str "%.*f %s" decimals r sizes.(int_of_float i)

let of_image cache i =
  let repository = Image.repository i in
  let tag = Image.tag i in
  let digest = Image.digest i in
  let m = Cache.Manifest.get cache i in
  let platform = Platform.
  Fmt.epr "XXX %a\n%!" Manifest.pp m;
  let size =
    match Manifest.size m with Some s -> bytes_to_size s | None -> "-"
  in
  { repository; tag; digest; platform; size }
  *)

let list ~cache = Cache.Manifest.list cache
