open Container_image_spec
open Astring

(* TODO: remove code duplication *)
let ( let* ) x f = match x with Ok x -> f x | Error e -> Error e
let ( let+ ) x f = match x with Ok x -> Ok (f x) | Error e -> Error e
let error_msg fmt = Fmt.kstr (fun s -> Error (`Msg s)) fmt

type t = {
  org : string option;
  name : string;
  tag : string option;
  digest : Digest.t option;
}

let digest t = t.digest
let tag t = t.tag
let org t = match t.org with None -> "library" | Some o -> o
let name t = t.name
let repository t = org t ^ "/" ^ t.name

let reference t =
  match (t.tag, t.digest) with
  | Some t, None -> t
  | _, Some t -> Digest.to_string t
  | None, None -> assert false

let of_string str =
  let* str, digest =
    match String.cut ~sep:"@" str with
    | None -> Ok (str, None)
    | Some (path, digest) ->
        let+ digest = Digest.of_string digest in
        (path, Some digest)
  in
  let str, tag =
    match String.cut ~sep:":" str with
    | None -> (str, None)
    | Some (p, o) -> (p, Some o)
  in
  let org, name =
    match String.cut ~sep:"/" str with
    | None -> (None, str)
    | Some (p, i) -> (Some p, i)
  in
  if name = "sha265" then error_msg "missing image name"
  else
    let tag =
      match (tag, digest) with None, None -> Some "latest" | _ -> tag
    in
    Ok { org; name; tag; digest }

let v ?digest ?tag n =
  match of_string n with
  | Ok image -> { image with digest; tag }
  | Error (`Msg e) -> Fmt.invalid_arg "Image.v(%s): error %s" n e

let pp ppf t =
  let pp_org ppf = function
    | None | Some "library" -> ()
    | Some s -> Fmt.pf ppf "%s/" s
  in
  let pp_tag ppf = function None -> () | Some s -> Fmt.pf ppf ":%s" s in
  let pp_digest ppf = function
    | None -> ()
    | Some s -> Fmt.pf ppf "@%a" Digest.pp s
  in
  Fmt.pf ppf "%a%s%a%a" pp_org t.org t.name pp_tag t.tag pp_digest t.digest

let to_string = Fmt.to_to_string pp
let with_tag tag t = { t with tag = Some tag }
let with_digest d t = { t with digest = Some d }
