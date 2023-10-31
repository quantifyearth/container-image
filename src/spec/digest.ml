open Common
open Astring

type algorithm = SHA256 | SHA512 | Unregistered of string list
type t = { algorithm : algorithm; encoded : string }

let algorithm t = t.algorithm

exception Break of string

let break fmt = Fmt.kstr (fun s -> raise (Break s)) fmt

let algorithm_of_string = function
  | "" -> error_msg "Digest.algorithm_of_string: error - empty digest"
  | "sha256" -> Ok SHA256
  | "sha512" -> Ok SHA512
  | s -> (
      let l =
        String.fields
          ~is_sep:(function '+' | '.' | '_' | '-' -> true | _ -> false)
          s
      in
      try
        List.iter
          (fun s ->
            if s = "" then break "algorithm-component";
            String.iter
              (function
                | 'a' .. 'z' | '0' .. '9' -> ()
                | _ -> break "algorithm-component")
              s)
          l;
        Ok (Unregistered l)
      with Break e -> error_msg "Digest.algorithm_of_string: error - %s" e)

let string_of_algorithm = function
  | SHA256 -> "sha256"
  | SHA512 -> "sha512"
  | Unregistered s -> String.concat ~sep:"+" s

let assert_hexa = function
  | 'a' .. 'f' | '0' .. '9' -> ()
  | c -> break "%c is not hexa-encoded" c

let assert_encoded = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '=' | '_' | '-' -> ()
  | c -> break "%c is not encoded properly" c

let encoded_of_string algo e =
  try
    let () =
      match algo with
      | SHA256 ->
          if String.length e <> 64 then break "invalid size";
          String.iter assert_hexa e
      | SHA512 ->
          if String.length e <> 128 then break "invalid size";
          String.iter assert_hexa e
      | Unregistered _ -> String.iter assert_encoded e
    in
    Ok e
  with Break e -> error_msg "Digest.encoded_of_string: invalid format (%S)" e

let v a e =
  match encoded_of_string a e with
  | Ok e -> Ok { algorithm = a; encoded = e }
  | Error e -> Error e

let unsafe_v a e = { algorithm = a; encoded = e }
let encoded_hash d = d.encoded

let of_string str =
  match String.cut ~sep:":" str with
  | None -> error_msg "Digest.of_string: %S does not contain ':'" str
  | Some (a, e) -> (
      match algorithm_of_string a with
      | Ok a -> (
          match encoded_of_string a e with
          | Ok e -> Ok { algorithm = a; encoded = e }
          | Error _ as e -> e)
      | Error _ as e -> e)

let to_string t = string_of_algorithm t.algorithm ^ ":" ^ t.encoded
let pp = Fmt.of_to_string to_string

let of_yojson = function
  | `String s -> unwrap (of_string s)
  | _ -> Error "Digest.t"

let to_yojson s = `String (to_string s)

let equal x y =
  x == y || (x.algorithm = y.algorithm && String.equal x.encoded y.encoded)

let sha256 s =
  match encoded_of_string SHA256 s with
  | Ok e -> { algorithm = SHA256; encoded = e }
  | Error (`Msg e) -> invalid_arg e

let sha512 s =
  match encoded_of_string SHA512 s with
  | Ok e -> { algorithm = SHA512; encoded = e }
  | Error (`Msg e) -> invalid_arg e

let validation_error a to_hex ~got ~expected =
  let a = string_of_algorithm a in
  error_msg "Digest.validate: validation error, got %s:%s, expected %s:%s" a
    (to_hex got) a (to_hex expected)

let unregistered_error ds =
  error_msg "Digest.validate: unregistered algorithms %a"
    Fmt.(Dump.list string)
    ds

let validate t buf =
  match t.algorithm with
  | SHA256 ->
      let expected = Digestif.SHA256.of_hex t.encoded in
      let got = Digestif.SHA256.digest_string buf in
      if Digestif.SHA256.equal got expected then Ok ()
      else validation_error SHA256 Digestif.SHA256.to_hex ~got ~expected
  | SHA512 ->
      let expected = Digestif.SHA512.of_hex t.encoded in
      let got = Digestif.SHA512.digest_string buf in
      if Digestif.SHA512.equal got expected then Ok ()
      else validation_error SHA512 Digestif.SHA512.to_hex ~got ~expected
  | Unregistered ds -> unregistered_error ds

let digest_string algo str =
  let encoded =
    match algo with
    | SHA256 -> Digestif.SHA256.(to_hex (digest_string str))
    | SHA512 -> Digestif.SHA512.(to_hex (digest_string str))
    | _ -> invalid_arg "digest_string"
  in
  unsafe_v algo encoded

let chain algo = function
  | [] -> []
  | h :: t ->
      let _, l =
        List.fold_left
          (fun (h, acc) l ->
            let str = to_string h ^ " " ^ to_string l in
            let h' = digest_string algo str in
            (h', h' :: acc))
          (h, [ h ]) t
      in
      List.rev l

let chain_id algo = function
  | [] -> invalid_arg "chain_id: empty list"
  | h :: t ->
      List.fold_left
        (fun h l ->
          let str = to_string h ^ " " ^ to_string l in
          let h' = digest_string algo str in
          h')
        h t
