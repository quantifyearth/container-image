open Astring

type algorithm = SHA256 | SHA512 | Unregistered of string list
type t = { algorithm : algorithm; encoded : string }

exception Break of string

let break fmt = Fmt.kstr (fun s -> raise (Break s)) fmt

let algorithm_of_string = function
  | "" -> Error "digest"
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
      with Break e -> Error e)

let algorithm_to_string = function
  | SHA256 -> "sha256"
  | SHA512 -> "sha512"
  | Unregistered s -> String.concat ~sep:"+" s

let assert_hexa = function
  | 'a' .. 'f' | '0' .. '9' -> ()
  | _ -> break "encoded"

let assert_encoded = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '=' | '_' | '-' -> ()
  | _ -> break "encoded"

let encoded_of_string algo e =
  try
    let () =
      match algo with
      | SHA256 ->
          if String.length e <> 64 then break "encoded";
          String.iter assert_hexa e
      | SHA512 ->
          if String.length e <> 128 then break "encoded";
          String.iter assert_hexa e
      | Unregistered _ -> String.iter assert_encoded e
    in
    Ok e
  with Break e -> Error e

let of_string str =
  match String.cut ~sep:":" str with
  | None -> Error "digest"
  | Some (a, e) -> (
      match algorithm_of_string a with
      | Ok a -> (
          match encoded_of_string a e with
          | Ok e -> Ok { algorithm = a; encoded = e }
          | Error e -> Error e)
      | Error e -> Error e)

let to_string t = algorithm_to_string t.algorithm ^ ":" ^ t.encoded
let of_yojson = function `String s -> of_string s | _ -> Error "digest"
let to_yojson s = `String (to_string s)

let sha256 s =
  match encoded_of_string SHA256 s with
  | Ok e -> { algorithm = SHA256; encoded = e }
  | Error e -> invalid_arg e

let sha512 s =
  match encoded_of_string SHA512 s with
  | Ok e -> { algorithm = SHA512; encoded = e }
  | Error e -> invalid_arg e

let validation_error a to_hex ~got ~expected =
  let a = algorithm_to_string a in
  Fmt.kstr
    (fun e -> Error e)
    "digest: validation error, got %s:%s, expected %s:%s" a (to_hex got) a
    (to_hex expected)

let unregistered_error ds =
  Fmt.kstr
    (fun e -> Error e)
    "digest: unregistered algorithms %a"
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
