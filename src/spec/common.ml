open Astring

exception Break of string

let error fmt = Fmt.kstr (fun s -> Error s) fmt
let pp_json ppf t = Fmt.string ppf (Yojson.Safe.to_string t)

type date_time = Ptime.t * Ptime.tz_offset_s option

let date_time_of_yojson : Yojson.Safe.t -> (date_time, string) result = function
  | `String s -> (
      match Ptime.rfc3339_string_error (Ptime.of_rfc3339 s) with
      | Ok (t, tz, _) -> Ok (t, tz)
      | Error e -> Error e)
  | _ -> Error "date_time"

let date_time_to_yojson : date_time -> Yojson.Safe.t =
 fun (t, tz) -> `String (Ptime.to_rfc3339 ?tz_offset_s:tz t)

type 'a map = (string * 'a) list

let map_of_yojson f : Yojson.Safe.t -> ('a map, string) result = function
  | `Null -> Ok []
  | `Assoc a -> (
      try
        let l =
          List.fold_left
            (fun acc (k, v) ->
              match f v with
              | Ok v -> (k, v) :: acc
              | Error e -> raise (Break e))
            [] a
        in
        Ok (List.rev l)
      with Break e -> Error e)
  | j -> error "expecting an object, got %a" pp_json j

let map_to_yojson f : 'a map -> Yojson.Safe.t =
 fun m ->
  let l = List.fold_left (fun acc (k, v) -> (k, f v) :: acc) [] m in
  `Assoc l

type nil = Nil

let nil_to_yojson Nil = `Assoc []
let nil_of_yojson = function `Assoc [] -> Ok Nil | _ -> Error "nil"

type env = string * string

let env_of_yojson = function
  | `String s -> (
      match String.cut ~sep:"=" s with
      | Some (k, v) -> Ok (k, v)
      | None -> Error "env")
  | _ -> Error "env"

let env_to_yojson (k, v) = `String (k ^ "=" ^ v)

type set = string list

let set_to_yojson s =
  map_to_yojson nil_to_yojson (List.rev_map (fun s -> (s, Nil)) s)

let set_of_yojson s =
  match map_of_yojson nil_of_yojson s with
  | Ok l -> Ok (List.rev_map (fun (s, Nil) -> s) l)
  | Error e -> Error e

type v2 = V2

let v2_of_yojson = function
  | `Int 2 -> Ok V2
  | _ -> error "expecting the constant 2"

let v2_to_yojson V2 = `Int 2

let const_of_yojson const n = function
  | `String s when n = s -> Ok const
  | _ -> error "expecting the constant %S" n

(* TODO write a proper parserfor RFC 6838 *)
type rfc_6838 = string [@@deriving yojson]
type z = int64 [@@deriving yojson]

let z_of_yojson n =
  match z_of_yojson n with
  | Error e -> Error e
  | Ok n -> if n < 0L then Error "negative int" else Ok n

let ( let* ) x f = match x with Ok x -> f x | Error e -> Error e
let ( let+ ) x f = match x with Ok x -> Ok (f x) | Error e -> Error e

let ( / ) x y =
  try Ok (Yojson.Safe.Util.member y x)
  with Yojson.Safe.Util.Type_error (e, _) -> Error e

let json_of_string str =
  match Yojson.Safe.from_string str with
  | json -> Ok json
  | exception Yojson.Json_error str -> Error str