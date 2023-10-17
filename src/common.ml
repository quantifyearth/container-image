open Astring

type date_time = Ptime.t * Ptime.tz_offset_s option

let date_time_of_yojson : Yojson.Safe.t -> (date_time, string) result = function
  | `String s -> (
      match Ptime.rfc3339_string_error (Ptime.of_rfc3339 s) with
      | Ok (t, tz, _) -> Ok (t, tz)
      | Error e -> Error e)
  | _ -> Error "date_time"

let date_time_to_yojson : date_time -> Yojson.Safe.t =
 fun (t, tz) -> `String (Ptime.to_rfc3339 ?tz_offset_s:tz t)

type digest = SHA256 of string

let digest_of_yojson = function
  | `String s -> (
      match String.cut ~sep:":" s with
      | Some ("sha256", s) -> Ok (SHA256 s)
      | _ -> Error "digest: invalid algorithm")
  | _ -> Error "digest"

let digest_to_yojson (SHA256 s) = `String ("sha256:" ^ s)

type 'a map = (string * 'a) list

exception Break of string

let map_of_yojson f : Yojson.Safe.t -> ('a map, string) result = function
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
  | _ -> Error "invalid JSON object"

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
