open Astring

type t = {
  type' : string;
  facets : string list;
  suffix : string option;
  parameters : (string * string) list;
}

exception Break of string

let break fmt = Fmt.kstr (fun s -> raise (Break s)) fmt

let check_name_first = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> ()
  | _ -> break "rfc6838: name first"

let check_name_chars = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!' | '#' | '$' | '&' | '-' | '^' | '_' ->
      ()
  | _ -> break "rfc6838: name chars"

let check_name = function
  | "" -> break "rfc6838: empty name"
  | s ->
      check_name_first s.[0];
      if String.length s > 127 then
        break "rfc6838: length of type (%d)" (String.length s);
      String.iter check_name_chars s

let facet_of_string s =
  let facets = String.cuts ~sep:"." s in
  List.iter check_name facets;
  facets

let subtype_of_string s =
  match String.cut ~sep:"+" s with
  | Some (f, suffix) ->
      check_name suffix;
      let fs = facet_of_string f in
      (fs, Some suffix)
  | None ->
      let fs = facet_of_string s in
      (fs, None)

let prefix_of_string s =
  match String.cut ~sep:"/" s with
  | Some (type', s) ->
      check_name type';
      let fs, suffix = subtype_of_string s in
      (type', fs, suffix)
  | None -> break "rfc6838: prefix"

let parameter_of_string s =
  match String.cut ~sep:"=" s with
  | Some (k, v) ->
      check_name k;
      (k, v)
  | None -> break "rfc6838: parameter"

let of_string s =
  match String.cuts ~sep:";" s with
  | [] -> Error "rfc6838: empty"
  | s :: ps -> (
      try
        let parameters = List.map parameter_of_string ps in
        let type', facets, suffix = prefix_of_string s in
        Ok { type'; facets; suffix; parameters }
      with Break e -> Error e)

let to_string t =
  let p =
    t.type'
    ^ "/"
    ^ String.concat ~sep:"." t.facets
    ^ match t.suffix with None -> "" | Some s -> "+" ^ s
  in
  let ps = List.map (fun (k, v) -> k ^ "=" ^ v) t.parameters in
  String.concat ~sep:"; " (p :: ps)

let to_yojson t = `String (to_string t)
let of_yojson = function `String s -> of_string s | _ -> Error "rfc6838"
