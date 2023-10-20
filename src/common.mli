type date_time = Ptime.t * Ptime.tz_offset_s option [@@deriving yojson]
type 'a map = (string * 'a) list [@@deriving yojson]
type env = string * string [@@deriving yojson]
type set = string list [@@deriving yojson]
type v2 [@@deriving yojson]
type rfc_6838 [@@deriving yojson]
type z = int64 [@@deriving yojson]

(* *)

val const_of_yojson : 'a -> string -> Yojson.Safe.t -> ('a, string) result
