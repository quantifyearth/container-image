type t [@@deriving yojson { strict = false }]

val token : t -> string

(* TODO: implement JWT spec: https://distribution.github.io/distribution/spec/auth/jwt/ *)
