type t = { access_token : string } [@@deriving yojson { strict = false }]

let token t = t.access_token
