let file = "oci-layout"
let version = "1.0.0"
let index = "index.json"
let blobs = "blobs"

type t = { version : int [@key "imageLayoutVersion"] } [@@deriving yojson]
