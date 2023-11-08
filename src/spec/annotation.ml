type t =
  | Created
  | Authors
  | Url
  | Documentation
  | Source
  | Version
  | Revision
  | Vendor
  | Licenses
  | Ref_name
  | Title
  | Description
  | Base_image_digest
  | Base_image_name
  | Reference_digest
  | Reference_type
  | Other of string

let to_string = function
  | Created -> "org.opencontainers.image.created"
  | Authors -> "org.opencontainers.image.authors"
  | Url -> "org.opencontainers.image.url"
  | Documentation -> "org.opencontainers.image.documentation"
  | Source -> "org.opencontainers.image.source"
  | Version -> "org.opencontainers.image.version"
  | Revision -> "org.opencontainers.image.revision"
  | Vendor -> "org.opencontainers.image.vendor"
  | Licenses -> "org.opencontainers.image.licenses"
  | Ref_name -> "org.opencontainers.image.ref.name"
  | Title -> "org.opencontainers.image.title"
  | Description -> "org.opencontainers.image.description"
  | Base_image_digest -> "org.opencontainers.image.base.digest"
  | Base_image_name -> "org.opencontainers.image.base.name"
  | Reference_digest -> "vnd.docker.reference.digest"
  | Reference_type -> "vnd.docker.reference.type"
  | Other s -> s

let of_string = function
  | "org.opencontainers.image.created" -> Created
  | "org.opencontainers.image.authors" -> Authors
  | "org.opencontainers.image.url" -> Url
  | "org.opencontainers.image.documentation" -> Documentation
  | "org.opencontainers.image.source" -> Source
  | "org.opencontainers.image.version" -> Version
  | "org.opencontainers.image.revision" -> Revision
  | "org.opencontainers.image.vendor" -> Vendor
  | "org.opencontainers.image.licenses" -> Licenses
  | "org.opencontainers.image.ref.name" -> Ref_name
  | "org.opencontainers.image.title" -> Title
  | "org.opencontainers.image.description" -> Description
  | "org.opencontainers.image.base.digest" -> Base_image_digest
  | "org.opencontainers.image.base.name" -> Base_image_name
  | "vnd.docker.reference.digest" -> Reference_digest
  | "vnd.docker.reference.type" -> Reference_type
  | s -> Other s

let to_yojson t = `String (to_string t)

let of_yojson = function
  | `String s -> Ok (of_string s)
  | _ -> Error "annotation"
