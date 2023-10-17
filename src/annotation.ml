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

let of_string = function
  | "org.opencontainers.image.created" -> Some Created
  | "org.opencontainers.image.authors" -> Some Authors
  | "org.opencontainers.image.url" -> Some Url
  | "org.opencontainers.image.documentation" -> Some Documentation
  | "org.opencontainers.image.source" -> Some Source
  | "org.opencontainers.image.version" -> Some Version
  | "org.opencontainers.image.revision" -> Some Revision
  | "org.opencontainers.image.vendor" -> Some Vendor
  | "org.opencontainers.image.licenses" -> Some Licenses
  | "org.opencontainers.image.ref.name" -> Some Ref_name
  | "org.opencontainers.image.title" -> Some Title
  | "org.opencontainers.image.description" -> Some Description
  | "org.opencontainers.image.base.digest" -> Some Base_image_digest
  | "org.opencontainers.image.base.name" -> Some Base_image_name
  | _ -> None
