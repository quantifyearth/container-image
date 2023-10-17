type t = {
  architecture : string;
  os : string;
  os_version : string option; [@key "os.version"] [@default None]
  os_features : string list; [@key "os.features"] [@default []]
  variant : string option; [@default None]
}
[@@deriving yojson]
