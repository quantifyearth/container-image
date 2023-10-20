open Common

type config = {
  user : string option; [@default None] [@key "User"]
  exposed_ports : set; [@default []] [@key "ExposedPorts"]
  env : env list; [@default []] [@key "Env"]
  entrypoint : string list; [@default []] [@key "Entrypoint"]
  cmd : string list; [@default []] [@key "Cmd"]
  volumes : set; [@default []] [@key "Volumes"]
  working_dir : string option; [@default None] [@key "WorkingDir"]
  labels : string map; [@default []] [@key "Labels"]
  stop_signal : string option; [@default None] [@key "StopSignal"]
  args_escaped : bool option; [@default None] [@key "ArgsEscaped"]
}
[@@deriving yojson]

type rootfs = { type_ : string; [@key "type"] diff_ids : Digest.t list }
[@@deriving yojson]

type history = {
  created : date_time option; [@default None]
  author : string option; [@default None]
  created_by : string option; [@default None]
  comment : string option; [@default None]
  empty_layer : bool; [@default false]
}
[@@deriving yojson]

type t = {
  created : date_time option; [@default None]
  author : string option; [@default None]
  architecture : string;
  variant : string option; [@default None]
  os : string;
  os_version : string option; [@default None] [@key "os.version"]
  os_features : string list; [@default []] [@key "os.features"]
  config : config option; [@default None]
  rootfs : rootfs;
  history : history list; [@default []]
}
[@@deriving yojson]
