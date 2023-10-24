open Common

type config = {
  user : string option; [@default None] [@key "User"]
  exposed_ports : set; [@default []] [@key "ExposedPorts"]
  env : env list; [@default []] [@key "Env"]
  entrypoint : string list; [@default []] [@key "Entrypoint"]
  cmd : string list; [@default []] [@key "Cmd"]
  volumes : set; [@default []] [@key "Volumes"]
  working_dir : string option; [@default None] [@key "WorkingDir"]
  labels : Annotation.t map; [@default []] [@key "Labels"]
  stop_signal : string option; [@default None] [@key "StopSignal"]
  args_escaped : bool option; [@default None] [@key "ArgsEscaped"]
  memory : int option; [@default None] [@key "Memory"]
  memory_swap : int option; [@default None] [@key "MemorySwap"]
  cpu_shares : int option; [@default None] [@key "CpuShares"]
  healthcheck : set; [@default []] [@key "HealthCheck"]
}
[@@deriving yojson]

type rootfs = { type_ : string; [@key "type"] diff_ids : Digest.t list }
[@@deriving yojson]

let rootfs_of_yojson json =
  match rootfs_of_yojson json with
  | Error e -> Error e
  | Ok t -> if t.type_ <> "layers" then Error "rootfs.type" else Ok t

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
  os : OS.t;
  os_version : string option; [@default None] [@key "os.version"]
  os_features : string list; [@default []] [@key "os.features"]
  variant : Arch.variant option; [@default None]
  config : config option; [@default None]
  rootfs : rootfs;
  history : history list; [@default []]
}
[@@deriving yojson { strict = false }]
