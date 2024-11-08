open Common

module OCI = struct
  type config = {
    user : string option; [@default None] [@key "User"]
    exposed_ports : set; [@default []] [@key "ExposedPorts"]
    env : env list; [@default []] [@key "Env"]
    entrypoint : string list; [@default []] [@key "Entrypoint"]
    cmd : string list; [@default []] [@key "Cmd"]
    volumes : set; [@default []] [@key "Volumes"]
    working_dir : string option; [@default None] [@key "WorkingDir"]
    labels : (Annotation.t, string) map; [@default []] [@key "Labels"]
    stop_signal : string option; [@default None] [@key "StopSignal"]
    args_escaped : bool option; [@default None] [@key "ArgsEscaped"]
    memory : int option; [@default None] [@key "Memory"]
    memory_swap : int option; [@default None] [@key "MemorySwap"]
    cpu_shares : int option; [@default None] [@key "CpuShares"]
    healthcheck : set; [@default []] [@key "HealthCheck"]
    on_build : string option; [@default None] [@key "OnBuild"]
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
    architecture : Arch.t;
    os : OS.t;
    os_version : string option; [@default None] [@key "os.version"]
    os_features : string list; [@default []] [@key "os.features"]
    variant : Arch.variant option; [@default None]
    config : config option; [@default None]
    rootfs : rootfs;
    history : history list; [@default []]
  }
  [@@deriving yojson]

  let of_string str =
    wrap
    @@
    let* json = json_of_string str in
    of_yojson json

  let platform { os_version; os_features; variant; architecture; os; _ } =
    Platform.v ?os_version ~os_features ?variant architecture os
end

module Docker = struct
  type config = {
    hostname : string option; [@key "Hostname"] [@default None]
    domain_name : string option; [@key "Domainname"] [@default None]
    user : string option; [@key "User"] [@default None]
    attach_stdin : bool; [@key "AttachStdin"] [@default false]
    attach_stdout : bool; [@key "AttachStdout"] [@default false]
    attach_stderr : bool; [@key "AttachStderr"] [@default false]
    exposed_ports : set; [@key "ExposedPorts"] [@default []]
    tty : bool; [@key "Tty"] [@default false]
    open_stdin : bool; [@key "OpenStdin"] [@default false]
    stdin_once : bool; [@key "StdinOnce"] [@default false]
    env : env list option; [@key "Env"]
    cmd : string list option; [@key "Cmd"] [@default None]
    healthcheck : set; [@key "HealthCheck"] [@default []]
    args_escaped : bool option; [@key "ArgsEscaped"] [@default None]
    image : string option; [@key "Image"] [@default None]
    volumes : set; [@key "Volumes"] [@default []]
    working_dir : string; [@key "WorkingDir"]
    entrypoint : string list option; [@key "Entrypoint"] [@default None]
    network_disabled : bool; [@key "NetworkDisabled"] [@default false]
    mac_address : string option; [@key "MacAddress"] [@default None]
    on_build : string list option; [@key "OnBuild"]
    labels : (Annotation.t, string) map; [@default []] [@key "Labels"]
    stop_signal : string option; [@key "StopSignal"] [@default None]
    stop_timeout : int option; [@key "StopTimeout"] [@default None]
    shell : string list; [@key "Shell"] [@default []]
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
    id : string option; [@default None]
    parent : Digest.t option; [@default None]
    comment : string option; [@default None]
    created : date_time;
    container : string option; [@default None]
    container_config : config option; [@default None]
    docker_version : string option; [@default None]
    author : string option; [@default None]
    config : config option; [@default None]
    architecture : Arch.t;
    variant : Arch.variant option; [@default None]
    os : OS.t;
    os_version : string option; [@default None] [@key "os.version"]
    os_features : string list; [@default []] [@key "os.features"]
    size : int64 option; [@default None] [@key "Size"]
    rootfs : rootfs;
    history : history list; [@default []]
  }
  [@@deriving yojson]

  let pp ppf t = pp_json ppf (to_yojson t)

  let of_string str =
    wrap
    @@
    let* json = json_of_string str in
    of_yojson json

  let platform { os_version; os_features; variant; architecture; os; _ } =
    Platform.v ?os_version ~os_features ?variant architecture os
end

type t = OCI of OCI.t | Docker of Docker.t

let pp ppf = function
  | OCI oci -> pp_json ppf (OCI.to_yojson oci)
  | Docker docker -> pp_json ppf (Docker.to_yojson docker)

let env = function
  | OCI oci -> (
      match Option.map (fun (config : OCI.config) -> config.env) oci.config with
      | None -> []
      | Some v -> v)
  | Docker docker -> (
      match
        Option.bind docker.config (fun (config : Docker.config) -> config.env)
      with
      | None -> []
      | Some v -> v)

let platform = function
  | OCI c -> OCI.platform c
  | Docker d -> Docker.platform d

let of_string ~media_type str =
  match media_type with
  | Media_type.OCI Image_config ->
      let+ c = OCI.of_string str in
      OCI c
  | Docker Image_config ->
      let+ c = Docker.of_string str in
      Docker c
  | ty ->
      error_msg "Config.of_string: %a is not a supported media type."
        Media_type.pp ty
