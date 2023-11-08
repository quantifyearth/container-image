open Common
open Astring

type t = {
  architecture : Arch.t;
  os : OS.t;
  os_version : string option; [@key "os.version"] [@default None]
  os_features : string list; [@key "os.features"] [@default []]
  variant : Arch.variant option; [@default None]
  features : string list; [@default []]
}
[@@deriving yojson]

let v ?os_version ?(os_features = []) ?variant architecture os =
  { architecture; os; os_version; os_features; variant; features = [] }

let arch t = t.architecture
let os t = t.os
let unknown = v Unknown Unknown

let of_string str =
  match String.cuts ~sep:"/" str with
  | [ os; arch ] ->
      let* os = OS.of_string os in
      let+ architecture = Arch.of_string arch in
      {
        os;
        architecture;
        os_version = None;
        os_features = [];
        variant = None;
        features = [];
      }
  | [ os; arch; variant ] ->
      let* os = OS.of_string os in
      let* architecture = Arch.of_string arch in
      let+ variant = Arch.variant_of_string variant in
      {
        os;
        architecture;
        os_version = None;
        os_features = [];
        variant = Some variant;
        features = [];
      }
  | _ -> error_msg "Platform.of_string: invalid string (%S)" str

let pp ppf t =
  match t.variant with
  | None -> Fmt.pf ppf "%a/%a" OS.pp t.os Arch.pp t.architecture
  | Some v ->
      Fmt.pf ppf "%a/%a/%a" OS.pp t.os Arch.pp t.architecture Arch.pp_variant v

let to_string = Fmt.to_to_string pp
let pp_v = Fmt.option ~none:(Fmt.any "N/A") Arch.pp_variant
let dump ppf t = pp_json ppf (to_yojson t)

let err_arch_variant t =
  Fmt.failwith "%a/%a: invalid architecture/variant pair" Arch.pp t.architecture
    pp_v t.variant

let err_os_arch t =
  Fmt.failwith "%a/%a: invalid os/architecture pair" OS.pp t.os Arch.pp
    t.architecture

let check t =
  let () =
    match (t.os, t.os_features) with
    | Windows, [ "win32k" ] -> ()
    | Windows, l ->
        Fmt.failwith "%a/%a invalid os/os.features pair" OS.pp t.os
          Fmt.(Dump.list string)
          l
    | _ -> ()
  in
  let () =
    match (t.architecture, t.variant) with
    | Arm, Some V6 | Arm, Some V7 | Arm, Some V8 | Arm64, Some V8 -> ()
    | Arm, _ | Arm64, _ | _, Some _ -> err_arch_variant t
    | _ -> ()
  in
  let () =
    match (t.os, t.architecture) with
    | Aix, Ppc64
    | Android, X386
    | Android, Xamd64
    | Android, Arm
    | Android, Arm64
    | Darwin, Xamd64
    | Darwin, Arm64
    | Dragonfly, Xamd64
    | Freebsd, X386
    | Freebsd, Xamd64
    | Freebsd, Arm
    | Illumos, Xamd64
    | Ios, Arm64
    | Js, Wasm
    | Linux, X386
    | Linux, Xamd64
    | Linux, Arm
    | Linux, Arm64
    | Linux, Loong64
    | Linux, Mips
    | Linux, Mipsle
    | Linux, Mips64
    | Linux, Mips64le
    | Linux, Ppc64
    | Linux, Ppc64le
    | Linux, Riscv64
    | Linux, S390x
    | Netbsd, X386
    | Netbsd, Xamd64
    | Netbsd, Arm
    | Openbsd, X386
    | Openbsd, Xamd64
    | Openbsd, Arm
    | Openbsd, Arm64
    | Plan9, X386
    | Plan9, Xamd64
    | Plan9, Arm
    | Solaris, Xamd64
    | Wasip1, Wasm
    | Windows, X386
    | Windows, Xamd64
    | Windows, Arm
    | Windows, Arm64 ->
        ()
    | _, _ -> err_os_arch t
  in
  let () =
    match t.features with
    | [] -> ()
    | _ ->
        failwith
          "platform: features is reserved for future versions of the \
           specification"
  in
  ()
