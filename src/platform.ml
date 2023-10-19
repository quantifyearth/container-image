type os =
  | Aix
  | Android
  | Darwin
  | Dragonfly
  | Freebsd
  | Illumos
  | Ios
  | Js
  | Linux
  | Netbsd
  | Openbsd
  | Plan9
  | Solaris
  | Wasip1
  | Windows

let os_to_string = function
  | Aix -> "aix"
  | Android -> "android"
  | Darwin -> "darwin"
  | Dragonfly -> "dragonfly"
  | Freebsd -> "freebsd"
  | Illumos -> "illumos"
  | Ios -> "ios"
  | Js -> "js"
  | Linux -> "linux"
  | Netbsd -> "netbsd"
  | Openbsd -> "openbsd"
  | Plan9 -> "plan9"
  | Solaris -> "solaris"
  | Wasip1 -> "wasip1"
  | Windows -> "windows"

let os_of_string = function
  | "aix" -> Some Aix
  | "android" -> Some Android
  | "darwin" -> Some Darwin
  | "dragonfly" -> Some Dragonfly
  | "freebsd" -> Some Freebsd
  | "illumos" -> Some Illumos
  | "ios" -> Some Ios
  | "js" -> Some Js
  | "linux" -> Some Linux
  | "netbsd" -> Some Netbsd
  | "openbsd" -> Some Openbsd
  | "plan9" -> Some Plan9
  | "solaris" -> Some Solaris
  | "wasip1" -> Some Wasip1
  | "windows" -> Some Windows
  | _ -> None

let os_to_yojson os = `String (os_to_string os)

let os_of_yojson = function
  | `String s -> (
      match os_of_string s with None -> Error "os" | Some s -> Ok s)
  | _ -> Error "os"

type arch =
  | X386
  | Xamd64
  | Arm
  | Arm64
  | Wasm
  | Loong64
  | Mips
  | Mipsle
  | Mips64
  | Mips64le
  | Ppc64
  | Ppc64le
  | Riscv64
  | S390x

let arch_to_string = function
  | X386 -> "386"
  | Xamd64 -> "amd64"
  | Arm -> "arm"
  | Arm64 -> "arm64"
  | Wasm -> "wasm"
  | Loong64 -> "loong64"
  | Mips -> "mips"
  | Mipsle -> "mipsle"
  | Mips64 -> "mips64"
  | Mips64le -> "mips64le"
  | Ppc64 -> "ppc64"
  | Ppc64le -> "ppc64le"
  | Riscv64 -> "riscv64"
  | S390x -> "s390x"

let arch_of_string = function
  | "386" -> Some X386
  | "amd64" -> Some Xamd64
  | "arm" -> Some Arm
  | "arm64" -> Some Arm64
  | "wasm" -> Some Wasm
  | "loong64" -> Some Loong64
  | "mips" -> Some Mips
  | "mipsle" -> Some Mipsle
  | "mips64" -> Some Mips64
  | "mips64le" -> Some Mips64le
  | "ppc64" -> Some Ppc64
  | "ppc64le" -> Some Ppc64le
  | "riscv64" -> Some Riscv64
  | "s390x" -> Some S390x
  | _ -> None

let arch_to_yojson a = `String (arch_to_string a)

let arch_of_yojson = function
  | `String s -> (
      match arch_of_string s with None -> Error "arch" | Some a -> Ok a)
  | _ -> Error "arch"

type variant = V6 | V7 | V8

let variant_to_string = function V6 -> "v6" | V7 -> "v7" | V8 -> "v8"

let variant_of_string = function
  | "v6" -> Some V6
  | "v7" -> Some V7
  | "v8" -> Some V8
  | _ -> None

let variant_to_yojson v = `String (variant_to_string v)

let variant_of_yojson = function
  | `String s -> (
      match variant_of_string s with None -> Error "variant" | Some v -> Ok v)
  | _ -> Error "variant"

type t = {
  architecture : arch;
  os : os;
  os_version : string option; [@key "os.version"] [@default None]
  os_features : string list; [@key "os.features"] [@default []]
  variant : variant option; [@default None]
  features : string list; [@default []]
}
[@@deriving yojson]

let arch t = t.architecture
let os t = t.os
let pp_os = Fmt.of_to_string os_to_string
let pp_variant = Fmt.of_to_string variant_to_string
let pp_arch = Fmt.of_to_string arch_to_string

let check t =
  let pp_v = Fmt.option ~none:(Fmt.any "N/A") pp_variant in
  let err_arch_variant () =
    Fmt.failwith "%a/%a: invalid architecture/variant pair" pp_arch
      t.architecture pp_v t.variant
  in
  let err_os_arch () =
    Fmt.failwith "%a/%a: invalid os/architecture pair" pp_os t.os pp_arch
      t.architecture
  in
  let () =
    match (t.os, t.os_features) with
    | Windows, [ "win32k" ] -> ()
    | Windows, l ->
        Fmt.failwith "%a/%a invalid os/os.features pair" pp_os t.os
          Fmt.(Dump.list string)
          l
    | _ -> ()
  in
  let () =
    match (t.architecture, t.variant) with
    | Arm, Some V6 | Arm, Some V7 | Arm, Some V8 | Arm64, Some V8 -> ()
    | Arm, _ | Arm64, _ | _, Some _ -> err_arch_variant ()
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
    | _, _ -> err_os_arch ()
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
