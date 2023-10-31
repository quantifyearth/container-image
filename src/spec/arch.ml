open Common

type t =
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

let to_string = function
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

let of_string = function
  | "386" -> Ok X386
  | "amd64" -> Ok Xamd64
  | "arm" -> Ok Arm
  | "arm64" -> Ok Arm64
  | "wasm" -> Ok Wasm
  | "loong64" -> Ok Loong64
  | "mips" -> Ok Mips
  | "mipsle" -> Ok Mipsle
  | "mips64" -> Ok Mips64
  | "mips64le" -> Ok Mips64le
  | "ppc64" -> Ok Ppc64
  | "ppc64le" -> Ok Ppc64le
  | "riscv64" -> Ok Riscv64
  | "s390x" -> Ok S390x
  | s -> error_msg "Arch.of_string: invalid string (%S)" s

let to_yojson a = `String (to_string a)
let of_yojson = function `String s -> unwrap (of_string s) | _ -> Error "arch"
let pp = Fmt.of_to_string to_string

type variant = V6 | V7 | V8

let variant_to_string = function V6 -> "v6" | V7 -> "v7" | V8 -> "v8"

let variant_of_string = function
  | "v6" -> Ok V6
  | "v7" -> Ok V7
  | "v8" -> Ok V8
  | s -> error_msg "Arch.variant_of_string: invalid string (%S)" s

let variant_to_yojson v = `String (variant_to_string v)

let variant_of_yojson = function
  | `String s -> unwrap (variant_of_string s)
  | _ -> Error "variant"

let pp_variant = Fmt.of_to_string variant_to_string
