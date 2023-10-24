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

let to_yojson a = `String (to_string a)

let of_yojson = function
  | `String s -> (
      match of_string s with None -> Error "arch" | Some a -> Ok a)
  | _ -> Error "arch"

let pp = Fmt.of_to_string to_string

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

let pp_variant = Fmt.of_to_string variant_to_string
