(** This property specifies the CPU architecture. Image indexes SHOULD use, and
    implementations SHOULD understand, values listed in the Go Language document
    for GOARCH. *)
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
[@@deriving yojson]

val pp : t Fmt.t

type variant = V6 | V7 | V8 [@@deriving yojson]

val pp_variant : variant Fmt.t
