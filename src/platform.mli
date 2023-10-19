(** This REQUIRED property specifies the operating system. Image indexes SHOULD
    use, and implementations SHOULD understand, values listed in the Go Language
    document for GOOS. *)
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

(** This REQUIRED property specifies the CPU architecture. Image indexes SHOULD
    use, and implementations SHOULD understand, values listed in the Go Language
    document for GOARCH. *)
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

type t [@@deriving yojson]

val arch : t -> arch
val os : t -> os
val check : t -> unit
