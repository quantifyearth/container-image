(** This property specifies the operating system. Image indexes SHOULD use, and
    implementations SHOULD understand, values listed in the Go Language document
    for GOOS. *)
type t =
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
[@@deriving yojson]

val pp : t Fmt.t
