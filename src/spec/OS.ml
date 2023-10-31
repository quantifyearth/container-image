open Common

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

let to_string = function
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

let of_string = function
  | "aix" -> Ok Aix
  | "android" -> Ok Android
  | "darwin" -> Ok Darwin
  | "dragonfly" -> Ok Dragonfly
  | "freebsd" -> Ok Freebsd
  | "illumos" -> Ok Illumos
  | "ios" -> Ok Ios
  | "js" -> Ok Js
  | "linux" -> Ok Linux
  | "netbsd" -> Ok Netbsd
  | "openbsd" -> Ok Openbsd
  | "plan9" -> Ok Plan9
  | "solaris" -> Ok Solaris
  | "wasip1" -> Ok Wasip1
  | "windows" -> Ok Windows
  | s -> error_msg "OS.of_string: invalid string (%S)" s

let to_yojson os = `String (to_string os)
let of_yojson = function `String s -> unwrap (of_string s) | _ -> Error "os"
let pp = Fmt.of_to_string to_string
