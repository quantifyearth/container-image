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

let to_yojson os = `String (to_string os)

let of_yojson = function
  | `String s -> ( match of_string s with None -> Error "os" | Some s -> Ok s)
  | _ -> Error "os"

let pp = Fmt.of_to_string to_string
