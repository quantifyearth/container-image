(** Configuration *)

module OCI : sig
  type config [@@deriving yojson]
  type t [@@deriving yojson]

  val platform : t -> Platform.t
end

module Docker : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val of_string : string -> (t, [ `Msg of string ]) result
  val platform : t -> Platform.t
end

type t = OCI of OCI.t | Docker of Docker.t

val pp : t Fmt.t

val env : t -> (string * string) list
(** Environment variables *)

val platform : t -> Platform.t

val of_string :
  media_type:Media_type.t -> string -> (t, [ `Msg of string ]) result
