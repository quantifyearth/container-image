(** Configuration *)

module OCI : sig
  type t [@@deriving yojson]
end

module Docker : sig
  type t [@@deriving yojson]

  val pp : t Fmt.t
  val of_string : string -> (t, string) result
end
