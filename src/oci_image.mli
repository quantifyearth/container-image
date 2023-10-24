(** OCI - Open Container Initiative image format.

    This module provides abstractions and functions for creating, manipulating,
    and interpreting OCI-compliant container images. *)

module Config = Config
module Descriptor = Descriptor
module Index = Index
module Manifest = Manifest
module Annotation = Annotation
module Digest = Digest

type t

val manifest : t -> Manifest.t
val index : t -> Index.t option
val layers : t -> Layer.t list
val config : t -> Config.t
