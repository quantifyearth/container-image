(** OCI - Open Container Initiative image format.

    This module provides abstractions and functions for creating, manipulating,
    and interpreting OCI-compliant container images. *)

module Config = Config
module Descriptor = Descriptor
module Index = Index
module Manifest = Manifest
module Annotation = Annotation
module Digest = Digest

type oci = {
  manifest : Manifest.OCI.t;
  index : Index.t option;
  layers : Layer.t list;
  config : Config.t;
}

type docker = { manigest_list : Manifest_list.t }

val get : string -> unit
