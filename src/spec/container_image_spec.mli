(** {1 Container Image Spec}

    This module provides converters from OCI and Docker image specifications to
    OCaml data types. It abstracts the details of each image specification to
    allow easy integration and manipulation in OCaml applications.

    Users of this module can expect types and sub-modules representing each of
    the main components of OCI and Docker image specifications. *)

(** {1 Types} *)

module Config = Config
module Descriptor = Descriptor
module Index = Index
module Annotation = Annotation
module Digest = Digest
module Manifest = Manifest
module Manifest_list = Manifest_list
module Layer = Layer
module Media_type = Media_type
module Blob = Blob
module Auth = Auth
module Platform = Platform
module OS = OS

type oci
(** The type for OCI images as described in the
    {{:https://github.com/opencontainers/image-spec/blob/main/spec.md} Image
      Format Specification} of the
    {{:https://opencontainers.org/} Open Container Initative}. *)

val manifest : oci -> Manifest.OCI.t
(** [manifest img] is the manifest of the OCI image [img]. *)

val index : oci -> Index.t option
(** [index img] is the optional index of the OCI image [img]. *)

val layers : oci -> Layer.t list
(** [layers img] is the list of layers of the OCI image [img]. *)

val config : oci -> Config.OCI.t
(** [config img] is the configuration of the OCI image [img]. *)

type docker
(** The type for Docker images as specified by the
    {{:https://github.com/moby/moby/blob/master/image/spec/spec.md} Docker Image
      Specification v1.3} *)

val manifest_list : docker -> Manifest_list.t
(** [manifest_list img] is the manifest list of the Docker image [img]. *)
