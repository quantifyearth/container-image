(** OCI - Open Container Initiative image format.

    This module provides abstractions and functions for creating, manipulating,
    and interpreting OCI-compliant container images. *)

module Config = Config
module Descriptor = Descriptor
module Index = Index

(*
 type digest
 (** Type representing a content-addressable identifier in an OCI image. *)

 val digest_of_string : string -> digest
 (** [digest_of_string s] creates a digest from the string [s]. *)

 val string_of_digest : digest -> string
 (** [string_of_digest d] returns the string representation of the digest [d]. *)

 type media_type =
   | Application_Vnd_Oci_Image_Manifest_V1_Json
   | Application_Vnd_Oci_Image_Layer_V1_Tar
   | Application_Vnd_Oci_Image_Config_V1_Json
   | Application_Vnd_Oci_Image_Rootfs_Tar
 (** The type of media types in the OCI specification. *)

 val media_type_of_string : string -> media_type
 (** [media_type_of_string s] is the media type corresponding to string [s]. *)

 val string_of_media_type : media_type -> string
 (** [string_of_media_type m] is the string representation of media type [m]. *)

 type descriptor = {
   media_type : media_type;
   size : int64;
   digest : digest;
   urls : string list;
   annotations : (string * string) list;
 }
 (** Type representing a content descriptor or a reference to data in an OCI image. *)

 val descriptor_of_yojson : Yojson.Safe.t -> (descriptor, string) result
 (** [descriptor_of_yojson json] constructs a descriptor from the JSON object [json]. *)

 val descriptor_to_yojson : descriptor -> Yojson.Safe.t
 (** [descriptor_to_yojson d] returns the JSON representation of the descriptor [d]. *)

 (* ... similar setup and documentation for other types like image_config, image_manifest, etc. ... *)

 val load_image : string -> (descriptor, string) result
 (** [load_image path] loads an OCI image from the filesystem at [path].
     Returns the main descriptor of the image or an error message if the loading fails. *)

 val save_image : descriptor -> string -> (unit, string) result
 (** [save_image descriptor path] saves an OCI image to the filesystem at [path].
     The [descriptor] should represent the main manifest of the OCI image.
     Returns unit on success or an error message on failure. *)

 (* ... additional functions and types with comprehensive documentation ... *)


 *)
