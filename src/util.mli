val guess_manifest :
  Container_image_spec.Manifest.t -> Container_image_spec.Descriptor.t option
(** [guess_manifest manifest] will try to use operating system information (e.g.
    architecture) of the host to guess a distinct manifest to use. *)
