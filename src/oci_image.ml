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

let get = Docker.get
