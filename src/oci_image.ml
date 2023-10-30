module Config = Config
module Descriptor = Descriptor
module Index = Index
module Annotation = Annotation
module Digest = Digest
module Manifest = Manifest
module Manifest_list = Manifest_list

type oci = {
  manifest : Manifest.OCI.t;
  index : Index.t option;
  layers : Layer.t list;
  config : Config.OCI.t;
}

type docker = { manigest_list : Manifest_list.t }

let get = Docker.get
