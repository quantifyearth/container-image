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

type oci = {
  manifest : Manifest.OCI.t;
  index : Index.t option;
  layers : Layer.t list;
  config : Config.OCI.t;
}

let manifest t = t.manifest
let index t = t.index
let layers t = t.layers
let config t = t.config

type docker = { manifest_list : Manifest_list.t }

let manifest_list t = t.manifest_list
