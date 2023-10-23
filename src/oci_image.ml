module Config = Config
module Descriptor = Descriptor
module Index = Index
module Manifest = Manifest
module Annotation = Annotation

type t = {
  manifest : Manifest.t;
  index : Index.t option;
  layers : Layer.t list;
  config : Config.t;
}

let manifest t = t.manifest
let index t = t.index
let layers t = t.layers
let config t = t.config
