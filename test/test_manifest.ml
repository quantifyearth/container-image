open Alcotest
open Container_image_spec

let of_json of_yojson str =
  match Yojson.Safe.from_string str with
  | exception Yojson.Json_error _ ->
      Fmt.epr "invalid JSON\n%!";
      None
  | json -> (
      match of_yojson json with
      | Ok x -> Some x
      | Error e ->
          Fmt.epr "JSON error: %s\n%!" e;
          None)

let oci_of_json = of_json Manifest.OCI.of_yojson

let test_invalid_media_type () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "invalid",
    "size": 1470,
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 148,
      "digest": "sha256:c57089565e894899735d458f0fd4bb17a0f1e0df8d72da392b85c9b35ee777cd"
    }
  ]
     }
|}
  in
  match oci_of_json json with
  | Some _ -> Alcotest.fail "expected failure: mediaType does not match pattern"
  | None -> ()

let test_invalid_config_size () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": "1470",
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 148,
      "digest": "sha256:c57089565e894899735d458f0fd4bb17a0f1e0df8d72da392b85c9b35ee777cd"
    }
  ]
}
|}
  in
  match oci_of_json json with
  | Some _ ->
      Alcotest.fail
        "expected failure: config.size is a string, expected integer"
  | None -> ()

let test_invalid_layers_size () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1470,
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": "675598",
      "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
    }
  ]
}
|}
  in
  match oci_of_json json with
  | Some _ ->
      Alcotest.fail "expected failure: layers.size is string, expected integer"
  | None -> ()

let test_valid_manifest_with_optional_fields () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1470,
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 675598,
      "digest": "sha256:9d3dd9504c685a304985025df4ed0283e47ac9ffa9bd0326fddf4d59513f0827"
    },
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 156,
      "digest": "sha256:2b689805fbd00b2db1df73fae47562faac1a626d5f61744bfe29946ecff5d73d"
    },
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 148,
      "digest": "sha256:c57089565e894899735d458f0fd4bb17a0f1e0df8d72da392b85c9b35ee777cd"
    }
  ],
  "annotations": {
    "key1": "value1",
    "key2": "value2"
  }
}
|}
  in
  match oci_of_json json with
  | Some _ -> ()
  | None -> Alcotest.fail "valid manifest with optional fields"

let test_valid_manifest_with_required_fields () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1470,
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 675598,
      "digest": "sha256:9d3dd9504c685a304985025df4ed0283e47ac9ffa9bd0326fddf4d59513f0827"
    },
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 156,
      "digest": "sha256:2b689805fbd00b2db1df73fae47562faac1a626d5f61744bfe29946ecff5d73d"
    },
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 148,
      "digest": "sha256:c57089565e894899735d458f0fd4bb17a0f1e0df8d72da392b85c9b35ee777cd"
    }
  ]
}
|}
  in
  match oci_of_json json with
  | Some _ -> ()
  | None -> Alcotest.fail "valid manifest with only required fields"

let test_invalid_empty_layer () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1470,
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": []
}
|}
  in
  match oci_of_json json with
  | Some _ ->
      Alcotest.fail "expected failure: empty layer, expected at least one"
  | None -> ()

let test_algorithm_bounds () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1470,
    "digest": "sha256+b64:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 1470,
      "digest": "sha256+foo-bar:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
    },
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 1470,
      "digest": "sha256.foo-bar:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
    },
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 1470,
      "digest": "multihash+base58:QmRZxt2b1FVZPNqd8hsiykDL3TdBDeTSPX9Kv46HmX4Gx8"
    }
  ]
}
|}
  in
  match oci_of_json json with
  | Some _ -> ()
  | None ->
      Alcotest.fail "expected pass: test bounds of algorithm field in digest."

let test_subject () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1470,
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 1470,
      "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
    }
  ],
  "subject" : {
    "mediaType": "application/vnd.oci.image.manifest.v1+json",
    "size": 1234,
    "digest": "sha256:220a60ecd4a3c32c282622a625a54db9ba0ff55b5ba9c29c7064a2bc358b6a3e"
  }
}
|}
  in
  match oci_of_json json with
  | Some _ -> ()
  | None ->
      Alcotest.fail "expected success: subject field with a valid descriptor"

let test_invalid_subject () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1470,
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 1470,
      "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
    }
  ],
  "subject" : ".nope"
}
|}
  in
  match oci_of_json json with
  | Some _ ->
      Alcotest.fail
        "expected failure: subject field with invalid value (something that is \
         not a descriptor)"
  | None -> ()

let test_invalid_algorithm_bounds () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "size": 1470,
    "digest": "sha256+b64:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "size": 1470,
      "digest": "sha256+foo+-b:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
    }
  ]
}
|}
  in
  match oci_of_json json with
  | Some _ ->
      Alcotest.fail
        "expected failure: push bounds of algorithm field in digest too far."
  | None -> ()

let test_config () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.example.config+json",
    "size": 1470,
    "digest": "sha256:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
  },
  "layers": [
    {
      "mediaType": "application/vnd.example.data+type",
      "size": 675598,
      "digest": "sha256:9d3dd9504c685a304985025df4ed0283e47ac9ffa9bd0326fddf4d59513f0827"
    }
  ]
}
|}
  in
  match oci_of_json json with
  | Some _ -> ()
  | None ->
      Alcotest.fail "valid manifest for an artifact with a dedicated config"

let test_empty_config () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.manifest.v1+json",
  "artifactType": "application/vnd.example+type",
  "config": {
    "mediaType": "application/vnd.oci.empty.v1+json",
    "size": 2,
    "digest": "sha256:44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a"
  },
  "layers": [
    {
      "mediaType": "application/vnd.example+type",
      "size": 675598,
      "digest": "sha256:9d3dd9504c685a304985025df4ed0283e47ac9ffa9bd0326fddf4d59513f0827"
    }
  ]
}
|}
  in
  match oci_of_json json with
  | Some _ -> ()
  | None ->
      Alcotest.fail
        "valid manifest for an artifact using the empty config and artifactType"

let test_docker () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.docker.distribution.manifest.list.v2+json",
  "manifests": [
    {
      "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
      "size": 530,
      "digest": "sha256:7b5e783161f46453f3f3d8bc81917bbf16fe8ef212e63e1e903186eb6afad1c6",
      "platform": {
        "architecture": "amd64",
        "os": "linux"
      }
    },
    {
      "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
      "size": 530,
      "digest": "sha256:dbe0a2d09b7e2e9ac138a95eea31576da52f0e83f2c3260ffe8c26ed24118f19",
      "platform": {
        "architecture": "arm64",
        "os": "linux"
      }
    }
  ]
}
  |}
  in
  match of_json Manifest_list.of_yojson json with
  | Some _ -> ()
  | None -> Alcotest.fail "invalide docker flat manifest"

let suite =
  [
    test_case "test_invalid_media_type" `Quick test_invalid_media_type;
    test_case "test_invalid_config_size" `Quick test_invalid_config_size;
    test_case "test_invalid_layers_size" `Quick test_invalid_layers_size;
    test_case "test_valid_manifest_with_optional_fields" `Quick
      test_valid_manifest_with_optional_fields;
    test_case "test_valid_manifest_with_required_fields" `Quick
      test_valid_manifest_with_required_fields;
    test_case "test_invalid_empty_layer" `Quick test_invalid_empty_layer;
    test_case "test_algorithm_bounds" `Quick test_algorithm_bounds;
    test_case "test_subject" `Quick test_subject;
    test_case "test_invalid_subject" `Quick test_invalid_subject;
    test_case "test_invalid_algorithm_bounds" `Quick
      test_invalid_algorithm_bounds;
    test_case "test_config" `Quick test_config;
    test_case "test_empty_config" `Quick test_empty_config;
    test_case "docker manifest list" `Quick test_docker;
  ]
