open Alcotest
open Container_image_spec

let of_json str =
  match Yojson.Safe.from_string str with
  | exception Yojson.Json_error _ ->
      Fmt.epr "invalid JSON\n%!";
      None
  | json -> (
      match Index.of_yojson json with
      | Ok x -> Some x
      | Error e ->
          Fmt.epr "JSON error: %s\n%!" e;
          None)

let test_invalid_media_type () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "invalid",
      "size": 7143,
      "digest": "sha256:e692418e4cbaf90ca69d05a66403747baa33ee08806650b51fab815ad7fc331f",
      "platform": {
        "architecture": "ppc64le",
        "os": "linux"
      }
    }
  ]
}
              |}
  in
  match of_json json with
  | Some _ -> fail "expected failure: mediaType does not match pattern"
  | None -> ()

let test_manifest_size_as_string () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": "7682",
      "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
      "platform": {
        "architecture": "amd64",
        "os": "linux"
      }
    }
  ]
}
|}
  in
  match of_json json with
  | Some _ ->
      fail "expected failure: manifest.digest is missing, expected required"
  | None -> ()

let test_missing_manifest_digest () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7682,
      "platform": {
        "architecture": "amd64",
        "os": "linux"
      }
    }
  ]
}
|}
  in
  match of_json json with
  | Some _ ->
      fail "expected failure due to missing digest, but parsing was successful."
  | None -> ()

let test_missing_platform_architecture () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7682,
      "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
      "platform": {
        "os": "linux"
      }
    }
  ]
}
|}
  in
  match of_json json with
  | Some _ ->
      fail
        "expected failure due to missing platform architecture, but parsing \
         was successful."
  | None -> ()

let test_invalid_manifest_media_type () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "invalid",
      "size": 7682,
      "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
      "platform": {
        "architecture": "amd64",
        "os": "linux"
      }
    }
  ]
}
|}
  in
  match of_json json with
  | Some _ ->
      fail
        "expected failure due to invalid manifest media type, but parsing was \
         successful."
  | None -> ()

let test_empty_manifest_media_type () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "",
      "size": 7682,
      "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
      "platform": {
        "architecture": "amd64",
        "os": "linux"
      }
    }
  ]
}
|}
  in
  match of_json json with
  | Some _ ->
      fail
        "expected failure due to empty manifest media type, but parsing was \
         successful."
  | None -> ()

let test_valid_with_customized_media_type () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/customized.manifest+json",
      "size": 7143,
      "digest": "sha256:e692418e4cbaf90ca69d05a66403747baa33ee08806650b51fab815ad7fc331f",
      "platform": {
        "architecture": "ppc64le",
        "os": "linux"
      }
    }
  ]
}
|}
  in
  match of_json json with
  | Some _ -> ()
  | None -> fail "expected successful parsing, but it failed."

let test_valid_with_artifactType () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType" : "application/vnd.oci.image.index.v1+json",
  "artifactType": "application/vnd.example+type",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7143,
      "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
    },
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "artifactType": "application/vnd.example1+type",
      "size": 506,
      "digest": "sha256:99953afc4b90c7d78079d189ae10da0a1002e6be5e9e8dedaf9f7f29def42111"
    }
  ]
}
              |}
  in
  match of_json json with
  | Some _ -> ()
  | None -> fail "expected successful parsing, but it failed."

let test_valid_with_subject_field () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7682,
      "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
      "platform": {
        "architecture": "amd64",
        "os": "linux"
      }
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
  match of_json json with
  | Some _ -> ()
  | None -> fail "expected successful parsing, but it failed."

let test_invalid_subject_field () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7682,
      "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
      "platform": {
        "architecture": "amd64",
        "os": "linux"
      }
    }
  ],
  "subject" : "nope"
}
              |}
  in
  match of_json json with
  | Some _ ->
      fail
        "expected parsing to fail due to invalid subject field, but it \
         succeeded."
  | None -> ()

let test_valid_with_optional_fields () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7143,
      "digest": "sha256:e692418e4cbaf90ca69d05a66403747baa33ee08806650b51fab815ad7fc331f",
      "platform": {
        "architecture": "ppc64le",
        "os": "linux"
      }
    },
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7682,
      "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
      "platform": {
        "architecture": "amd64",
        "os": "linux"
      }
    }
  ],
  "annotations": {
    "com.example.key1": "value1",
    "com.example.key2": "value2"
  }
}
              |}
  in
  match of_json json with
  | Some _ -> ()
  | None -> fail "expected successful parsing, but it failed."

let test_valid_without_optional_fields () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7143,
      "digest": "sha256:e692418e4cbaf90ca69d05a66403747baa33ee08806650b51fab815ad7fc331f",
      "platform": {
        "architecture": "ppc64le",
        "os": "linux"
      }
    }
  ]
}
              |}
  in
  match of_json json with
  | Some _ -> ()
  | None -> fail "expected successful parsing, but it failed."

let example () =
  let json =
    {|
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.index.v1+json",
  "manifests": [
    {
      "mediaType": "application/vnd.oci.image.index.v1+json",
      "size": 7143,
      "digest": "sha256:0228f90e926ba6b96e4f39cf294b2586d38fbb5a1e385c05cd1ee40ea54fe7fd",
      "annotations": {
        "org.opencontainers.image.ref.name": "stable-release"
      }
    },
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "size": 7143,
      "digest": "sha256:e692418e4cbaf90ca69d05a66403747baa33ee08806650b51fab815ad7fc331f",
      "platform": {
        "architecture": "ppc64le",
        "os": "linux"
      },
      "annotations": {
        "org.opencontainers.image.ref.name": "v1.0"
      }
    },
    {
      "mediaType": "application/xml",
      "size": 7143,
      "digest": "sha256:b3d63d132d21c3ff4c35a061adf23cf43da8ae054247e32faa95494d904a007e",
      "annotations": {
        "org.freedesktop.specifications.metainfo.version": "1.0",
        "org.freedesktop.specifications.metainfo.type": "AppStream"
      }
    }
  ],
  "annotations": {
    "com.example.index.revision": "r124356"
  }
              }
  |}
  in
  match of_json json with
  | Some _ -> ()
  | None -> fail "expected successful parsing, but it failed."

let suite =
  [
    test_case "Invalid mediaType" `Quick test_invalid_media_type;
    test_case "Valid with optional fields" `Quick
      test_valid_with_optional_fields;
    test_case "Valid without optional fields" `Quick
      test_valid_without_optional_fields;
    test_case "Invalid manifest size as string" `Quick
      test_invalid_manifest_media_type;
    test_case "Missing platform architecture" `Quick
      test_missing_platform_architecture;
    test_case "Valid with artifactType" `Quick test_valid_with_artifactType;
    test_case "Valid with subject field" `Quick test_valid_with_subject_field;
    test_case "Invalid subject field" `Quick test_invalid_subject_field;
    test_case "Manifest size as string" `Quick test_manifest_size_as_string;
    test_case "Missing manifest digest" `Quick test_missing_manifest_digest;
    test_case "Empty manifest media type" `Quick test_empty_manifest_media_type;
    test_case "Valid with customized media type" `Quick
      test_valid_with_customized_media_type;
    test_case "example" `Quick example;
  ]
