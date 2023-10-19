let of_json str =
  match Yojson.Safe.from_string str with
  | exception Yojson.Json_error _ ->
      Fmt.epr "invalid JSON\n%!";
      None
  | json -> (
      match Oci_image.Descriptor.of_yojson json with
      | Ok x -> Some x
      | Error e ->
          Fmt.epr "JSON error: %s\n%!" e;
          None)

let test_descriptor json fail test_name =
  let test_fun () =
    match of_json json with
    | Some _ ->
        if fail then
          Alcotest.failf "Test failed: %s - unexpected valid descriptor"
            test_name
    | None ->
        if not fail then
          Alcotest.failf "Test failed: %s - this test is a valid descriptor"
            test_name
  in
  (test_name, `Quick, test_fun)

let suite =
  [
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
                     |}
      false "Valid descriptor";
    test_descriptor
      {|
{
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "mediaType missing";
    test_descriptor
      {|
{
  "mediaType": "application",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "mediaType does not match pattern (no subtype)";
    test_descriptor
      {|
{
  "mediaType": ".foo/bar",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "mediaType does not match pattern (invalid first type character)";
    test_descriptor
      {|
{
  "mediaType": "foo/.bar",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "mediaType does not match pattern (invalid first subtype character)";
    test_descriptor
      {|
{
  "mediaType": "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567/1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      false "mediaType has type and subtype as long as possible";
    test_descriptor
      {|
{
  "mediaType": "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678/bar",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "mediaType does not match pattern (type too long)";
    test_descriptor
      {|
{
  "mediaType": "foo/12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "mediaType does not match pattern (subtype too long)";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "size missing";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": "7682",
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "size is a string, expected integer";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682
}
|}
      true "digest missing";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": ":5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "digest does not match pattern (missing algorithm)";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:"
}
|}
      true "digest does not match pattern (missing hash)";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "SHA256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true "digest does not match pattern (invalid algorithm characters)";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5B0BCABD1ED22E9FB1310CF6C2DEC7CDEF19F0AD69EFA1F392E94A4333501270"
}
|}
      true
      "digest does not match pattern (characters needs to be lower for sha256)";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
  "urls": [
    "https://example.com/foo"
  ]
}
|}
      false "valid URL entry";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
  "urls": [
    "value"
  ]
}
|}
      true "urls does not match format (invalid url characters)";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "artifactType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      false "artifactType is present and an IANA compliant value";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "artifactType": "foo/.bar",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|}
      true
      "artifactType does not match pattern (invalid first subtype character)";
    test_descriptor
      {|
{
  "mediaType": "text/plain",
  "size": 34,
  "data": "aHR0cHM6Ly9naXRodWIuY29tL29wZW5jb250YWluZXJzCg==",
  "digest": "sha256:2690af59371e9eca9453dc29882643f46e5ca47ec2862bd517b5e17351325153"
}
|}
      false "data field is present and has base64 content";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256+b64:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|}
      false "ok";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256+b64:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|}
      false "ok";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256+foo-bar:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|}
      false "ok";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256.foo-bar:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|}
      false "ok";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "multihash+base58:QmRZxt2b1FVZPNqd8hsiykDL3TdBDeTSPX9Kv46HmX4Gx8"
}
|}
      false "ok";
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256+foo+-b:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|}
      true "repeated separators in algorithm";
    test_descriptor
      {|
{
  "digest": "sha256+b64u:LCa0a2j_xo_5m0U8HTBBNBNCLXBkg7-g-YpeiGJm564",
  "size": 1000000,
  "mediaType": "application/vnd.oci.image.config.v1+json"
}
|}
      false "ok";
    test_descriptor
      {|
{
  "digest": "sha256+b64u.unknownlength:LCa0a2j_xo_5m0U8HTBBNBNCLXBkg7-g-YpeiGJm564=",
  "size": 1000000,
  "mediaType": "application/vnd.oci.image.config.v1+json"
}
|}
      false
      "test for those who cannot use modulo arithmetic to recover padding.";
    test_descriptor
      {|
{
  "mediaType": "text/plain",
  "size": 34,
  "data": "aHR0cHM6Ly9naXRodWIuY29tL29wZW5jb250YWluZXJzCg",
  "digest": "sha256:2690af59371e9eca9453dc29882643f46e5ca47ec2862bd517b5e17351325153"
}
|}
      true "invalid base64 content";
  ]
