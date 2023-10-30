open Container_image_spec

let of_json str =
  match Yojson.Safe.from_string str with
  | exception Yojson.Json_error _ ->
      Fmt.epr "invalid JSON\n%!";
      None
  | json -> (
      match Descriptor.of_yojson json with
      | Ok x -> Some x
      | Error e ->
          Fmt.epr "JSON error: %s\n%!" e;
          None)

let test_descriptor fail test_name json =
  let test_fun () =
    match of_json json with
    | Some _ ->
        if fail then Alcotest.failf "%s - unexpected valid descriptor" test_name
    | None ->
        if not fail then
          Alcotest.failf "%s - this test is a valid descriptor" test_name
  in
  (test_name, `Quick, test_fun)

let test_ok = test_descriptor false
let test_ko = test_descriptor true

let suite =
  [
    test_ok "Valid descriptor"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
 |};
    test_ko "mediaType missing"
      {|
{
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "mediaType does not match pattern (no subtype)"
      {|
{
  "mediaType": "application",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "mediaType does not match pattern (invalid first type character)"
      {|
{
  "mediaType": ".foo/bar",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "mediaType does not match pattern (invalid first subtype character)"
      {|
{
  "mediaType": "foo/.bar",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ok "mediaType has type and subtype as long as possible"
      {|
{
  "mediaType": "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567/1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "mediaType does not match pattern (type too long)"
      {|
{
  "mediaType": "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678/bar",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "mediaType does not match pattern (subtype too long)"
      {|
{
  "mediaType": "foo/12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "size missing"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "size is a string, expected integer"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": "7682",
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "digest missing"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682
}
|};
    test_ko "digest does not match pattern (missing algorithm)"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": ":5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko "digest does not match pattern (missing hash)"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:"
}
|};
    test_ko "digest does not match pattern (invalid algorithm characters)"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "SHA256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko
      "digest does not match pattern (characters needs to be lower for sha256)"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5B0BCABD1ED22E9FB1310CF6C2DEC7CDEF19F0AD69EFA1F392E94A4333501270"
}
|};
    test_ok "valid URL entry"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
  "urls": [
    "https://example.com/foo"
  ]
}
|};
    test_ko "urls does not match format (invalid url characters)"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
  "urls": [
    "value"
  ]
}
|};
    test_ok "artifactType is present and an IANA compliant value"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "artifactType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ko
      "artifactType does not match pattern (invalid first subtype character)"
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "artifactType": "foo/.bar",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
|};
    test_ok "data field is present and has base64 content"
      {|
{
  "mediaType": "text/plain",
  "size": 34,
  "data": "aHR0cHM6Ly9naXRodWIuY29tL29wZW5jb250YWluZXJzCg==",
  "digest": "sha256:2690af59371e9eca9453dc29882643f46e5ca47ec2862bd517b5e17351325153"
}
|};
    test_ok "unregistered digest"
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256+b64:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|};
    test_ok "unregistered digest"
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256+b64:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|};
    test_ok "unregistered digest"
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256+foo-bar:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|};
    test_ok "unregistered digest"
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256.foo-bar:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|};
    test_ok "unregistered digest"
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "multihash+base58:QmRZxt2b1FVZPNqd8hsiykDL3TdBDeTSPX9Kv46HmX4Gx8"
}
|};
    test_ko "repeated separators in algorithm"
      {|
{
  "mediaType": "application/vnd.oci.image.config.v1+json",
  "size": 1470,
  "digest": "sha256+foo+-b:c86f7763873b6c0aae22d963bab59b4f5debbed6685761b5951584f6efb0633b"
}
|};
    test_ok "unregistered digest"
      {|
{
  "digest": "sha256+b64u:LCa0a2j_xo_5m0U8HTBBNBNCLXBkg7-g-YpeiGJm564",
  "size": 1000000,
  "mediaType": "application/vnd.oci.image.config.v1+json"
}
|};
    test_ok
      "test for those who cannot use modulo arithmetic to recover padding."
      {|
{
  "digest": "sha256+b64u.unknownlength:LCa0a2j_xo_5m0U8HTBBNBNCLXBkg7-g-YpeiGJm564=",
  "size": 1000000,
  "mediaType": "application/vnd.oci.image.config.v1+json"
}
|};
    test_ko "invalid base64 content"
      {|
{
  "mediaType": "text/plain",
  "size": 34,
  "data": "aHR0cHM6Ly9naXRodWIuY29tL29wZW5jb250YWluZXJzCg",
  "digest": "sha256:2690af59371e9eca9453dc29882643f46e5ca47ec2862bd517b5e17351325153"
}
|};
  ]
