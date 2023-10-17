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

let test_descriptor json expected_success test_name =
  let test_fun () =
    match of_json json with
    | Some _ ->
        if not expected_success then
          Alcotest.failf "Test failed: %s - unexpected valid descriptor"
            test_name
    | None ->
        if expected_success then
          Alcotest.failf "Test failed: %s - this test is a valid descriptor"
            test_name
  in
  (test_name, `Quick, test_fun)

let suite =
  [
    (* Valid descriptor *)
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
       |}
      true "Valid descriptor";
    (* Invalid descriptor: size is negative *)
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": -1,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
       |}
      false "Invalid descriptor with negative size";
    (* Invalid descriptor: digest is missing *)
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682
}
       |}
      false "Invalid descriptor with missing digest";
    (* Invalid descriptor: extra field *)
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270",
  "extraField": "extraValue"
}
       |}
      false "Invalid descriptor with extra field";
    (* Invalid descriptor: mediaType is missing *)
    test_descriptor
      {|
{
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
       |}
      false "Invalid descriptor with missing mediaType";
    (* Invalid descriptor: size is a string *)
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": "7682",
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
       |}
      false "Invalid descriptor with size as a string";
    (* Invalid descriptor: digest is null *)
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": null
}
       |}
      false "Invalid descriptor with null digest";
    (* Invalid descriptor: mediaType is incorrect *)
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+xml",
  "size": 7682,
  "digest": "sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
       |}
      false "Invalid descriptor with incorrect mediaType";
    (* Invalid descriptor: digest has invalid format *)
    test_descriptor
      {|
{
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "size": 7682,
  "digest": "sha256_invalid:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"
}
       |}
      false "Invalid descriptor with incorrect digest format";
    (* Invalid descriptor: JSON structure is completely invalid *)
    test_descriptor
      {|
{
  "mediaType": ["application/vnd.oci.image.manifest.v1+json"],
  "size": {"number": 7682},
  "digest": ["sha256:5b0bcabd1ed22e9fb1310cf6c2dec7cdef19f0ad69efa1f392e94a4333501270"]
}
       |}
      false "Completely invalid JSON structure";
  ]
