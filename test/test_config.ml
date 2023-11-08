open Alcotest
open Container_image_spec

let of_json str =
  match Yojson.Safe.from_string str with
  | exception Yojson.Json_error _ ->
      Fmt.epr "invalid JSON\n%!";
      None
  | json -> (
      match Config.OCI.of_yojson json with
      | Ok x -> Some x
      | Error e ->
          Fmt.epr "JSON error: %s\n%!" e;
          None)

let test_os () =
  let str =
    {|
{
    "architecture": "amd64",
    "os": 123,
    "rootfs": {
      "diff_ids": [
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ],
      "type": "layers"
    }
}
|}
  in
  match of_json str with
  | None -> ()
  | Some _ ->
      Alcotest.fail
        "expected failure: field \"os\" has numeric value, must be string"

let test_variant () =
  let str =
    {|
{
    "architecture": "arm64",
    "variant": 123,
    "os": "linux",
    "rootfs": {
      "diff_ids": [
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ],
      "type": "layers"
    }
}
  |}
  in
  match of_json str with
  | Some _ ->
      fail
        "expected failure: field \"variant\" has numeric value, must be string"
  | None -> ()

let test_config_user () =
  let str =
    {|
{
    "created": "2015-10-31T22:22:56.015925234Z",
    "author": "Alyssa P. Hacker <alyspdev@example.com>",
    "architecture": "amd64",
    "os": "linux",
    "config": {
        "User": 1234
    },
    "rootfs": {
      "diff_ids": [
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ],
      "type": "layers"
    }
}
  |}
  in
  match of_json str with
  | Some _ ->
      fail
        "expected failure: field \"config.User\" has numeric value, must be \
         string"
  | None -> ()

let test_history () =
  let str =
    {|
{
    "history": "should be an array",
    "architecture": "amd64",
    "os": 123,
    "rootfs": {
      "diff_ids": [
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ],
      "type": "layers"
    }
}
  |}
  in
  match of_json str with
  | Some _ ->
      fail "expected failure: history has string value, must be an array"
  | None -> ()

let test_env_numeric () =
  let str =
    {|
{
    "architecture": "amd64",
    "os": 123,
    "config": {
        "Env": [
            7353
        ]
    },
    "rootfs": {
      "diff_ids": [
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ],
      "type": "layers"
    }
}
  |}
  in
  match of_json str with
  | Some _ -> fail "expected failure: Env has numeric value, must be a string"
  | None -> ()

let test_volumes_string_array () =
  let str =
    {|
{
    "architecture": "amd64",
    "os": 123,
    "config": {
        "Volumes": [
            "/var/job-result-data",
            "/var/log/my-app-logs"
        ]
    },
    "rootfs": {
      "diff_ids": [
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ],
      "type": "layers"
    }
}
  |}
  in
  match of_json str with
  | Some _ ->
      fail
        "expected failure: config.Volumes has string array, must be an object \
         (string set)"
  | None -> ()

let test_invalid_json () =
  let str = {| invalid JSON |} in
  match of_json str with
  | Some _ -> fail "expected failure: invalid JSON"
  | None -> ()

let test_valid_config_optional_fields () =
  let str =
    {|
{
    "created": "2015-10-31T22:22:56.015925234Z",
    "author": "Alyssa P. Hacker <alyspdev@example.com>",
    "architecture": "arm64",
    "variant": "v8",
    "os": "linux",
    "config": {
        "User": "1:1",
        "ExposedPorts": {
            "8080/tcp": {}
        },
        "Env": [
            "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
            "FOO=docker_is_a_really",
            "BAR=great_tool_you_know"
        ],
        "Entrypoint": [
            "/bin/sh"
        ],
        "Cmd": [
            "--foreground",
            "--config",
            "/etc/my-app.d/default.cfg"
        ],
        "Volumes": {
            "/var/job-result-data": {},
            "/var/log/my-app-logs": {}
        },
        "StopSignal": "SIGKILL",
        "WorkingDir": "/home/alice",
        "Labels": {
            "com.example.project.git.url": "https://example.com/project.git",
            "com.example.project.git.commit": "45a939b2999782a3f005621a8d0f29aa387e1d6b"
        }
    },
    "rootfs": {
      "diff_ids": [
        "sha256:9d3dd9504c685a304985025df4ed0283e47ac9ffa9bd0326fddf4d59513f0827",
        "sha256:2b689805fbd00b2db1df73fae47562faac1a626d5f61744bfe29946ecff5d73d"
      ],
      "type": "layers"
    },
    "history": [
      {
        "created": "2015-10-31T22:22:54.690851953Z",
        "created_by": "/bin/sh -c #(nop) ADD file:a3bc1e842b69636f9df5256c49c5374fb4eef1e281fe3f282c65fb853ee171c5 in /"
      },
      {
        "created": "2015-10-31T22:22:55.613815829Z",
        "created_by": "/bin/sh -c #(nop) CMD [\"sh\"]",
        "empty_layer": true
      }
    ]
}
  |}
  in
  match of_json str with
  | Some _ -> ()
  | None -> fail "expected valid configuration with optional fields"

let test_valid_config_required_fields () =
  let str =
    {|
{
    "architecture": "amd64",
    "os": "linux",
    "rootfs": {
      "diff_ids": [
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ],
      "type": "layers"
    }
}
  |}
  in
  match of_json str with
  | Some _ -> ()
  | None -> fail "expected valid configuration with only required fields"

let test_env_invalid () =
  let str =
    {|
{
    "architecture": "amd64",
    "os": "linux",
    "config": {
        "Env": [
            "foo"
        ]
    },
    "rootfs": {
      "diff_ids": [
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
],
"type": "layers"
}
}
|}
  in
  match of_json str with
  | Some _ ->
      Alcotest.fail
        "expected failure: Env value invalid, must be in the format KEY=VALUE"
  | None -> ()

let test_example () =
  let json =
    {|
{
    "created": "2015-10-31T22:22:56.015925234Z",
    "author": "Alyssa P. Hacker <alyspdev@example.com>",
    "architecture": "amd64",
    "os": "linux",
    "config": {
        "User": "alice",
        "ExposedPorts": {
            "8080/tcp": {}
        },
        "Env": [
            "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
            "FOO=oci_is_a",
            "BAR=well_written_spec"
        ],
        "Entrypoint": [
            "/bin/my-app-binary"
        ],
        "Cmd": [
            "--foreground",
            "--config",
            "/etc/my-app.d/default.cfg"
        ],
        "Volumes": {
            "/var/job-result-data": {},
            "/var/log/my-app-logs": {}
        },
        "WorkingDir": "/home/alice",
        "Labels": {
            "com.example.project.git.url": "https://example.com/project.git",
            "com.example.project.git.commit": "45a939b2999782a3f005621a8d0f29aa387e1d6b"
        }
    },
    "rootfs": {
      "diff_ids": [
        "sha256:c6f988f4874bb0add23a778f753c65efe992244e148a1d2ec2a8b664fb66bbd1",
        "sha256:5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
      ],
      "type": "layers"
    },
    "history": [
      {
        "created": "2015-10-31T22:22:54.690851953Z",
        "created_by": "/bin/sh -c #(nop) ADD file:a3bc1e842b69636f9df5256c49c5374fb4eef1e281fe3f282c65fb853ee171c5 in /"
      },
      {
        "created": "2015-10-31T22:22:55.613815829Z",
        "created_by": "/bin/sh -c #(nop) CMD [\"sh\"]",
        "empty_layer": true
      },
      {
        "created": "2015-10-31T22:22:56.329850019Z",
        "created_by": "/bin/sh -c apk add curl"
      }
    ]
}
  |}
  in
  match of_json json with Some _ -> () | None -> Alcotest.fail "example"

let config_of_json str =
  match Yojson.Safe.from_string str with
  | exception Yojson.Json_error _ ->
      Fmt.epr "invalid JSON\n%!";
      None
  | json -> (
      match Config.OCI.config_of_yojson json with
      | Ok x -> Some x
      | Error e ->
          Fmt.epr "JSON error: %s\n%!" e;
          None)

let test_config () =
  let json =
    {|
{
  "ExposedPorts": {
      "6379/tcp": {}
  },
  "Env": [
      "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
      "GOSU_VERSION=1.16",
      "REDIS_VERSION=7.2.3",
      "REDIS_DOWNLOAD_URL=http://download.redis.io/releases/redis-7.2.3.tar.gz",
      "REDIS_DOWNLOAD_SHA=3e2b196d6eb4ddb9e743088bfc2915ccbb42d40f5a8a3edd8cb69c716ec34be7"
  ],
  "Entrypoint": [
      "docker-entrypoint.sh"
  ],
  "Cmd": [
      "redis-server"
  ],
  "Volumes": {
      "/data": {}
  },
  "WorkingDir": "/data",
  "ArgsEscaped": true,
  "OnBuild": null
}|}
  in
  match config_of_json json with
  | Some _ -> ()
  | None -> Alcotest.fail "config example"

let suite =
  [
    test_case "os" `Quick test_os;
    test_case "variant" `Quick test_variant;
    test_case "config_user" `Quick test_config_user;
    test_case "history" `Quick test_history;
    test_case "env_numeric" `Quick test_env_numeric;
    test_case "volumes_string_array" `Quick test_volumes_string_array;
    test_case "invalid_json" `Quick test_invalid_json;
    test_case "env_invalid" `Quick test_env_invalid;
    test_case "valid_config_optional_fields" `Quick
      test_valid_config_optional_fields;
    test_case "valid_config_required_fields" `Quick
      test_valid_config_required_fields;
    test_case "example" `Quick test_example;
    test_case "config example" `Quick test_config;
  ]
