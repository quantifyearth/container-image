open Container_image_spec

let show ~cache i =
  Fmt.pr "🔎 Showing %a\n%!" Fmt.(styled `Cyan Image.pp) i;
  let m = Cache.Manifest.get cache i in
  let pp = Fmt.of_to_string Yojson.Safe.pretty_to_string in
  match m with
  | `Docker_manifest m ->
      Fmt.pr "DOCKER MANIFEST:\n%a\n%!" pp (Manifest.Docker.to_yojson m)
  | `OCI_manifest m ->
      Fmt.pr "OCI MANIFEST:\n%a\n%!" pp (Manifest.OCI.to_yojson m)
  | `OCI_index i -> Fmt.pr "OCI INDEX:\n%a\n%!" pp (Index.to_yojson i)
  | `Docker_manifest_list l ->
      Fmt.pr "DOCKER MANIFEST LIST:\n%a\n%!" pp (Manifest_list.to_yojson l)
