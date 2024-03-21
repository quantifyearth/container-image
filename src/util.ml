let guess_manifest (v : Container_image_spec.Manifest.t) =
  let open Container_image_spec in
  let arch = Osrelease.Arch.v () in
  let os = Osrelease.OS.v () in
  match v with
  | `Docker_manifest d -> (
      let platform = Manifest.Docker.config d |> Descriptor.platform in
      match platform with
      | None -> None
      | Some platform ->
          let m_os =
            Osrelease.OS.of_string (Platform.os platform |> OS.to_string)
          in
          let m_arch =
            Osrelease.Arch.of_string (Platform.arch platform |> Arch.to_string)
          in
          if arch = m_arch && os = m_os then Some (Manifest.Docker.config d)
          else None)
  | `OCI_manifest m -> (
      let platform = Manifest.OCI.config m |> Descriptor.platform in
      match platform with
      | None -> None
      | Some platform ->
          let m_os =
            Osrelease.OS.of_string (Platform.os platform |> OS.to_string)
          in
          let m_arch =
            Osrelease.Arch.of_string (Platform.arch platform |> Arch.to_string)
          in
          if arch = m_arch && os = m_os then Some (Manifest.OCI.config m)
          else None)
  | `Docker_manifest_list l ->
      let manifests = Manifest_list.manifests l in
      let manifest =
        Stdlib.List.find_opt
          (fun m ->
            match Descriptor.platform m with
            | None -> false
            | Some (platform : Platform.t) ->
                let m_os =
                  Osrelease.OS.of_string (Platform.os platform |> OS.to_string)
                in
                let m_arch =
                  Osrelease.Arch.of_string
                    (Platform.arch platform |> Arch.to_string)
                in
                arch = m_arch && os = m_os)
          manifests
      in
      manifest
  | `OCI_index l ->
      let manifests = Index.manifests l in
      let manifest =
        Stdlib.List.find_opt
          (fun m ->
            match Descriptor.platform m with
            | None -> false
            | Some (platform : Platform.t) ->
                let m_os =
                  Osrelease.OS.of_string (Platform.os platform |> OS.to_string)
                in
                let m_arch =
                  Osrelease.Arch.of_string
                    (Platform.arch platform |> Arch.to_string)
                in
                arch = m_arch && os = m_os)
          manifests
      in
      manifest
