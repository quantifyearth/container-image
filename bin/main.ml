open Cmdliner

let all_tags =
  Arg.(
    value
    @@ flag
    @@ info ~doc:"Download all tagged images in the repository"
         [ "a"; "all-tags" ])

let disable_content_trust =
  Arg.(
    value
    @@ flag
    @@ info ~doc:"Skip image verification (default true)"
         [ "disable-content-trust" ])

let platform =
  Arg.(
    value
    @@ opt (some string) None
    @@ info ~doc:"Set platform if server is multi-platform capable"
         [ "platform string" ])

let image =
  Arg.(
    required
    @@ pos 0 (some string) None
    @@ info ~doc:"Download an image from a registry" ~docv:"NAME[:TAG|@DIGEST]"
         [])

let setup =
  let style_renderer = Fmt_cli.style_renderer () in
  Term.(
    const (fun style_renderer level ->
        Fmt_tty.setup_std_outputs ?style_renderer ();
        Logs.set_level level;
        Logs.set_reporter (Logs_fmt.reporter ()))
    $ style_renderer
    $ Logs_cli.level ())

let run () all_tags disable_content_trust platform image =
  ignore (all_tags, disable_content_trust, platform, image);
  Oci_image.get image

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  Cmd.v
    (Cmd.info "oci-image" ~version)
    Term.(
      const run $ setup $ all_tags $ disable_content_trust $ platform $ image)

let () = exit (Cmd.eval cmd)
