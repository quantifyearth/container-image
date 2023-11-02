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
         [ "platform" ])

let image =
  let open Container_image in
  let image = Arg.conv (Image.of_string, Image.pp) in
  Arg.(
    required
    @@ pos 0 (some image) None
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

let null_auth ?ip:_ ~host:_ _ =
  Ok None (* Warning: use a real authenticator in your code! *)

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let run () all_tags disable_content_trust platform image =
  ignore (all_tags, disable_content_trust, image);
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client =
    Cohttp_eio.Client.make
      ~https:(Some (https ~authenticator:null_auth))
      (Eio.Stdenv.net env)
  in
  let fs = Eio.Stdenv.fs env in
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  let root = Eio.Path.(fs / Xdg.cache_dir xdg / "container-image") in
  let cache = Container_image.Cache.v root in
  Container_image.Cache.init cache;
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  Container_image.fetch ~client ~cache ~domain_mgr ?platform image

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  Cmd.v
    (Cmd.info "oci-image" ~version)
    Term.(
      const run $ setup $ all_tags $ disable_content_trust $ platform $ image)

let () =
  match Cmd.eval ~catch:false cmd with
  | exception Failure s -> Fmt.pr "%s\n%!" s
  | i -> exit i
