open Cmdliner

let all_tags =
  Arg.(
    value
    @@ flag
    @@ info ~doc:"Download all tagged images in the repository"
         [ "a"; "all-tags" ])

let platform =
  Arg.(
    value
    @@ opt (some string) None
    @@ info ~doc:"Set platform if server is multi-platform capable"
         [ "platform" ])

let checkout_directory =
  Arg.(
    value
    @@ opt (some dir) None
    @@ info ~doc:"The directory to checkout the image to."
         [ "checkout-directory" ])

let image =
  let open Container_image in
  let image = Arg.conv (Image.of_string, Image.pp) in
  Arg.(
    required
    @@ pos 0 (some image) None
    @@ info ~doc:"Download an image from a registry" ~docv:"NAME[:TAG|@DIGEST]"
         [])

let image_id =
  Arg.(
    required
    @@ pos 0 (some string) None
    @@ info ~doc:"Download an image from a registry" ~docv:"IMAGE" [])

let username =
  Arg.(
    value
    @@ opt (some string) None
    @@ info ~doc:"Username" ~docv:"STRING" [ "username"; "u" ])

let password =
  let env = Cmd.Env.info "IMAGE_TOKEN" in
  Arg.(
    value
    @@ opt (some string) None
    @@ info ~env ~doc:"Password" ~docv:"FILE" [ "password"; "p" ])

let no_progress =
  Arg.(
    value
    @@ flag
    @@ info ~doc:"Do not display the progress bars" [ "no-progress" ])

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
  let tls_config =
    Tls.Config.client ~authenticator () |> function
    | Ok v -> v
    | Error (`Msg m) -> Fmt.failwith "tls_config failed: %s" m
  in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let cache env =
  let fs = Eio.Stdenv.fs env in
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  let root = Eio.Path.(fs / Xdg.cache_dir xdg / "image") in
  let cache = Container_image.Cache.v root in
  Container_image.Cache.init cache;
  cache

let fetch () all_tags platform image username password no_progress =
  ignore all_tags;
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let client =
    Cohttp_eio.Client.make
      ~https:(Some (https ~authenticator:null_auth))
      (Eio.Stdenv.net env)
  in
  let cache = cache env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let show_progress = not no_progress in
  Container_image.fetch ~show_progress ~client ~cache ~domain_mgr ?platform
    ?username ?password image

let list () =
  Eio_main.run @@ fun env ->
  let cache = cache env in
  let images = Container_image.list ~cache in
  let text = PrintBox.text in
  let text_bold = PrintBox.(text_with_style Style.bold) in
  let text_color c = PrintBox.(text_with_style Style.(fg_color c)) in
  let box =
    [
      text_bold "📖 REPOSITORY";
      text_bold "IMAGE ID";
      text_bold "TAGS";
      text_bold "PLATFORM";
      text_bold "SIZE";
    ]
    :: List.map
         (fun t ->
           let open Container_image.List in
           let repo = repository t in
           let tags = String.concat ", " (tags t) in
           let digest = digest t in
           let id =
             let hash = Container_image.Spec.Digest.encoded_hash digest in
             String.sub hash 0 12
           in
           let platform =
             let some = Container_image.Spec.Platform.to_string in
             Option.fold ~none:"" ~some (platform t)
           in
           let size = size t in
           [
             text_color Cyan repo;
             text id;
             text_color Yellow tags;
             text platform;
             text size;
           ])
         images
    |> PrintBox.grid_l ~bars:false ~pad:(PrintBox.hpad 1)
  in
  PrintBox_text.output stdout box;
  Fmt.pr "\n%!"

let checkout () image path =
  Eio_main.run @@ fun env ->
  let cache = cache env in
  let root =
    match path with
    | None -> Eio.Stdenv.cwd env
    | Some path ->
        let fs = Eio.Stdenv.fs env in
        Eio.Path.(fs / path)
  in
  let image = Container_image.Cache.Manifest.guess cache image in
  Container_image.checkout ~cache ~root image

let show () image =
  Eio_main.run @@ fun env ->
  let cache = cache env in
  let image = Container_image.Cache.Manifest.guess cache image in
  Container_image.show ~cache image

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let fetch_cmd =
  Cmd.v
    (Cmd.info "fetch" ~version)
    Term.(
      const fetch
      $ setup
      $ all_tags
      $ platform
      $ image
      $ username
      $ password
      $ no_progress)

let list_term = Term.(const list $ setup)
let list_cmd = Cmd.v (Cmd.info "list" ~version) list_term

let checkout_cmd =
  Cmd.v
    (Cmd.info "checkout" ~version)
    Term.(const checkout $ setup $ image_id $ checkout_directory)

let show_cmd =
  Cmd.v (Cmd.info "show" ~version) Term.(const show $ setup $ image_id)

let cmd =
  Cmd.group ~default:list_term (Cmd.info "image")
    [ fetch_cmd; list_cmd; checkout_cmd; show_cmd ]

let () =
  let () = Printexc.record_backtrace true in
  match Cmd.eval ~catch:false cmd with
  | i -> exit i
  | (exception Failure s) | (exception Invalid_argument s) ->
      Printexc.print_backtrace stderr;
      Fmt.epr "\n%a %s\n%!" Fmt.(styled `Red string) "[ERROR]" s;
      exit Cmd.Exit.cli_error
  | exception e ->
      Printexc.print_backtrace stderr;
      Fmt.epr "\n%a\n%!" Fmt.exn e;
      exit Cmd.Exit.some_error
