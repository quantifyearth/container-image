open Container_image_spec
module B = Blob

type task = { promise : unit Eio.Promise.t; resolver : unit Eio.Promise.u }

type t = {
  root : [ `Dir ] Eio.Path.t;
  lock : Eio.Mutex.t;
  pending : (string, task) Hashtbl.t;
}

let v root = { root; lock = Eio.Mutex.create (); pending = Hashtbl.create 13 }
let ( / ) = Eio.Path.( / )
let mkdirs dir = Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir

let mkdir_parent file =
  match Eio.Path.split file with
  | None -> ()
  | Some (parent, _) -> mkdirs parent

let init t =
  mkdirs (t.root / "blobs" / "sha256");
  mkdirs (t.root / "manifests")

let with_lock lock fn =
  Eio.Mutex.lock lock;
  let finally () = Eio.Mutex.unlock lock in
  Fun.protect ~finally fn

let task path = Eio.Path.native_exn path

let remove_task t path =
  let task = task path in
  with_lock t.lock (fun () ->
      match Hashtbl.find_opt t.pending task with
      | None -> ()
      | Some { resolver; _ } ->
          Eio.Promise.resolve resolver ();
          Hashtbl.remove t.pending task)

let find_and_add_task t ?size path =
  let task = task path in
  with_lock t.lock (fun () ->
      match Hashtbl.find_opt t.pending task with
      | Some t -> `Pending t
      | None ->
          let exists = Eio.Path.is_file path in
          let correct_size =
            match size with
            | None -> true
            | Some size ->
                exists && (Eio.Path.stat ~follow:true path).size = size
          in
          if exists && correct_size then `Already_exists
          else
            let p, u = Eio.Promise.create ~label:task () in
            Hashtbl.add t.pending task { promise = p; resolver = u };
            (* if broken file, delete it from the cache *)
            if exists then Eio.Path.unlink path;
            let finally () = remove_task t path in
            `Fresh finally)

let if_exists t ?size ?(then_ = Fun.id) ?(else_ = Fun.id) file =
  match find_and_add_task t ?size file with
  | `Already_exists -> then_ ()
  | `Fresh finally -> Fun.protect ~finally else_
  | `Pending l ->
      Eio.Promise.await l.promise;
      then_ ()

module Blob = struct
  let file t digest =
    let algo = Digest.string_of_algorithm (Digest.algorithm digest) in
    let hash = Digest.encoded_hash digest in
    Eio.Path.(t.root / "blobs" / algo / hash)

  let if_exists t ~size ?then_ ?else_ digest =
    if_exists t ~size ?then_ ?else_ (file t digest)

  let add_fd t digest body =
    Eio.Switch.run @@ fun sw ->
    let file = file t digest in
    mkdir_parent file;
    let dst = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) file in
    Flow.copy body dst

  let add_string t digest body =
    Eio.Switch.run @@ fun sw ->
    let file = file t digest in
    mkdir_parent file;
    let dst = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) file in
    let body = Eio.Flow.string_source body in
    try
      Eio.Flow.copy body dst;
      Eio.Flow.close dst
    with e ->
      Eio.Flow.close dst;
      Eio.Path.unlink file;
      raise e

  let get_string t digest =
    let file = file t digest in
    Eio.Path.with_open_in file Eio.Flow.read_all

  let get_fd ~sw t digest =
    let file = file t digest in
    Eio.Path.open_in ~sw file
end

module Manifest = struct
  let file t image =
    let org = Image.org image in
    let name = Image.name image in
    let file str = Eio.Path.(t.root / "manifests" / org / name / str) in
    let tag t = file ("tags/" ^ t) in
    let digest d =
      file
        (Fmt.str "digests/%s/%s"
           Digest.(string_of_algorithm (algorithm d))
           (Digest.encoded_hash d))
    in
    match (Image.tag image, Image.digest image) with
    | None, None -> tag "latest"
    | Some t, None -> tag t
    | _, Some d -> digest d

  let if_exists t ?then_ ?else_ digest =
    if_exists t ?then_ ?else_ (file t digest)

  let list_tags dir full_name =
    let tags = Eio.Path.read_dir Eio.Path.(dir / full_name / "tags") in
    List.map
      (fun tag ->
        let image = Image.v ~tag full_name in
        image)
      tags

  let list_digests dir full_name algo =
    let digests =
      Eio.Path.read_dir Eio.Path.(dir / full_name / "digests" / algo)
    in
    List.map
      (fun digest ->
        match Digest.algorithm_of_string algo with
        | Error (`Msg e) -> failwith e
        | Ok algo ->
            let digest = Digest.unsafe_v algo digest in
            Image.v ~digest full_name)
      digests

  let list_algos dir full_name =
    let algos = Eio.Path.read_dir Eio.Path.(dir / full_name / "digests") in
    List.map (list_digests dir full_name) algos |> List.flatten

  let list t =
    let dir = Eio.Path.(t.root / "manifests") in
    let orgs = Eio.Path.read_dir dir in
    let orgs =
      Eio.Fiber.List.map
        (fun org ->
          let names = Eio.Path.read_dir Eio.Path.(dir / org) in
          List.map
            (fun name ->
              let full_name = Fmt.str "%s/%s" org name in
              let tags = list_tags dir full_name in
              let digests = list_algos dir full_name in
              tags @ digests)
            names)
        orgs
    in
    List.concat (List.concat orgs)

  exception Invalid_descriptor of Image.t * string * string

  let add t image m =
    Eio.Switch.run @@ fun sw ->
    let file = file t image in
    mkdir_parent file;
    let src = Manifest.to_string m in
    let dst = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) file in
    Eio.Flow.copy_string src dst

  let get t image =
    let file = file t image in
    let str = Eio.Path.with_open_in file Eio.Flow.read_all in
    match Manifest.of_string str with
    | Ok d -> d
    | Error (`Msg e) -> raise (Invalid_descriptor (image, "get/1", e))
end
