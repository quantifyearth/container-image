open Container_image_spec
module B = Blob

type t = {
  root : [ `Dir ] Eio.Path.t;
  lock : Eio.Mutex.t;
  pending : (string, Eio.Mutex.t) Hashtbl.t;
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
      | Some l ->
          Eio.Mutex.unlock l;
          Hashtbl.remove t.pending task)

let find_and_add_task t ?size path =
  let task = task path in
  with_lock t.lock (fun () ->
      match Hashtbl.find_opt t.pending task with
      | Some l -> `Pending l
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
            let l = Eio.Mutex.create () in
            Eio.Mutex.lock l;
            Hashtbl.add t.pending task l;
            (* if broken file, delete it from the cache *)
            if exists then Eio.Path.unlink path;
            `Fresh)

let if_exists t ?size ?(then_ = Fun.id) ?(else_ = Fun.id) file =
  match find_and_add_task t ?size file with
  | `Already_exists -> then_ ()
  | `Fresh ->
      let finally () = remove_task t file in
      Fun.protect ~finally else_
  | `Pending l ->
      let finally () = Eio.Mutex.unlock l in
      Eio.Mutex.lock l;
      Fun.protect ~finally then_

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

  let list t =
    let dir = Eio.Path.(t.root / "manifests") in
    let orgs = Eio.Path.read_dir dir in
    let orgs =
      Eio.Fiber.List.map
        (fun org ->
          let names = Eio.Path.read_dir Eio.Path.(dir / org) in
          List.map (fun name -> Image.v (org ^ "/" ^ name)) names)
        orgs
    in
    List.concat orgs

  let json_of_string str =
    match Yojson.Safe.from_string str with
    | json -> Ok json
    | exception Yojson.Json_error str -> Error str

  let ( let* ) x f = match x with Ok x -> f x | Error e -> Error e
  let ( let+ ) x f = match x with Ok x -> Ok (f x) | Error e -> Error e

  exception Invalid_descriptor of Image.t * string * string
  exception Invalid_media_type of Image.t * Media_type.t

  let add t image m =
    Eio.Switch.run @@ fun sw ->
    let file = file t image in
    mkdir_parent file;
    let d = Manifest.to_descriptor m in
    assert (Descriptor.decoded_data d = Ok (Manifest.to_string m));
    let src = Descriptor.to_string d in
    let dst = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) file in
    Eio.Flow.copy_string src dst

  let resolve image d =
    let media_type = Descriptor.media_type d in
    match Descriptor.decoded_data d with
    | Error (`Msg e) -> raise (Invalid_descriptor (image, "resolve/1", e))
    | Ok str -> (
        let r =
          let+ blob = B.of_string ~media_type str in
          match B.v blob with
          | Docker (Image_manifest m) -> `Docker_manifest m
          | Docker (Image_manifest_list l) -> `Docker_manifest_list l
          | OCI (Image_index i) -> `OCI_index i
          | OCI (Image_manifest m) -> `OCI_manifest m
          | _ -> raise (Invalid_media_type (image, media_type))
        in
        match r with
        | Ok r -> r
        | Error (`Msg e) -> raise (Invalid_descriptor (image, "resolve/2", e)))

  let get t image =
    let file = file t image in
    let str = Eio.Path.with_open_in file Eio.Flow.read_all in
    let r =
      let* json = json_of_string str in
      Descriptor.of_yojson json
    in
    match r with
    | Ok d -> resolve image d
    | Error e -> raise (Invalid_descriptor (image, "get/1", e))
end
