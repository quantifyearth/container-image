open Container_image_spec
module B = Blob

type t = { root : [ `Dir ] Eio.Path.t }

let v root = { root }
let ( / ) = Eio.Path.( / )
let mkdirs dir = Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir
let init t = mkdirs (t.root / "blobs" / "sha256")

module Blob = struct
  let file t digest =
    let algo = Digest.string_of_algorithm (Digest.algorithm digest) in
    let hash = Digest.encoded_hash digest in
    Eio.Path.(t.root / "blobs" / algo / hash)

  let exists t ~size digest =
    let file = file t digest in
    Eio.Path.is_file file
    &&
    let broken = (Eio.Path.stat ~follow:true file).size <> size in
    if broken then Eio.Path.unlink file;
    not broken

  let add ~sw t digest body =
    let file = file t digest in
    let () =
      match Eio.Path.split file with
      | None -> ()
      | Some (parent, _) -> mkdirs parent
    in
    let dst = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) file in
    try
      Flow.copy body dst;
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
    Eio.Path.(t.root / "manifest" / org / name)

  let exists t image = Eio.Path.is_file (file t image)

  let json_of_string str =
    match Yojson.Safe.from_string str with
    | json -> Ok json
    | exception Yojson.Json_error str -> Error str

  let ( let* ) x f = match x with Ok x -> f x | Error e -> Error e
  let ( let+ ) x f = match x with Ok x -> Ok (f x) | Error e -> Error e

  exception Invalid_descriptor of Image.t * string
  exception Invalid_media_type of Image.t * Media_type.t

  let add ~sw t image descriptor =
    let file = file t image in
    let src = Descriptor.to_string descriptor in
    let dst = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) file in
    Eio.Flow.copy_string src dst

  let get t image =
    let file = file t image in
    let str = Eio.Path.with_open_in file Eio.Flow.read_all in
    let r =
      let* json = json_of_string str in
      Descriptor.of_yojson json
    in
    match r with Ok d -> d | Error e -> raise (Invalid_descriptor (image, e))

  let resolve t image =
    let d = get t image in
    let media_type = Descriptor.media_type d in
    let str = Blob.get_string t (Descriptor.digest d) in
    let r =
      let+ blob = B.of_string ~media_type str in
      match B.v blob with
      | Docker (Image_manifest m) -> `Docker_manifest m
      | Docker (Image_manifest_list l) -> `Docker_manifest_list l
      | _ -> raise (Invalid_media_type (image, media_type))
    in
    match r with
    | Ok r -> r
    | Error (`Msg e) -> raise (Invalid_descriptor (image, e))
end
