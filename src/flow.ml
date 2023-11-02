open Container_image_spec

let with_lock ~lock fn =
  Mutex.lock lock;
  let finally () = Mutex.unlock lock in
  Fun.protect ~finally fn

module Progress = struct
  type t = { flow : Eio.Flow.source_ty Eio.Resource.t; progress : int -> unit }

  let read_methods = []
  let lock = Mutex.create ()

  let single_read (t : t) buf =
    let i = Eio.Flow.single_read t.flow buf in
    with_lock ~lock (fun () -> t.progress i);
    i
end

module Digest = struct
  type t = {
    flow : Eio.Flow.source_ty Eio.Resource.t;
    feed : ?off:int -> ?len:int -> Cstruct.t -> unit;
    length : int64;
    read : int64 ref;
  }

  let read_methods = []

  let finalise ctx =
    (* FIXME: make it work for SHA512 too *)
    let hash = Digestif.SHA256.(get !ctx) in
    let hex = Digestif.SHA256.(to_hex hash) in
    Digest.sha256 hex

  let single_read (t : t) buf =
    let i = Eio.Flow.single_read t.flow buf in
    t.read := Int64.add !(t.read) (Int64.of_int i);
    if !(t.read) > t.length then failwith "stream too long";
    t.feed ~off:0 ~len:i buf;
    i
end

module Gzip = struct
  type state = Read | Flush of int

  type t = {
    flow : Eio.Flow.source_ty Eio.Resource.t;
    mutable decoder : Gz.Inf.decoder;
    i : De.bigstring;
    o : De.bigstring;
    mutable state : state;
  }

  let read_methods = []

  let rec single_read t buf =
    match t.state with
    | Flush rem ->
        let io_len = De.io_buffer_size in
        let off = io_len - rem in
        let len = min io_len (Cstruct.length buf) in
        Cstruct.blit (Cstruct.of_bigarray ~off t.o) 0 buf 0 len;
        let rem = rem - len in
        if rem = 0 then (
          let decoder = Gz.Inf.flush t.decoder in
          t.decoder <- decoder;
          t.state <- Read)
        else t.state <- Flush rem;
        len
    | Read -> (
        match Gz.Inf.decode t.decoder with
        | `Await decoder ->
            let i = Eio.Flow.single_read t.flow (Cstruct.of_bigarray t.i) in
            let decoder = Gz.Inf.src decoder t.i 0 i in
            t.decoder <- decoder;
            single_read t buf
        | `Flush decoder ->
            t.state <- Flush (De.io_buffer_size - Gz.Inf.dst_rem decoder);
            single_read t buf
        | `Malformed err -> Fmt.failwith "Gzip.single_read: Error %s" err
        | `End decoder ->
            t.state <- Flush (De.io_buffer_size - Gz.Inf.dst_rem decoder);
            single_read t buf)
end

type 'a t = { finalise : unit -> unit; flow : 'a Eio.Resource.t }

let progress_handler = Eio.Flow.Pi.source (module Progress)
let digest_handler = Eio.Flow.Pi.source (module Digest)
let gzip_handler = Eio.Flow.Pi.source (module Gzip)

let with_progress ~progress flow =
  Eio.Resource.T (Progress.{ flow; progress }, progress_handler)

type ctx = { ctx : Digestif.SHA256.ctx ref; read : int64 ref; length : int64 }

let ctx ~length =
  (* FIXME: make it work for SHA512 too *)
  let ctx = ref (Digestif.SHA256.init ()) in
  let read = ref 0L in
  { ctx; read; length }

let with_digest ~ctx:{ ctx; read; length } flow =
  (* FIXME: make it work for SHA512 too *)
  let feed ?off ?len buf =
    let arr = Cstruct.to_bigarray buf in
    ctx := Digestif.SHA256.feed_bigstring !ctx ?off ?len arr
  in
  Eio.Resource.T (Digest.{ flow; feed; length; read }, digest_handler)

let with_gzip flow =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let decoder = Gz.Inf.decoder `Manual ~o in
  Eio.Resource.T (Gzip.{ flow; decoder; i; o; state = Read }, gzip_handler)

let source ~progress ~length ~digest flow =
  let ctx = ctx ~length in
  let flow = flow |> with_digest ~ctx |> with_progress ~progress in
  let finalise () =
    if !(ctx.read) <> length then failwith "invalid length";
    let d = Digest.finalise ctx.ctx in
    if d <> digest then failwith "invalid digest"
  in
  { finalise; flow }

let read_all { finalise; flow } =
  let str = Eio.Flow.read_all flow in
  finalise ();
  str

let copy { finalise; flow } dst =
  Eio.Flow.copy flow dst;
  finalise ()
