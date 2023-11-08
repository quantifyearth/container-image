open Container_image_spec
open Optint

type t = {
  repository : string;
  tags : string list;
  digest : Digest.t;
  platform : Platform.t option;
  size : string;
}

let repository t = t.repository
let tags t = t.tags
let digest t = t.digest
let platform t = t.platform
let size t = t.size
let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB" |]

let bytes_to_size ?(decimals = 2) n =
  if n = Int63.zero then Fmt.str "0 byte"
  else
    let n = Int63.to_float n in
    let i = Float.floor (Float.log n /. Float.log 1024.) in
    let r = n /. Float.pow 1024. i in
    Fmt.str "%.*f %s" decimals r sizes.(int_of_float i)

let of_image ~tags ~cache i =
  let repository =
    match Image.org i with "library" -> Image.name i | _ -> Image.repository i
  in
  let digest =
    match Image.digest i with Some d -> d | None -> assert false
    (* it's always a Some x because we used Cache.Manifest.list_digest
       -- would be nice to enforce this statically *)
  in
  let tags = Hashtbl.find_all tags digest in
  let m = Cache.Manifest.get cache i in
  let read d =
    let k = Descriptor.digest d in
    let media_type = Descriptor.media_type d in
    let b = Cache.Blob.get_string cache k in
    match Config.of_string ~media_type b with
    | Ok c -> c
    | Error (`Msg e) ->
        Fmt.failwith "TODO: %a %a %s" Digest.pp k Media_type.pp media_type e
  in
  let platform = Manifest.platform read m in
  let size =
    match Manifest.size m with Some s -> bytes_to_size s | None -> "-"
  in
  { repository; tags; digest; platform; size }

let list ~cache =
  let all_tags = Cache.Manifest.list_tags cache in
  let tags = Hashtbl.create (List.length all_tags) in
  List.iter
    (fun i ->
      let tag =
        match Image.tag i with Some t -> t | None -> assert false
        (* it's always a Some x because we use
           Cache.Manfifest.list_tags. It would be nice to enfore it
           statically. *)
      in
      let m = Cache.Manifest.get cache i in
      let ms = Manifest.manifests m in
      List.iter
        (fun d ->
          let digest = Descriptor.digest d in
          Hashtbl.add tags digest tag)
        ms)
    all_tags;

  let is = Cache.Manifest.list_digests cache in
  let ts = List.map (of_image ~tags ~cache) is in
  (* FIXME: should use Descriptor.attestation_manifest on the index
     instead *)
  List.filter (fun t -> t.platform <> Some Platform.unknown) ts
