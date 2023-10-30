open Alcotest
open Container_image_spec

let digest str =
  Digest.unsafe_v SHA256 Digestif.SHA256.(to_hex (digest_string str))

let chain_digest_AB = digest ("sha256:a" ^ " " ^ "sha256:b")

let chain_digest_ABC =
  digest (Digest.to_string chain_digest_AB ^ " " ^ "sha256:c")

let chain =
  let eq x y =
    match List.compare_lengths x y with
    | 0 -> List.for_all2 Digest.equal x y
    | _ -> false
  in
  Alcotest.testable Fmt.(Dump.list Digest.pp) eq

let a = Digest.unsafe_v SHA256 "a"
let b = Digest.unsafe_v SHA256 "b"
let c = Digest.unsafe_v SHA256 "c"

let test_empty () =
  let v = [] in
  let v' = Digest.chain SHA256 v in
  Alcotest.(check chain) "empty" v' []

let test_identity () =
  let v = [ a ] in
  let v' = Digest.chain SHA256 v in
  Alcotest.(check chain) "identity" v' [ a ]

let test_two () =
  let v = [ a; b ] in
  let v' = Digest.chain SHA256 v in
  Alcotest.(check chain) "two" v' [ a; chain_digest_AB ]

let test_three () =
  let v = [ a; b; c ] in
  let v' = Digest.chain SHA256 v in
  Alcotest.(check chain) "three" v' [ a; chain_digest_AB; chain_digest_ABC ]

let suite =
  [
    test_case "empty" `Quick test_empty;
    test_case "identity" `Quick test_identity;
    test_case "two" `Quick test_two;
    test_case "three" `Quick test_three;
  ]
