open Alcotest
open Utils

module AuthTests = struct
  open Common

  let signature () =
    (*let expected =                                                                    *)
    (*  ( "Authorization"                                                               *)
    (*  , "AWS4-HMAC-SHA256 \                                                           *)
        (*     Credential=AKIAY6SRDBMYEY6VJETV/20240302/eu-west-2/lambda/aws4_request, \    *)
        (*     SignedHeaders=host;x-amz-date, \                                             *)
        (*     Signature=ee810415c4cb4bd262f19da69b8de7e6aafe2a416754fcf611c862938e7476bf" )*)
    (*in                                                                                *)
    (*let actual =                                                                      *)
    (*   Common.Auth.build_signature                                                    *)
    (*in                                                                                *)
    (*check' (pair string string) ~expected                                             *)
    ()
  ;;

  let hash () =
    let open Mirage_crypto.Hash in
    let input = "test string" in
    let hash = Auth.hash_string input |> SHA256.get |> Cstruct.to_hex_string in
    let expected = "d5579c46dfcc7f18207013e65b44e4cb4e2c2298f4ac457ba8f82743f31e930b" in
    check' string ~msg:"" ~expected ~actual:hash
  ;;

  let hash_hmac () =
    let open Mirage_crypto.Hash in
    let input = "test string" in
    let key = "test key" in
    let hash =
      Auth.hash_string_hmac ~key:(Cstruct.string key) input
      |> SHA256.hmac_get
      |> Cstruct.to_hex_string
    in
    let expected = "6864a9fdc9bc77190c4bc6d1d875a0afe19461907f486f4ba5213a1f15b71cc9" in
    check' string ~msg:"" ~expected ~actual:hash
  ;;

  let suite =
    ( "Auth"
    , [ test_case "signature" `Quick signature
      ; test_case "hash" `Quick hash
      ; test_case "hash_hmac" `Quick hash_hmac
      ] )
  ;;
end

module RequestTests = struct
  open Common

  let canonical_url () =
    let url = "http://s3.amazonaws.com/examplebucket/myphoto.jpg?query=value" in
    let expected = "/examplebucket/myphoto.jpg" in
    let actual = Request.canonical_url url in
    check' string ~msg:"" ~expected ~actual
  ;;

  let canonical_query () =
    let params =
      [ "prefix", [ "somePrefix" ]; "marker", [ "someMarker" ]; "max-keys", [ "20" ] ]
    in
    let actual = Request.canonical_query params in
    let expected =
      Format.sprintf
        "%s=%s&%s=%s&%s=%s"
        (Uri.pct_encode "marker")
        (Uri.pct_encode "someMarker")
        (Uri.pct_encode "max-keys")
        (Uri.pct_encode "20")
        (Uri.pct_encode "prefix")
        (Uri.pct_encode "somePrefix")
    in
    check' string ~msg:"" ~expected ~actual;
    let actual = [ "acl", [] ] |> Request.canonical_query in
    let expected = "acl=" in
    check' string ~msg:"" ~expected ~actual;
    let actual = [] |> Request.canonical_query in
    let expected = "" in
    check' string ~msg:"" ~expected ~actual
  ;;

  let suite =
    ( "Request"
    , [ test_case "canonical_url" `Quick canonical_url
      ; test_case "canonical_query" `Quick canonical_query
      ] )
  ;;
end

module UtilsTests = struct
  let string_join () =
    let expected = "a:b:c" in
    let actual = String.join ~sep:":" [ "a"; "b"; "c" ] in
    let () = check' string ~msg:"join parts with sep" ~expected ~actual in
    let expected = "abc" in
    let actual = String.join ~sep:"" [ "a"; "b"; "c" ] in
    let () = check' string ~msg:"join parts without sep" ~expected ~actual in
    let expected = "" in
    let actual = String.join ~sep:"" [] in
    check' string ~msg:"join without parts" ~expected ~actual
  ;;

  let string_compare () =
    let items = [ "c"; "b"; "a" ] in
    let sorted = List.sort String.compare items in
    check'
      (list string)
      ~msg:"sort single letters"
      ~expected:[ "a"; "b"; "c" ]
      ~actual:sorted;
    let items = [ "a"; "b"; "c" ] in
    let sorted = List.sort String.compare_rev items in
    check'
      (list string)
      ~msg:"sort single reversed"
      ~expected:[ "c"; "b"; "a" ]
      ~actual:sorted;
    let items = [ "armor"; "aardvark"; "apple" ] in
    let sorted = List.sort String.compare items in
    let expected = [ "aardvark"; "apple"; "armor" ] in
    check' (list string) ~msg:"sort past first letter" ~expected ~actual:sorted
  ;;

  let contains () =
    let haystack = "banana" in
    let needle = "nan" in
    let contains = String.contains_string ~needle ~haystack in
    check' bool ~msg:"" ~expected:true ~actual:contains;
    let haystack = "banana" in
    let needle = "nano" in
    let contains = String.contains_string ~needle ~haystack in
    check' bool ~msg:"" ~expected:false ~actual:contains
  ;;

  let suite =
    ( "Utils"
    , [ test_case "string_join" `Quick string_join
      ; test_case "string_compare" `Quick string_compare
      ; test_case "contains" `Quick contains
      ] )
  ;;
end

let () =
  let open Alcotest in
  run "UnitTests" [ AuthTests.suite; RequestTests.suite; UtilsTests.suite ]
;;
