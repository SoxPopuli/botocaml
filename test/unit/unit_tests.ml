open Alcotest
open Utils

module AuthTests = struct
  open Common

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
    "Auth", [ test_case "hash" `Quick hash; test_case "hash_hmac" `Quick hash_hmac ]
  ;;
end

module RequestTests = struct
  open Common

  let auth_header_get () =
    let datetime =
      Timedesc.make
        ~tz:Timedesc.Time_zone.utc
        ~year:2013
        ~month:05
        ~day:24
        ~hour:00
        ~minute:00
        ~second:00
        ()
      |> Result.get_ok
    in
    let example_request =
      Request.make
        ~datetime
        ~meth:Request.Method.Get
        ~headers:[ "Range", "bytes=0-9" ]
        ~url:"https://examplebucket.s3.amazonaws.com/test.txt"
        ()
      |> Option.get
    in
    let access_id = "AKIAIOSFODNN7EXAMPLE" in
    let access_secret = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" in
    let auth_header =
      Request.build_auth_header
        ~datetime
        ~access_id
        ~access_secret
        ~service:"s3"
        ~region:UsEast1
        ~request:example_request
        ()
    in
    let expected_auth_header =
      "AWS4-HMAC-SHA256 \
       Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;range;x-amz-content-sha256;x-amz-date,Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
    in
    check'
      string
      ~msg:"auth header matches"
      ~actual:auth_header
      ~expected:expected_auth_header
  ;;

  let auth_header_put () =
    let datetime =
      Timedesc.make
        ~tz:Timedesc.Time_zone.utc
        ~year:2013
        ~month:05
        ~day:24
        ~hour:00
        ~minute:00
        ~second:00
        ()
      |> Result.get_ok
    in
    let example_request =
      Request.make
        ~datetime
        ~meth:(Request.Method.Put "Welcome to Amazon S3.")
        ~headers:
          [ "Date", "Fri, 24 May 2013 00:00:00 GMT"
          ; "x-amz-storage-class", "REDUCED_REDUNDANCY"
          ]
        ~url:"https://examplebucket.s3.amazonaws.com/test$file.text"
        ()
      |> Option.get
    in
    let access_id = "AKIAIOSFODNN7EXAMPLE" in
    let access_secret = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" in
    let expected_auth_header =
      "AWS4-HMAC-SHA256 \
       Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class,Signature=98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd"
    in
    let auth_header =
      Request.build_auth_header
        ~datetime
        ~request:example_request
        ~access_id
        ~access_secret
        ~region:UsEast1
        ~service:"s3"
        ()
    in
    check' string ~msg:"" ~expected:expected_auth_header ~actual:auth_header
  ;;

  let auth_header_query_param_empty () =
    let datetime =
      Timedesc.make
        ~tz:Timedesc.Time_zone.utc
        ~year:2013
        ~month:05
        ~day:24
        ~hour:00
        ~minute:00
        ~second:00
        ()
      |> Result.get_ok
    in
    let example_request =
      Request.make
        ~datetime
        ~meth:Request.Method.Get
        ~headers:[]
        ~query_params:[ "lifecycle", [] ]
        ~url:"https://examplebucket.s3.amazonaws.com/"
        ()
      |> Option.get
    in
    let access_id = "AKIAIOSFODNN7EXAMPLE" in
    let access_secret = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" in
    let expected_auth_header =
      "AWS4-HMAC-SHA256 \
       Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;x-amz-content-sha256;x-amz-date,Signature=fea454ca298b7da1c68078a5d1bdbfbbe0d65c699e0f91ac7a200a0136783543"
    in
    let auth_header =
      Request.build_auth_header
        ~datetime
        ~request:example_request
        ~access_id
        ~access_secret
        ~region:UsEast1
        ~service:"s3"
        ()
    in
    check' string ~msg:"" ~expected:expected_auth_header ~actual:auth_header
  ;;

  let auth_header_query_params () =
    let datetime =
      Timedesc.make
        ~tz:Timedesc.Time_zone.utc
        ~year:2013
        ~month:05
        ~day:24
        ~hour:00
        ~minute:00
        ~second:00
        ()
      |> Result.get_ok
    in
    let example_request =
      Request.make
        ~datetime
        ~meth:Request.Method.Get
        ~headers:[]
        ~query_params:[ "max-keys", [ "2" ]; "prefix", [ "J" ] ]
        ~url:"https://examplebucket.s3.amazonaws.com/"
        ()
      |> Option.get
    in
    let access_id = "AKIAIOSFODNN7EXAMPLE" in
    let access_secret = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" in
    let expected_auth_header =
      "AWS4-HMAC-SHA256 \
       Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;x-amz-content-sha256;x-amz-date,Signature=34b48302e7b5fa45bde8084f4b7868a86f0a534bc59db6670ed5711ef69dc6f7"
    in
    let auth_header =
      Request.build_auth_header
        ~datetime
        ~request:example_request
        ~access_id
        ~access_secret
        ~region:UsEast1
        ~service:"s3"
        ()
    in
    check' string ~msg:"" ~expected:expected_auth_header ~actual:auth_header
  ;;

  let suite =
    ( "Request"
    , [ test_case "auth_header_get" `Quick auth_header_get
      ; test_case "auth_header_put" `Quick auth_header_put
      ; test_case "auth_header_query_param_empty" `Quick auth_header_query_param_empty
      ; test_case "auth_header_query_params" `Quick auth_header_query_params
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
