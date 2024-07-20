open Alcotest

module LambdaTests = struct
  open Common

  let getenv name =
    match Sys.getenv_opt name with
    | Some x -> x
    | None -> Sys_error (Format.sprintf "Missing environment variable: %s" name) |> raise
  ;;

  let load_credentials_data
    ?(access_id = getenv "AWS_ACCESS_KEY_ID")
    ?(access_secret = getenv "AWS_SECRET_ACCESS_KEY")
    ?(region = getenv "AWS_REGION")
    ()
    =
    Credentials.make ~access_id ~access_secret ~region:(Region.from_string region) ()
  ;;

  let invoke_error = Alcotest.testable Lambda.Error.Invoke.pp Lambda.Error.Invoke.equal

  let test_credentials =
    load_credentials_data
      ~access_id:"000000000000"
      ~access_secret:""
      ~region:"us-east-1"
      ()
  ;;

  let test_uri = Uri.make ~scheme:"http" ~host:"localhost" ~port:4566 ()

  let lambda_config =
    Lambda.Config.make ~credentials:test_credentials ~service_url:test_uri ()
  ;;

  let invoke () =
    let func_name = "simple" in
    let response = Lambda.invoke ~config:lambda_config ~func_name () |> Lwt_main.run in
    let expected = "{\"code\":200,\"message\":\"hello\"}" |> Result.ok in
    check' (result string invoke_error) ~msg:"" ~expected ~actual:response
  ;;

  let invoke_exception () =
    let func_name = "exception" in
    let response = Lambda.invoke ~config:lambda_config ~func_name () |> Lwt_main.run in
    let expected =
      "{\"errorType\":\"Error\",\"errorMessage\":\"error :(\",\"trace\":[\"Error: error \
       :(\",\"    at Runtime.handler (/var/task/exception.js:2:9)\",\"    at \
       Runtime.handleOnceNonStreaming (file:///var/runtime/index.mjs:1173:29)\"]}"
      |> Result.ok
    in
    check' (result string invoke_error) ~msg:"" ~expected ~actual:response
  ;;

  let invoke_not_found () =
    let func_name = "function_that_doesnt_exist" in
    let response = Lambda.invoke ~config:lambda_config ~func_name () |> Lwt_main.run in
    let expected =
      `ResourceNotFound
        "Function not found: \
         arn:aws:lambda:us-east-1:000000000000:function:function_that_doesnt_exist"
      |> Lambda.Error.Invoke.aws_error
      |> Result.error
    in
    check' (result string invoke_error) ~msg:"" ~expected ~actual:response
  ;;

  let suite =
    ( "Lambda"
    , [ test_case "invoke" `Slow invoke
      ; test_case "invoke_exception" `Slow invoke_exception
      ; test_case "invoke_not_found" `Slow invoke_not_found
      ] )
  ;;
end

let () = run "IntegrationTests" [ LambdaTests.suite ]
