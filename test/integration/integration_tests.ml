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

  (** Invokes real aws lambda, gets parameters from env vars *)
  let invoke_test () =
    let invoke_error =
      Alcotest.testable Lambda.Error.Aws.Invoke.pp Lambda.Error.Aws.Invoke.equal
    in
    let creds =
      load_credentials_data
        ~access_id:"000000000000"
        ~access_secret:""
        ~region:"us-east-1"
        ()
    in
    let func_name = "simple" in
    let lambda_config =
      Lambda.Config.make
        ~credentials:creds
        ~service_url:(Uri.make ~scheme:"http" ~host:"localhost" ~port:4566 ())
        ()
    in
    let response =
      Lambda.invoke ~config:lambda_config ~func_name () |> Lwt_main.run
    in
    let response = response in
    let expected = 
      "{\"code\":200,\"message\":\"hello\"}"
      |> Result.ok
    in
    check' (result string invoke_error) ~msg:"" ~expected ~actual:response
  ;;

  let suite = "Lambda", [ test_case "invoke" `Slow invoke_test ]
end

let () = run "IntegrationTests" [ LambdaTests.suite ]
