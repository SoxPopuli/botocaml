open Alcotest

module LambdaTests = struct
  open Common

  let getenv name =
    match Sys.getenv_opt name with
    | Some x -> x
    | None -> Sys_error (Format.sprintf "Missing environment variable: %s" name) |> raise
  ;;

  let load_credentials_data () =
    let access_id = getenv "AWS_ACCESS_KEY_ID" in
    let access_secret = getenv "AWS_SECRET_ACCESS_KEY" in
    let region = getenv "AWS_REGION" in
    Credentials.make ~access_id ~access_secret ~region:(Region.from_string region) ()
  ;;

  (** Invokes real aws lambda, gets parameters from env vars *)
  let invoke_test () =
    let creds = load_credentials_data () in
    let func_name = getenv "FUNCTION_NAME" in
    let module Lambda = (val Lambda.from_credentials creds) in
    let response = Lambda.invoke ~func_name () |> Lwt_main.run |> Result.get_ok in
    check' int ~msg:"" ~expected:200 ~actual:response.code
  ;;

  let suite = "Lambda", [ test_case "invoke" `Slow invoke_test ]
end

let () = 
  run "IntegrationTests" [ LambdaTests.suite ]
