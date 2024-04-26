open Common
open Utils
module Error = Error

module type Config = sig
  val service_url : string option
  val credentials : Credentials.t option
end

module type Provider = sig
  val invoke
    :  ?payload:string
    -> ?region:Region.t
    -> func_name:string
    -> unit
    -> (string, Error.Aws.Invoke.t) result Lwt.t
end

module Make (C : Config) : Provider = struct
  let service = "lambda"
  let function_prefix = "/2015-03-31/functions"

  let base_url ~region =
    match C.service_url with
    | Some url -> url
    | None -> [%string "%{service}.%{region}.amazonaws.com"]
  ;;

  let base_region =
    C.credentials
    |> Option.bind ~f:Credentials.region
    |> Option.bind_none ~f:(fun () ->
      [ "AWS_REGION"; "AWS_DEFAULT_REGION" ]
      |> List.find_map Sys.getenv_opt
      |> Option.map Region.from_string)
    |> Option.to_result ~none:(`InvokeError "No region found")
  ;;

  let base_credentials =
    C.credentials |> Option.to_result ~none:(`InvokeError "No credentials found")
  ;;

  let invoke ?payload ?region ~func_name () =
    let open LwtSyntax in
    let$ creds = base_credentials in
    let$ region =
      match region with
      | Some r -> Ok r
      | None -> base_region
    in
    let path = [%string "%{function_prefix}/%{Uri.pct_encode func_name}/invocations"] in
    let uri =
      Uri.make ~scheme:"https" ~host:(base_url ~region:(Region.show region)) ~path ()
    in
    let now = Timedesc.now ~tz_of_date_time:Timedesc.Time_zone.utc () in
    let$ req =
      Request.post_json ~datetime:now ~uri ?body:payload ()
      |> Option.map
           (Request.with_auth_header
              ~datetime:now
              ~access_id:creds.access_id
              ~access_secret:creds.access_secret
              ~region
              ~service)
      |> Option.to_result ~none:(`InvokeError "failed to build request")
    in
    let& response = Request.perform req |> Lwt.map (Result.map_error Error.curlError) in
    let result =
      match response.code with
      | 200 -> Ok response.body
      | _ ->
        response.body
        |> Yojson.Safe.from_string
        |> Error.Aws.Invoke.from_json
        |> (function
         | Some err -> Error err
         | None -> Error (`InvokeError "Unexpected error"))
    in
    return result
  ;;
end

let from_credentials creds =
  let module M =
    Make (struct
      let service_url = None
      let credentials = Some creds
    end)
  in
  (module M : Provider)
;;

module Default = Make (struct
    let service_url = None
    let credentials = Credentials.try_load ()
  end)
