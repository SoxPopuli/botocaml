open Common
open Utils
module Error = Error

module Config = struct
  type t =
    { service_url : Uri.t option
    ; credentials : Credentials.t option
    ; region : Region.t option
    }
  [@@deriving show, eq]

  let make ?service_url ?credentials ?region () = { service_url; credentials; region }

  let default () =
    { service_url = None; credentials = Credentials.try_load (); region = None }
  ;;

  let region config =
    match config.region with
    | Some r -> Some r
    | None -> config.credentials |> Option.bind ~f:Credentials.region
  ;;
end

let service = "lambda"
let function_prefix = "/2015-03-31/functions"

let service_url ?region ~func_name service_url =
  let path = [%string "%{function_prefix}/%{Uri.pct_encode func_name}/invocations"] in
  match service_url with
  | Some url -> path |> Uri.with_path url
  | None ->
    let region = region |> Option.value ~default:Region.UsEast1 |> Region.show in
    let host = [%string "%{service}.%{region}.amazonaws.com"] in
    Uri.make ~scheme:"https" ~host ~path ()
;;

let invoke ?now ?payload ~(config : Config.t) ~func_name () =
  let open LwtSyntax in
  let$ creds =
    config.credentials |> Option.to_result ~none:(`InvokeError "no credentials provided")
  in
  let$ region =
    Config.region config |> Option.to_result ~none:(`InvokeError "no region provided")
  in
  let uri = service_url ~region ~func_name config.service_url in
  let now =
    now |> Option.value ~default:(Timedesc.now ~tz_of_date_time:Timedesc.Time_zone.utc ())
  in
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
      |> tee print_endline
      |> Yojson.Safe.from_string
      |> Error.Aws.Invoke.from_json
      |> (function
       | Some err -> Error err
       | None -> Error (`InvokeError "Unexpected error"))
  in
  return result
;;
