open Common
open Utils
module Error = Error

let service = "lambda"
let base_url ~region = Format.sprintf "%s.%s.amazonaws.com" service region

let region (creds : Credentials.t) =
  creds.region
  |> Option.bind_none ~f:(fun () ->
    Sys.getenv_opt "AWS_REGION" |> Option.map Region.from_string)
  |> Option.bind_none ~f:(fun () ->
    Sys.getenv_opt "AWS_DEFAULT_REGION" |> Option.map Region.from_string)
;;

let invoke ?(creds = Credentials.try_load ()) ?payload ~func_name () =
  let open LwtSyntax in
  let$ creds =
    creds |> Option.to_result ~none:(Error.RequestError "Failed to load credentials")
  in
  let$ region =
    region creds |> Option.to_result ~none:(Error.RequestError "No region provided")
  in
  let path =
    Format.sprintf "/2015-03-31/functions/%s/invocations" (Uri.pct_encode func_name)
  in
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
    |> Option.to_result ~none:(Error.RequestError "failed to build request")
  in
  let& response = Request.perform req |> Lwt.map (Result.map_error Error.curlError) in
  return (Ok response)
;;
