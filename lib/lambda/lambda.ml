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
    -> (Ezcurl_core.response, Error.t) result Lwt.t
end

module Make (C : Config) : Provider = struct
  let service = "lambda"

  let base_url ~region =
    match C.service_url with
    | Some url -> url
    | None -> Format.sprintf "%s.%s.amazonaws.com" service region
  ;;

  let base_region =
    C.credentials
    |> Option.bind ~f:Credentials.region
    |> Option.bind_none ~f:(fun () ->
      Sys.getenv_opt "AWS_REGION" |> Option.map Region.from_string)
    |> Option.bind_none ~f:(fun () ->
      Sys.getenv_opt "AWS_DEFAULT_REGION" |> Option.map Region.from_string)
    |> Option.to_result ~none:(Error.RequestError "No region found")
  ;;

  let credentials =
    C.credentials |> Option.to_result ~none:(Error.RequestError "No credentials found")
  ;;

  let invoke ?payload ?region ~func_name () =
    let open LwtSyntax in
    let$ creds = credentials in
    let$ region =
      match region with
      | Some r -> Ok r
      | None -> base_region
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
    Format.printf "%a" Request.pp req;
    let& response = Request.perform req |> Lwt.map (Result.map_error Error.curlError) in
    return (Ok response)
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
