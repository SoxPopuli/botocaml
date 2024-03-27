open Utils
module Ezcurl = Ezcurl_lwt

module Method = struct
  type t =
    | Get
    | Post
    | Put
  [@@deriving eq, show]

  let name = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
  ;;
end

type t =
  { meth : Method.t
  ; uri : Uri.t
  ; headers : (string * string) list
  ; query_params : (string * string list) list
  ; body : string option
  }
[@@deriving show, eq]

let iso8601_minimal dt =
  let iso = Timedesc.to_iso8601 ~frac_s:0 dt in
  StringLabels.fold_left iso ~init:"" ~f:(fun acc ch ->
    match ch with
    | ':' | '-' -> acc
    | _ -> acc ^ String.make 1 ch)
;;

let ymd dt =
  let to_string_padded x =
    let s = Int.to_string x in
    if String.length s = 1 then "0" ^ s else s
  in
  let year = Timedesc.year dt |> Int.to_string in
  let month = Timedesc.month dt |> to_string_padded in
  let day = Timedesc.day dt |> to_string_padded in
  year ^ month ^ day
;;

let now_utc () = Timedesc.now ~tz_of_date_time:Timedesc.Time_zone.utc ()
let canonical_method meth = Method.name meth

let pct_encode_excluding_slash str =
  let module CharMap = Hashtbl.Make (Char) in
  let reserved_chars =
    [ ' ', "%20"
    ; '!', "%21"
    ; '"', "%22"
    ; '#', "%23"
    ; '$', "%24"
    ; '%', "%25"
    ; '&', "%26"
    ; '\'', "%27"
    ; '(', "%28"
    ; ')', "%29"
    ; '*', "%2A"
    ; '+', "%2B"
    ; ',', "%2C"
    ; ':', "%3A"
    ; ';', "%3B"
    ; '=', "%3D"
    ; '?', "%3F"
    ; '@', "%40"
    ; '[', "%5B"
    ; ']', "%5D"
    ]
    |> List.to_seq
    |> CharMap.of_seq
  in
  StringLabels.fold_left str ~init:"" ~f:(fun acc ch ->
    match CharMap.find_opt reserved_chars ch with
    | Some pct -> acc ^ pct
    | None -> acc ^ String.make 1 ch)
;;

let canonical_url uri = uri |> Uri.path |> pct_encode_excluding_slash

let canonical_query params =
  params
  |> List.sort_by_keys String.compare
  |> List.map (fun (k, v) ->
    let key = Uri.pct_encode k in
    let values = v |> List.map Uri.pct_encode |> String.join ~sep:"," in
    key ^ "=" ^ values)
  |> String.join ~sep:"&"
;;

let canonicalize_headers headers =
  headers |> List.map_first String.lowercase_ascii |> List.sort_by_keys String.compare
;;

let canonical_headers headers =
  headers
  |> List.map (fun (k, v) -> k ^ ":" ^ v)
  |> List.fold_left (fun acc str -> acc ^ str ^ "\n") ""
;;

let signed_headers headers = headers |> List.map fst |> String.join ~sep:";"

let canonical_request ~canonical_headers ~signed_headers ~meth ~uri ~query_params ~body =
  let body = body |> Option.value ~default:"" in
  let parts =
    [ canonical_method meth
    ; canonical_url uri
    ; canonical_query query_params
    ; canonical_headers
    ; signed_headers
    ; body |> Auth.hash_string |> Auth.hex_of_hash
    ]
  in
  String.join ~sep:"\n" parts
;;

let build_scope ~date_ymd ~region ~service =
  let region = Region.show region in
  Format.sprintf "%s/%s/%s/aws4_request" date_ymd region service
;;

let string_to_sign ~datetime ~scope ~request =
  let signing_method = "AWS4-HMAC-SHA256" in
  let timestamp = iso8601_minimal datetime in
  let request = request |> Auth.hash_string |> Auth.hex_of_hash in
  [ signing_method; timestamp; scope; request ] |> String.join ~sep:"\n"
;;

let signature ~date_ymd ~access_secret ~region ~service ~string_to_sign =
  let open Mirage_crypto.Hash in
  let date_key =
    Auth.hash_string_hmac ~key:(Cstruct.string @@ "AWS4" ^ access_secret) date_ymd
  in
  let date_region_key =
    Auth.hash_string_hmac ~key:(SHA256.hmac_get date_key) (Region.show region)
  in
  let date_region_service_key =
    Auth.hash_string_hmac ~key:(SHA256.hmac_get date_region_key) service
  in
  let signing_key =
    Auth.hash_string_hmac ~key:(SHA256.hmac_get date_region_service_key) "aws4_request"
  in
  Auth.hash_string_hmac ~key:(SHA256.hmac_get signing_key) string_to_sign
;;

let build_auth_header
  ?(datetime = now_utc ())
  ~access_id
  ~access_secret
  ~region
  ~service
  request
  =
  let headers = request.headers |> canonicalize_headers in
  let canonical_headers = canonical_headers headers in
  let signed_headers = signed_headers headers in
  let canonical_request =
    canonical_request
      ~canonical_headers
      ~signed_headers
      ~meth:request.meth
      ~uri:request.uri
      ~query_params:request.query_params
      ~body:request.body
  in
  let date_ymd = ymd datetime in
  let scope = build_scope ~date_ymd ~region ~service in
  let string_to_sign = string_to_sign ~datetime ~scope ~request:canonical_request in
  let signature =
    signature ~date_ymd ~access_secret ~region ~service ~string_to_sign
    |> Auth.hex_of_hmac
  in
  let signing_method = "AWS4-HMAC-SHA256" in
  let credential = Format.sprintf "%s/%s" access_id scope in
  Format.sprintf
    "%s Credential=%s,SignedHeaders=%s,Signature=%s"
    signing_method
    credential
    signed_headers
    signature
;;

let with_auth_header
  ?(datetime = now_utc ())
  ~access_id
  ~access_secret
  ~region
  ~service
  (req : t)
  =
  let auth_header =
    ( "authorization"
    , build_auth_header ~datetime ~access_id ~access_secret ~region ~service req )
  in
  { req with headers = auth_header :: req.headers }
;;

let perform (req : t) =
  let meth =
    match req.meth with
    | Get -> Ezcurl.GET
    | Post -> Ezcurl.POST []
    | Put -> Ezcurl.PUT
  in
  let content = req.body |> Option.map (fun x -> `String x) in
  let url = Uri.with_query req.uri req.query_params |> Uri.to_string in
  Ezcurl.http ?content ~meth ~headers:req.headers ~url ()
;;

let make
  ?(datetime = now_utc ())
  ?(meth = Method.Get)
  ?(headers = [])
  ?(query_params = [])
  ?body
  ~uri
  ()
  =
  let open OptionSyntax in
  let* host = Uri.host uri in
  let content =
    body |> Option.value ~default:"" |> Auth.hash_string |> Auth.hex_of_hash
  in
  let date = iso8601_minimal datetime in
  let headers =
    let base = [ "host", host; "x-amz-content-sha256", content; "x-amz-date", date ] in
    headers @ base
  in
  let req = { meth; uri; headers; query_params; body } in
  return req
;;

let post_json ?(datetime = now_utc ()) ?(headers = []) ?(query_params = []) ?body ~uri () =
  let headers = ("Content-Type", "application/json") :: headers in
  make ~datetime ~meth:Method.Post ~headers ~query_params ~uri ?body ()
;;
