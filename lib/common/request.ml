open Utils

module Method = struct
  type t =
    | Get
    | Put
  [@@deriving eq]

  let show = function
    | Get -> "GET"
    | Put -> "PUT"
  ;;

  let pp fmt self = Format.fprintf fmt "%s" (show self)
end

type t =
  { meth : Method.t
  ; url : string
  ; headers : (string * string) list
  ; query_params : (string * string list) list
  ; body : string
  }
[@@deriving show, eq]

let canonical_method meth = Method.show meth
let canonical_url url = url |> Uri.of_string |> Uri.path

let canonical_query params =
  params
  |> List.sort_by_keys String.compare
  |> List.map (fun (k, v) ->
    let key = Uri.pct_encode k in
    let values = v |> List.map Uri.pct_encode |> String.join ~sep:"," in
    Format.sprintf "%s=%s" key values)
  |> String.join ~sep:"&"
;;

let canonical_headers headers =
  headers
  |> List.map_first String.lowercase_ascii
  |> List.sort_by_keys String.compare
  |> List.map (fun (k, v) -> Format.sprintf "%s:%s" k (String.trim v))
  |> String.join ~sep:"\n"
;;

let signed_headers headers =
  headers |> List.map (fun (k, _) -> String.lowercase_ascii k) |> String.join ~sep:";"
;;

let hashed_payload payload =
  let open Mirage_crypto.Hash in
  Auth.hash_string payload |> SHA256.get |> Cstruct.to_hex_string
;;

let canonical_request self =
  let headers_to_sign =
    self.headers
    |> List.filter (fun (k, _) ->
      match String.lowercase_ascii k with
      | "host" | "content-type" -> true
      | x when String.contains_string ~haystack:x ~needle:"x-amz" -> true
      | _ -> false)
  in
  let parts =
    [ canonical_method self.meth
    ; canonical_url self.url
    ; canonical_query self.query_params
    ; canonical_headers self.headers
    ; signed_headers headers_to_sign
    ; self.body |> Auth.hash_string |> Auth.hex_of_hash
    ]
  in
  String.join ~sep:"\n" parts
;;
