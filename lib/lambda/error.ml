type t =
  | HttpError of { code: int; msg: string }
  | RequestError of string

let curlError (code, msg) =
  let code = Curl.int_of_curlCode code in
  HttpError { code; msg }
