(*Example Lambda request
  POST /2015-03-31/functions/test/invocations HTTP/1.1
  Host: lambda.eu-west-2.amazonaws.com
  Accept-Encoding: identity
  User-Agent: aws-cli/2.15.19 Python/3.11.7 Linux/6.7.6-arch1-1 source/x86_64.arch prompt/off command/lambda.invoke
  X-Amz-Date: 20240302T185937Z
  Authorization: AWS4-HMAC-SHA256 Credential=AKIAY6SRDBMYEY6VJETV/20240302/eu-west-2/lambda/aws4_request, SignedHeaders=host;x-amz-date, Signature=ee810415c4cb4bd262f19da69b8de7e6aafe2a416754fcf611c862938e7476bf
  Content-Length: 0
*)

(*Example Lambda response
  HTTP/1.1 200 OK
  Date: Sat, 02 Mar 2024 18:59:37 GMT
  Content-Type: application/json
  Content-Length: 50
  Connection: keep-alive
  x-amzn-RequestId: 3435ca6f-a508-45e3-9a97-1919cd2f6c9a
  x-amzn-Remapped-Content-Length: 0
  X-Amz-Executed-Version: $LATEST
  X-Amzn-Trace-Id: root=1-65e37719-14c1a9161181b6491db573cd;parent=26f0f4f799501463;sampled=0;lineage=b1f874d6:0

  {"statusCode":200,"body":"\"Hello from Lambda!\""}
*)

open! Utils
open Mirage_crypto

let hash_string s =
  let module Sha256 = Hash.SHA256 in
  let cstr = Cstruct.string s in
  let hash = Sha256.empty in
  Sha256.feed hash cstr
;;

let hash_string_hmac ~key s =
  let module Sha256 = Hash.SHA256 in
  let cstr = Cstruct.string s in
  let hash = Sha256.hmac_empty ~key in
  Sha256.hmac_feed hash cstr
;;

let hex_of_hash hash = hash |> Hash.SHA256.get |> Cstruct.to_hex_string
let hex_of_hmac hash = hash |> Hash.SHA256.hmac_get |> Cstruct.to_hex_string
