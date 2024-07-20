type curl_error =
  { code : int
  ; typ : string
  ; msg : string
  }
[@@deriving show, eq]

let string_of_curlCode (code : Curl.curlCode) = Curl.strerror code

let curlError (code, msg) =
  let typ = string_of_curlCode code in
  let code = Curl.int_of_curlCode code in
  `CurlError { code; typ; msg }
;;

module Aws = struct
  let get_type_and_message (x : Yojson.Safe.t) =
    let open Utils in
    let open Utils.OptionSyntax in
    let module StringMap = Map.Make (String) in
    let is_json_string (x : Yojson.Safe.t) =
      match x with
      | `String x -> Some x
      | _ -> None
    in
    match x with
    | `Assoc x ->
      let map = StringMap.of_list x in
      let* type_ =
        map
        |> StringMap.find_with_fallback ~keys:[ "Type"; "type" ]
        |> Option.bind ~f:is_json_string
      in
      let* message =
        map
        |> StringMap.find_with_fallback ~keys:[ "Message"; "message" ]
        |> Option.bind ~f:is_json_string
      in
      Some (type_, message)
    | _ -> None
  ;;

  type t =
    [ `Service of string
    | `ResourceNotFound of string
    | `InvalidRequestContent of string
    | `RequestTooLarge of string
    | `UnsupportedMediaType of string
    | `TooManyRequests of string
    | `InvalidParameterValue of string
    | `EC2Unexpected of string
    | `SubnetIPAddressLimitReached of string
    | `ENILimitReached of string
    | `EFSMountConnectivity of string
    | `EFSMountFailure of string
    | `EFSMountTimeout of string
    | `EFSIO of string
    | `SnapStart of string
    | `SnapStartTimeout of string
    | `SnapStartNotReady of string
    | `EC2Throttled of string
    | `EC2AccessDenied of string
    | `InvalidSubnetID of string
    | `InvalidSecurityGroupID of string
    | `InvalidZipFile of string
    | `KMSDisabled of string
    | `KMSInvalidState of string
    | `KMSAccessDenied of string
    | `KMSNotFound of string
    | `InvalidRuntime of string
    | `ResourceConflict of string
    | `ResourceNotReady of string
    | `RecursiveInvocation of string
    ]

  let name (x : t) =
    match x with
    | `Service _ -> "Service"
    | `ResourceNotFound _ -> "ResourceNotFound"
    | `InvalidRequestContent _ -> "InvalidRequestContent"
    | `RequestTooLarge _ -> "RequestTooLarge"
    | `UnsupportedMediaType _ -> "UnsupportedMediaType"
    | `TooManyRequests _ -> "TooManyRequests"
    | `InvalidParameterValue _ -> "InvalidParameterValue"
    | `EC2Unexpected _ -> "EC2Unexpected"
    | `SubnetIPAddressLimitReached _ -> "SubnetIPAddressLimitReached"
    | `ENILimitReached _ -> "ENILimitReached"
    | `EFSMountConnectivity _ -> "EFSMountConnectivity"
    | `EFSMountFailure _ -> "EFSMountFailure"
    | `EFSMountTimeout _ -> "EFSMountTimeout"
    | `EFSIO _ -> "EFSIO"
    | `SnapStart _ -> "SnapStart"
    | `SnapStartTimeout _ -> "SnapStartTimeout"
    | `SnapStartNotReady _ -> "SnapStartNotReady"
    | `EC2Throttled _ -> "EC2Throttled"
    | `EC2AccessDenied _ -> "EC2AccessDenied"
    | `InvalidSubnetID _ -> "InvalidSubnetID"
    | `InvalidSecurityGroupID _ -> "InvalidSecurityGroupID"
    | `InvalidZipFile _ -> "InvalidZipFile"
    | `KMSDisabled _ -> "KMSDisabled"
    | `KMSInvalidState _ -> "KMSInvalidState"
    | `KMSAccessDenied _ -> "KMSAccessDenied"
    | `KMSNotFound _ -> "KMSNotFound"
    | `InvalidRuntime _ -> "InvalidRuntime"
    | `ResourceConflict _ -> "ResourceConflict"
    | `ResourceNotReady _ -> "ResourceNotReady"
    | `RecursiveInvocation _ -> "RecursiveInvocation"
  ;;

  let message (x : t) =
    match x with
    | `Service x -> x
    | `ResourceNotFound x -> x
    | `InvalidRequestContent x -> x
    | `RequestTooLarge x -> x
    | `UnsupportedMediaType x -> x
    | `TooManyRequests x -> x
    | `InvalidParameterValue x -> x
    | `EC2Unexpected x -> x
    | `SubnetIPAddressLimitReached x -> x
    | `ENILimitReached x -> x
    | `EFSMountConnectivity x -> x
    | `EFSMountFailure x -> x
    | `EFSMountTimeout x -> x
    | `EFSIO x -> x
    | `SnapStart x -> x
    | `SnapStartTimeout x -> x
    | `SnapStartNotReady x -> x
    | `EC2Throttled x -> x
    | `EC2AccessDenied x -> x
    | `InvalidSubnetID x -> x
    | `InvalidSecurityGroupID x -> x
    | `InvalidZipFile x -> x
    | `KMSDisabled x -> x
    | `KMSInvalidState x -> x
    | `KMSAccessDenied x -> x
    | `KMSNotFound x -> x
    | `InvalidRuntime x -> x
    | `ResourceConflict x -> x
    | `ResourceNotReady x -> x
    | `RecursiveInvocation x -> x
  ;;

  module Invoke = struct
    type t =
      [ `Service of string
      | `ResourceNotFound of string
      | `InvalidRequestContent of string
      | `RequestTooLarge of string
      | `UnsupportedMediaType of string
      | `TooManyRequests of string
      | `InvalidParameterValue of string
      | `EC2Unexpected of string
      | `SubnetIPAddressLimitReached of string
      | `ENILimitReached of string
      | `EFSMountConnectivity of string
      | `EFSMountFailure of string
      | `EFSMountTimeout of string
      | `EFSIO of string
      | `SnapStart of string
      | `SnapStartTimeout of string
      | `SnapStartNotReady of string
      | `EC2Throttled of string
      | `EC2AccessDenied of string
      | `InvalidSubnetID of string
      | `InvalidSecurityGroupID of string
      | `InvalidZipFile of string
      | `KMSDisabled of string
      | `KMSInvalidState of string
      | `KMSAccessDenied of string
      | `KMSNotFound of string
      | `InvalidRuntime of string
      | `ResourceConflict of string
      | `ResourceNotReady of string
      | `RecursiveInvocation of string
      | `InvokeError of string
      | `CurlError of curl_error
      ]
    [@@deriving variants, show, eq]

    let from_json (x : Yojson.Safe.t) : t option =
      let map = x |> Yojson.Safe.Util.to_assoc |> List.to_seq |> Hashtbl.of_seq in
      let typ = Hashtbl.find map "__type" |> Yojson.Safe.Util.to_string in
      let message = Hashtbl.find map "Message" |> Yojson.Safe.Util.to_string in
      match typ with
      | "ServiceException" -> Some (`Service message)
      | "ResourceNotFoundException" -> Some (`ResourceNotFound message)
      | "InvalidRequestContentException" -> Some (`InvalidRequestContent message)
      | "RequestTooLargeException" -> Some (`RequestTooLarge message)
      | "UnsupportedMediaTypeException" -> Some (`UnsupportedMediaType message)
      | "TooManyRequestsException" -> Some (`TooManyRequests message)
      | "InvalidParameterValueException" -> Some (`InvalidParameterValue message)
      | "EC2UnexpectedException" -> Some (`EC2Unexpected message)
      | "SubnetIPAddressLimitReachedException" ->
        Some (`SubnetIPAddressLimitReached message)
      | "ENILimitReachedException" -> Some (`ENILimitReached message)
      | "EFSMountConnectivityException" -> Some (`EFSMountConnectivity message)
      | "EFSMountFailureException" -> Some (`EFSMountFailure message)
      | "EFSMountTimeoutException" -> Some (`EFSMountTimeout message)
      | "EFSIOException" -> Some (`EFSIO message)
      | "SnapStartException" -> Some (`SnapStart message)
      | "SnapStartTimeoutException" -> Some (`SnapStartTimeout message)
      | "SnapStartNotReadyException" -> Some (`SnapStartNotReady message)
      | "EC2ThrottledException" -> Some (`EC2Throttled message)
      | "EC2AccessDeniedException" -> Some (`EC2AccessDenied message)
      | "InvalidSubnetIDException" -> Some (`InvalidSubnetID message)
      | "InvalidSecurityGroupIDException" -> Some (`InvalidSecurityGroupID message)
      | "InvalidZipFileException" -> Some (`InvalidZipFile message)
      | "KMSDisabledException" -> Some (`KMSDisabled message)
      | "KMSInvalidStateException" -> Some (`KMSInvalidState message)
      | "KMSAccessDeniedException" -> Some (`KMSAccessDenied message)
      | "KMSNotFoundException" -> Some (`KMSNotFound message)
      | "InvalidRuntimeException" -> Some (`InvalidRuntime message)
      | "ResourceConflictException" -> Some (`ResourceConflict message)
      | "ResourceNotReadyException" -> Some (`ResourceNotReady message)
      | "RecursiveInvocationException" -> Some (`RecursiveInvocation message)
      | _ -> None
    ;;
  end
end

type t =
  | HttpError of
      { code : int
      ; typ : string
      ; msg : string
      }
  | RequestError of string
  | AwsError of Aws.t
