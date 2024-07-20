module RequestError = struct
  type t =
    { code : int
    ; msg : string
    ; body : string
    }
  [@@deriving show, eq]

  let of_curl_error code body =
    { code = Curl.int_of_curlCode code; msg = Curl.strerror code; body }
  ;;
end

module Invoke = struct
  type t =
    | RequestError of RequestError.t
    | InvokeError of string
    | AwsError of
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
  [@@deriving show, eq]

  let aws_error x = AwsError x
  let request_error x = RequestError x

  let from_json (x : Yojson.Safe.t) : t option =
    let map = x |> Yojson.Safe.Util.to_assoc |> List.to_seq |> Hashtbl.of_seq in
    let typ = Hashtbl.find map "__type" |> Yojson.Safe.Util.to_string in
    let message = Hashtbl.find map "Message" |> Yojson.Safe.Util.to_string in
    (match typ with
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
     | _ -> None)
    |> Option.map aws_error
  ;;
end
