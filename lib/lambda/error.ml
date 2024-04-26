type curl_error =
  { code : int
  ; typ : string
  ; msg : string
  }

let string_of_curlCode (code : Curl.curlCode) =
  match code with
  | Curl.CURLE_OK -> "OK"
  | Curl.CURLE_UNSUPPORTED_PROTOCOL -> "UNSUPPORTED_PROTOCOL"
  | Curl.CURLE_FAILED_INIT -> "FAILED_INIT"
  | Curl.CURLE_URL_MALFORMAT -> "URL_MALFORMAT"
  | Curl.CURLE_URL_MALFORMAT_USER -> "URL_MALFORMAT_USER"
  | Curl.CURLE_COULDNT_RESOLVE_PROXY -> "COULDNT_RESOLVE_PROXY"
  | Curl.CURLE_COULDNT_RESOLVE_HOST -> "COULDNT_RESOLVE_HOST"
  | Curl.CURLE_COULDNT_CONNECT -> "COULDNT_CONNECT"
  | Curl.CURLE_FTP_WEIRD_SERVER_REPLY -> "QFTP_WEIRD_SERVER_REPLY"
  | Curl.CURLE_FTP_ACCESS_DENIED -> "QFTP_ACCESS_DENIED"
  | Curl.CURLE_FTP_USER_PASSWORD_INCORRECT -> "QFTP_USER_PASSWORD_INCORRECT"
  | Curl.CURLE_FTP_WEIRD_PASS_REPLY -> "QFTP_WEIRD_PASS_REPLY"
  | Curl.CURLE_FTP_WEIRD_USER_REPLY -> "QQFTP_WEIRD_USER_REPLY"
  | Curl.CURLE_FTP_WEIRD_PASV_REPLY -> "QFTP_WEIRD_PASV_REPLY"
  | Curl.CURLE_FTP_WEIRD_227_FORMAT -> "QFTP_WEIRD_227_FORMAT"
  | Curl.CURLE_FTP_CANT_GET_HOST -> "QFTP_CANT_GET_HOST"
  | Curl.CURLE_FTP_CANT_RECONNECT -> "QFTP_CANT_RECONNECT"
  | Curl.CURLE_FTP_COULDNT_SET_BINARY -> "QFTP_COULDNT_SET_BINARY"
  | Curl.CURLE_PARTIAL_FILE -> "QPARTIAL_FILE"
  | Curl.CURLE_FTP_COULDNT_RETR_FILE -> "QFTP_COULDNT_RETR_FILE"
  | Curl.CURLE_FTP_WRITE_ERROR -> "QFTP_WRITE_ERROR"
  | Curl.CURLE_FTP_QUOTE_ERROR -> "QQFTP_QUOTE_ERROR"
  | Curl.CURLE_HTTP_NOT_FOUND -> "QHTTP_NOT_FOUND"
  | Curl.CURLE_WRITE_ERROR -> "QWRITE_ERROR"
  | Curl.CURLE_MALFORMAT_USER -> "MALFORMAT_USER"
  | Curl.CURLE_FTP_COULDNT_STOR_FILE -> "FTP_COULDNT_STOR_FILE"
  | Curl.CURLE_READ_ERROR -> "READ_ERROR"
  | Curl.CURLE_OUT_OF_MEMORY -> "OUT_OF_MEMORY"
  | Curl.CURLE_OPERATION_TIMEOUTED -> "OPERATION_TIMEOUTED"
  | Curl.CURLE_FTP_COULDNT_SET_ASCII -> "FTP_COULDNT_SET_ASCII"
  | Curl.CURLE_FTP_PORT_FAILED -> "FTP_PORT_FAILED"
  | Curl.CURLE_FTP_COULDNT_USE_REST -> "FTP_COULDNT_USE_REST"
  | Curl.CURLE_FTP_COULDNT_GET_SIZE -> "FTP_COULDNT_GET_SIZE"
  | Curl.CURLE_HTTP_RANGE_ERROR -> "HTTP_RANGE_ERROR"
  | Curl.CURLE_HTTP_POST_ERROR -> "HTTP_POST_ERROR"
  | Curl.CURLE_SSL_CONNECT_ERROR -> "SSL_CONNECT_ERROR"
  | Curl.CURLE_FTP_BAD_DOWNLOAD_RESUME -> "FTP_BAD_DOWNLOAD_RESUME"
  | Curl.CURLE_FILE_COULDNT_READ_FILE -> "FILE_COULDNT_READ_FILE"
  | Curl.CURLE_LDAP_CANNOT_BIND -> "LDAP_CANNOT_BIND"
  | Curl.CURLE_LDAP_SEARCH_FAILED -> "LDAP_SEARCH_FAILED"
  | Curl.CURLE_LIBRARY_NOT_FOUND -> "LIBRARY_NOT_FOUND"
  | Curl.CURLE_FUNCTION_NOT_FOUND -> "FUNCTION_NOT_FOUND"
  | Curl.CURLE_ABORTED_BY_CALLBACK -> "ABORTED_BY_CALLBACK"
  | Curl.CURLE_BAD_FUNCTION_ARGUMENT -> "BAD_FUNCTION_ARGUMENT"
  | Curl.CURLE_BAD_CALLING_ORDER -> "BAD_CALLING_ORDER"
  | Curl.CURLE_HTTP_PORT_FAILED -> "HTTP_PORT_FAILED"
  | Curl.CURLE_BAD_PASSWORD_ENTERED -> "BAD_PASSWORD_ENTERED"
  | Curl.CURLE_TOO_MANY_REDIRECTS -> "TOO_MANY_REDIRECTS"
  | Curl.CURLE_UNKNOWN_TELNET_OPTION -> "UNKNOWN_TELNET_OPTION"
  | Curl.CURLE_TELNET_OPTION_SYNTAX -> "TELNET_OPTION_SYNTAX"
  | Curl.CURLE_OBSOLETE -> "OBSOLETE"
  | Curl.CURLE_SSL_PEER_CERTIFICATE -> "SSL_PEER_CERTIFICATE"
  | Curl.CURLE_GOT_NOTHING -> "GOT_NOTHING"
  | Curl.CURLE_SSL_ENGINE_NOTFOUND -> "SSL_ENGINE_NOTFOUND"
  | Curl.CURLE_SSL_ENGINE_SETFAILED -> "SSL_ENGINE_SETFAILED"
  | Curl.CURLE_SEND_ERROR -> "SEND_ERROR"
  | Curl.CURLE_RECV_ERROR -> "RECV_ERROR"
  | Curl.CURLE_SHARE_IN_USE -> "SHARE_IN_USE"
  | Curl.CURLE_SSL_CERTPROBLEM -> "SSL_CERTPROBLEM"
  | Curl.CURLE_SSL_CIPHER -> "SSL_CIPHER"
  | Curl.CURLE_SSL_CACERT -> "SSL_CACERT"
  | Curl.CURLE_BAD_CONTENT_ENCODING -> "BAD_CONTENT_ENCODING"
  | Curl.CURLE_LDAP_INVALID_URL -> "LDAP_INVALID_URL"
  | Curl.CURLE_FILESIZE_EXCEEDED -> "FILESIZE_EXCEEDED"
  | Curl.CURLE_USE_SSL_FAILED -> "USE_SSL_FAILED"
  | Curl.CURLE_SEND_FAIL_REWIND -> "SEND_FAIL_REWIND"
  | Curl.CURLE_SSL_ENGINE_INITFAILED -> "SSL_ENGINE_INITFAILED"
  | Curl.CURLE_LOGIN_DENIED -> "LOGIN_DENIED"
  | Curl.CURLE_TFTP_NOTFOUND -> "TFTP_NOTFOUND"
  | Curl.CURLE_TFTP_PERM -> "TFTP_PERM"
  | Curl.CURLE_REMOTE_DISK_FULL -> "REMOTE_DISK_FULL"
  | Curl.CURLE_TFTP_ILLEGAL -> "TFTP_ILLEGAL"
  | Curl.CURLE_TFTP_UNKNOWNID -> "TFTP_UNKNOWNID"
  | Curl.CURLE_REMOTE_FILE_EXISTS -> "REMOTE_FILE_EXISTS"
  | Curl.CURLE_TFTP_NOSUCHUSER -> "TFTP_NOSUCHUSER"
  | Curl.CURLE_CONV_FAILED -> "CONV_FAILED"
  | Curl.CURLE_CONV_REQD -> "CONV_REQD"
  | Curl.CURLE_SSL_CACERT_BADFILE -> "SSL_CACERT_BADFILE"
  | Curl.CURLE_REMOTE_FILE_NOT_FOUND -> "REMOTE_FILE_NOT_FOUND"
  | Curl.CURLE_SSH -> "SSH"
  | Curl.CURLE_SSL_SHUTDOWN_FAILED -> "SSL_SHUTDOWN_FAILED"
  | Curl.CURLE_AGAIN -> "AGAIN"
;;

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
    [@@deriving variants]

    let from_json (x : Yojson.Safe.t) : t option =
      let open Utils.OptionSyntax in
      let* type_, message = get_type_and_message x in
      match type_ with
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
