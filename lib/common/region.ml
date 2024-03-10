type t =
  | AfSouth1 (** Africa (Cape Town) *)
  (* *)
  | ApEast1 (** Asia Pacific (Hong Kong) *)
  (* *)
  | ApNortheast1 (** Asia Pacific (Tokyo) *)
  (* *)
  | ApNortheast2 (** Asia Pacific (Seoul) *)
  (* *)
  | ApNortheast3 (** Asia Pacific (Osaka) *)
  (* *)
  | ApSouth1 (** Asia Pacific (Mumbai) *)
  (* *)
  | ApSouth2 (** Asia Pacific (Hyderabad) *)
  (* *)
  | ApSoutheast1 (** Asia Pacific (Singapore) *)
  (* *)
  | ApSoutheast2 (** Asia Pacific (Sydney) *)
  (* *)
  | ApSoutheast3 (** Asia Pacific (Jakarta) *)
  (* *)
  | ApSoutheast4 (** Asia Pacific (Melbourne) *)
  (* *)
  | CaCentral1 (** Canada (Central) *)
  (* *)
  | CaWest1 (** Canada West (Calgary) *)
  (* *)
  | CnNorth1 (** China (Beijing) *)
  (* *)
  | CnNorthwest1 (** China (Ningxia) *)
  (* *)
  | EuCentral1 (** Europe (Frankfurt) *)
  (* *)
  | EuCentral2 (** Europe (Zurich) *)
  (* *)
  | EuNorth1 (** Europe (Stockholm) *)
  (* *)
  | EuSouth1 (** Europe (Milan) *)
  (* *)
  | EuSouth2 (** Europe (Spain) *)
  (* *)
  | EuWest1 (** Europe (Ireland) *)
  (* *)
  | EuWest2 (** Europe (London) *)
  (* *)
  | EuWest3 (** Europe (Paris) *)
  (* *)
  | IlCentral1 (** Israel (Tel Aviv) *)
  (* *)
  | MeCentral1 (** Middle East (UAE) *)
  (* *)
  | MeSouth1 (** Middle East (Bahrain) *)
  (* *)
  | SaEast1 (** South America (Sao Paulo) *)
  (* *)
  | UsEast1 (** US East (N. Virginia) *)
  (* *)
  | UsEast2 (** US East (Ohio) *)
  (* *)
  | UsGovEast1 (** AWS GovCloud (US-East) *)
  (* *)
  | UsGovWest1 (** AWS GovCloud (US-West) *)
  (* *)
  | UsWest1 (** US West (N. California) *)
  (* *)
  | UsWest2 (** US West (Oregon) *)
  (* *)
  | Other of string (** Custom Region *)
[@@deriving eq]

let show = function
  | AfSouth1 -> "af-south-1"
  | ApEast1 -> "ap-east-1"
  | ApNortheast1 -> "ap-northeast-1"
  | ApNortheast2 -> "ap-northeast-2"
  | ApNortheast3 -> "ap-northeast-3"
  | ApSouth1 -> "ap-south-1"
  | ApSouth2 -> "ap-south-2"
  | ApSoutheast1 -> "ap-southeast-1"
  | ApSoutheast2 -> "ap-southeast-2"
  | ApSoutheast3 -> "ap-southeast-3"
  | ApSoutheast4 -> "ap-southeast-4"
  | CaCentral1 -> "ca-central-1"
  | CaWest1 -> "ca-west-1"
  | CnNorth1 -> "cn-north-1"
  | CnNorthwest1 -> "cn-northwest-1"
  | EuCentral1 -> "eu-central-1"
  | EuCentral2 -> "eu-central-2"
  | EuNorth1 -> "eu-north-1"
  | EuSouth1 -> "eu-south-1"
  | EuSouth2 -> "eu-south-2"
  | EuWest1 -> "eu-west-1"
  | EuWest2 -> "eu-west-2"
  | EuWest3 -> "eu-west-3"
  | IlCentral1 -> "il-central-1"
  | MeCentral1 -> "me-central-1"
  | MeSouth1 -> "me-south-1"
  | SaEast1 -> "sa-east-1"
  | UsEast1 -> "us-east-1"
  | UsEast2 -> "us-east-2"
  | UsGovEast1 -> "us-gov-east-1"
  | UsGovWest1 -> "us-gov-west-1"
  | UsWest1 -> "us-west-1"
  | UsWest2 -> "us-west-2"
  | Other x -> x
;;

let pp fmt x = Format.fprintf fmt "%s" (show x)
