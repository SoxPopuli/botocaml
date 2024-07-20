open Common
module Error = Error

module Config : sig
  type t =
    { service_url : Uri.t option
    ; credentials : Credentials.t option
    ; region : Region.t option
    }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool

  val make
    :  ?service_url:Uri.t
    -> ?credentials:Credentials.t
    -> ?region:Region.t
    -> unit
    -> t

  val default : unit -> t
  val region : t -> Region.t option
end

val invoke
  :  ?now:Timedesc.t
  -> ?payload:string
  -> config:Config.t
  -> func_name:string
  -> unit
  -> (string, Error.Invoke.t) result Lwt.t
