module Method : sig
  type t =
    | Get
    | Post
    | Put

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val name : t -> string
end

type t

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool

val make
  :  ?datetime:Timedesc.t
  -> ?meth:Method.t
  -> ?headers:(string * string) list
  -> ?query_params:(string * string list) list
  -> ?body:string
  -> uri:Uri.t
  -> unit
  -> t option

val post_json
  :  ?datetime:Timedesc.t
  -> ?headers:(string * string) list
  -> ?query_params:(string * string list) list
  -> ?body:string
  -> uri:Uri.t
  -> unit
  -> t option

val build_auth_header
  :  ?datetime:Timedesc.t
  -> access_id:string
  -> access_secret:string
  -> region:Region.t
  -> service:string
  -> t
  -> string

val with_auth_header
  :  ?datetime:Timedesc.t
  -> access_id:string
  -> access_secret:string
  -> region:Region.t
  -> service:string
  -> t
  -> t

val perform : t -> (Ezcurl_core.response, Curl.curlCode * string) result Lwt.t
