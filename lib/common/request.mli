module Method : sig
  type t =
    | Get
    | Post of string
    | Put of string

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val name : t -> string
  val payload : t -> string option
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
  -> url:string
  -> unit
  -> t option

val post_json
  :  ?datetime:Timedesc.t
  -> ?headers:(string * string) list
  -> ?query_params:(string * string list) list
  -> url:string
  -> string
  -> t option

val build_auth_header
  :  ?datetime:Timedesc.t
  -> access_id:string
  -> access_secret:string
  -> region:Region.t
  -> service:string
  -> request:t
  -> unit
  -> string
