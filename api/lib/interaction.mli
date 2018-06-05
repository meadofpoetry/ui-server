type response = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

module type Handler = sig
  type t
  val to_body             : t -> Cohttp_lwt.Body.t
  val of_body             : Cohttp_lwt.Body.t -> t Lwt.t
  val of_error_string     : string -> t
  val of_exn              : exn -> t
  val respond             : ?status:Cohttp.Code.status_code -> ?headers:Cohttp.Header.t -> ?flush:bool ->
                            t -> unit -> response
  val respond_result      : ?err_status:Cohttp.Code.status_code -> (t, t) result -> response
  val respond_result_unit : ?err_status:Cohttp.Code.status_code -> (unit,t) result -> response
  val respond_option      : t option -> response
  val (>>=)               : 'a Lwt.t -> ('a -> response) -> response
end

module Json : Handler with type t := Yojson.Safe.json

val respond_file      : string -> string -> ?headers:Cohttp.Header.t -> unit -> response
val respond_not_found : ?uri:Uri.t -> unit -> response
val respond_redirect  : string -> ?headers:Cohttp.Header.t -> unit -> response
val respond_error     : ?status:Cohttp.Code.status_code -> string -> ?headers:Cohttp.Header.t -> unit -> response
val respond_need_auth : ?headers:Cohttp.Header.t -> auth:Cohttp.Auth.challenge -> unit -> response
val respond_html_elt  : ?status:Cohttp.Code.status_code -> 'a Tyxml.Html.elt ->
                        ?headers:Cohttp.Header.t -> ?flush:bool -> unit -> response
val respond_string    : ?status:Cohttp.Code.status_code -> string -> ?flush:bool ->
                        ?headers:Cohttp.Header.t -> unit -> response
