val home_page :
  ?headers:Cohttp.Header.t ->
  unit ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val login_page :
  ?headers:Cohttp.Header.t ->
  unit ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val not_found :
  ?uri:Netlib.Uri.t -> unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val redirect_auth :
  (name:string -> pass:string -> ('a, [> Api.Authorize.error ]) result Lwt.t) ->
  Api.env ->
  Cohttp.Header.t ->
  ('a -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
