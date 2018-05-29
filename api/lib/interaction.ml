open Lwt.Infix
open Containers

type response = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

let respond_not_found = Cohttp_lwt_unix.Server.respond_not_found

let respond_need_auth = Cohttp_lwt_unix.Server.respond_need_auth

let respond_file base path =
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat base path)

let respond_string ?(status = `OK) body =
  Cohttp_lwt_unix.Server.respond_string ~status ~body

let respond_html_elt ?(status = `OK) body =
  Cohttp_lwt_unix.Server.respond ~status
    ~body:(Cohttp_lwt.Body.of_string @@ Format.asprintf "%a" (Tyxml.Html.pp_elt ()) body)

let respond_redirect path =
  Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty path)

let respond_error ?(status = `Forbidden) error = Cohttp_lwt_unix.Server.respond_error ~status ~body:error

module type Req = sig

  type t
  val to_body         : t -> Cohttp_lwt.Body.t
  val of_body         : Cohttp_lwt.Body.t -> t Lwt.t
  val of_error_string : string -> t
  val of_exn          : exn -> t

end

module Json_req : Req with type t = Yojson.Safe.json = struct

  type t = Yojson.Safe.json

  let to_body (t:t) = Yojson.Safe.to_string t |> Uri.pct_encode |> Cohttp_lwt.Body.of_string
  let of_body (b:Cohttp_lwt.Body.t) = Cohttp_lwt.Body.to_string b >|= fun body ->
                                      Uri.pct_decode body |> Yojson.Safe.from_string
  let of_error_string (s:string) = `String s
  let of_exn (e:exn) = Printexc.to_string e |> fun x -> `String x

end

module type Handler = sig
  include Req
  val respond             : ?status:Cohttp.Code.status_code -> ?headers:Cohttp.Header.t -> ?flush:bool ->
                            t -> unit -> response
  val respond_result      : ?err_status:Cohttp.Code.status_code -> (t, t) result -> response
  val respond_result_unit : ?err_status:Cohttp.Code.status_code -> (unit,t) result -> response
  val respond_option      : t option -> response
  val (>>=)               : 'a Lwt.t -> ('a -> response) -> response
end

module Make(M:Req) : (Handler with type t := M.t) = struct

  include M

  let respond ?(status=`OK) ?headers ?flush x =
    Cohttp_lwt_unix.Server.respond ~status ?headers ?flush ~body:(to_body x)

  let respond_result ?(err_status=`Bad_request) = function
    | Ok x    -> Cohttp_lwt_unix.Server.respond ~status:`OK ~body:(to_body x) ()
    | Error x -> Cohttp_lwt_unix.Server.respond ~status:err_status ~body:(to_body x) ()

  let respond_result_unit ?(err_status=`Bad_request) = function
    | Ok ()   -> Cohttp_lwt_unix.Server.respond ~status:`OK ~body:Cohttp_lwt.Body.empty ()
    | Error x -> Cohttp_lwt_unix.Server.respond ~status:err_status ~body:(to_body x) ()

  let respond_option = function
    | Some x  -> Cohttp_lwt_unix.Server.respond ~status:`OK ~body:(to_body x) ()
    | None    -> Cohttp_lwt_unix.Server.respond_not_found ()

  let (>>=) t f = Lwt.try_bind (fun () -> t) f (fun exn -> M.of_exn exn |> Result.fail |> respond_result)

end

module Json : Handler with type t := Yojson.Safe.json = Make(Json_req)
