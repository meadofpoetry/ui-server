open Lwt.Infix
open Containers

type response = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

let respond_not_found = Cohttp_lwt_unix.Server.respond_not_found

let respond_need_auth = Cohttp_lwt_unix.Server.respond_need_auth

let respond_file base path =
  Cohttp_lwt_unix.Server.respond_file
    ~fname:(Filename.concat base path)

let respond_string ?(status = `OK) body =
  Cohttp_lwt_unix.Server.respond_string ~status ~body

let respond_html_elt ?(status = `OK) body =
  Cohttp_lwt_unix.Server.respond ~status
    ~body:(Cohttp_lwt.Body.of_string
           @@ Format.asprintf "%a" (Tyxml.Html.pp_elt ()) body)

let respond_redirect path =
  Cohttp_lwt_unix.Server.respond_redirect
    ~uri:(Uri.with_path Uri.empty path)

let respond_error ?(status = `Forbidden) error =
  Cohttp_lwt_unix.Server.respond_error ~status ~body:error

module type Req = sig

  type t
  val content_type : string option
  val to_body : t -> Cohttp_lwt.Body.t
  val of_body : Cohttp_lwt.Body.t -> t Lwt.t
  val of_error_string : string -> t
  val of_exn : exn -> t

end

module Json_req : Req with type t = Yojson.Safe.json = struct

  type t = Yojson.Safe.json

  let content_type = Some "application/json"
  let to_body (t : t) = Yojson.Safe.to_string t |> Cohttp_lwt.Body.of_string
  let of_body (b : Cohttp_lwt.Body.t) =
    Cohttp_lwt.Body.to_string b
    >|= (fun body ->
      Uri.pct_decode body
      |> Yojson.Safe.from_string)
  let of_error_string (s : string) = `String s
  let of_exn (e : exn) =
    Printexc.to_string e
    |> fun x -> `String x

end

module type Handler = sig
  type t
  val to_body : t -> Cohttp_lwt.Body.t
  val of_body : Cohttp_lwt.Body.t -> t Lwt.t
  val of_error_string : string -> t
  val of_exn : exn -> t
  val respond : ?headers:Cohttp.Header.t ->
                ?flush:bool ->
                ?status:Cohttp.Code.status_code ->
                t ->
                unit -> response
  val respond_result : ?err_status:Cohttp.Code.status_code -> (t, t) result -> response
  val respond_result_unit : ?err_status:Cohttp.Code.status_code -> (unit,t) result -> response
  val ( >>= ) : 'a Lwt.t -> ('a -> response) -> response
end

module Make(M : Req) : (Handler with type t := M.t) = struct

  let to_body = M.to_body
  let of_body = M.of_body
  let of_error_string = M.of_error_string
  let of_exn  = M.of_exn

  let respond' ?headers ?flush ?(status = `OK) ~body () =
    let headers = match M.content_type with
      | Some s -> Some (Cohttp.Header.add_opt_unless_exists headers "Content-Type" s)
      | None -> None
    in Cohttp_lwt_unix.Server.respond ?headers ?flush ~status ~body ()

  let respond ?headers ?flush ?status x () =
    respond' ?headers ?flush ?status ~body:(to_body x) ()

  let respond_result ?(err_status=`Bad_request) = function
    | Ok x -> respond x ()
    | Error x -> respond ~status:err_status x ()

  let respond_result_unit ?(err_status=`Bad_request) = function
    | Ok () -> respond' ~body:Cohttp_lwt.Body.empty ()
    | Error x -> respond ~status:err_status x ()

  let ( >>= ) t f =
    Lwt.try_bind (fun () -> t) f
      (fun exn ->
        M.of_exn exn
        |> Result.fail
        |> respond_result)

end

module Json : Handler with type t := Yojson.Safe.json = Make(Json_req)
