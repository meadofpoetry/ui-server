open Lwt.Infix

let yojson_of_body body =
  Cohttp_lwt.Body.to_string body
  >|= fun body ->
  Uri.pct_decode body
  |> Yojson.Safe.from_string

let yojson_to_body js =
  Yojson.Safe.to_string js
  |> Uri.pct_encode
  |> Cohttp_lwt.Body.of_string

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

  val to_body : t -> Cohttp_lwt.Body.t

end

module Make(M:Req) = struct

  let respond x = Cohttp_lwt_unix.Server.respond ~status:`OK ~body:(M.to_body x)

  let respond_result = function
    | Ok x    -> Cohttp_lwt_unix.Server.respond ~status:`OK ~body:(M.to_body x) ()
    | Error x -> Cohttp_lwt_unix.Server.respond ~status:`Bad_request ~body:(M.to_body x) ()

  let respond_result_unit = function
    | Ok ()   -> Cohttp_lwt_unix.Server.respond ~status:`OK ~body:Cohttp_lwt.Body.empty ()
    | Error x -> Cohttp_lwt_unix.Server.respond ~status:`Bad_request ~body:(M.to_body x) ()

  let respond_option = function
    | Some x  -> Cohttp_lwt_unix.Server.respond ~status:`OK ~body:(M.to_body x) ()
    | None    -> Cohttp_lwt_unix.Server.respond_not_found ()

end
