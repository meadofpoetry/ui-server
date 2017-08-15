open Lwt.Infix

let yojson_of_body body =
  Cohttp_lwt_body.to_string body
  >|= fun body ->
  String.split_on_char '=' body
  |> (fun l -> List.nth l 1)
  |> Uri.pct_decode
  |> Yojson.Safe.from_string

let yojson_to_body js =
  Yojson.Safe.to_string js
  |> Uri.pct_encode
  |> (^) "body = "
  |> Cohttp_lwt_body.of_string

let respond_error ?(status = `Forbidden) error = Cohttp_lwt_unix.Server.respond_error ~status ~body:error

let respond ?(status = `OK) body = Cohttp_lwt_unix.Server.respond ~status ~body

let respond_js ?(status = `OK) js =
  Cohttp_lwt_unix.Server.respond ~status ~body:(yojson_to_body js)

let respond_ok = Cohttp_lwt_unix.Server.respond ~status:`OK ~body:Cohttp_lwt_body.empty                     

let respond_redirect path =
  Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty path)

let respond_not_found = Cohttp_lwt_unix.Server.respond_not_found

let respond_need_auth = Cohttp_lwt_unix.Server.respond_need_auth
