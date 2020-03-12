let respond_redirect path =
  Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty path)

let respond_need_auth = Cohttp_lwt_unix.Server.respond_need_auth

let respond_not_found = Cohttp_lwt_unix.Server.respond_not_found

let respond_error ?(status = `Forbidden) error =
  Cohttp_lwt_unix.Server.respond_error ~status ~body:error

let home_page ?headers =
  match headers with
  | Some headers -> fun () -> respond_redirect ~headers "/" ()
  | None -> fun () -> respond_redirect "/" ()

let login_page = respond_redirect "/login"

let not_found ?(uri : Netlib.Uri.t option) () :
    (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =
  respond_not_found ?uri ()

let error_page ?(status = `Not_found) = function
  | Failure e -> respond_error ~status e ()
  | _ -> respond_error ~status "Unknown error" ()

let redirect_auth validate env headers request =
  let open Lwt.Infix in
  Lwt.catch
    (fun () -> Api.Authorize.auth validate env)
    (fun _ -> Lwt.return_error `Need_auth)
  >>= function
  | Ok id -> Lwt.catch (fun () -> request id) (fun e -> error_page e)
  | Error #Api.Authorize.error ->
      respond_need_auth ~headers ~auth:(`Basic "User Visible Realm") ()
  (* TODO proper error *)
  | Error _ -> respond_error "Unknown error" ()

(* | Done hd   -> home_page ~headers:hd ()*)
