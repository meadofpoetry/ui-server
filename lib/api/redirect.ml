open Authorize
open Interaction
   
let home_page ?headers =
  match headers with
  | Some headers -> (fun () -> respond_redirect ~headers "/" ())
  | None         -> (fun () -> respond_redirect "/" ())
                  
let login_page = respond_redirect "/login"

let not_found = respond_not_found

let error_page ?(status = `Not_found) = function
  | Failure e -> respond_error ~status e ()
  | _         -> respond_error ~status "Unknown error" ()
               
let redirect_auth validate headers request =
  let open Lwt.Infix in
  Lwt.catch 
    (fun () -> auth validate headers)
    (fun _  -> Lwt.return_error `Need_auth)
  >>= function
  | Ok id     ->
     Lwt.catch (fun () -> request id) (fun e -> error_page e)
  | Error #Authorize.error ->
     respond_need_auth ~headers:headers ~auth:(`Basic "User Visible Realm") ()
  (* TODO proper error *)
  | Error _ -> respond_error "Unknown error" ()
(* | Done hd   -> home_page ~headers:hd ()*)

let redirect_if p request =
  if not p
  then request ()
  else Lwt.(>>=) (Lwt_io.printf "redirect_if: not permitted\n") (fun () -> respond_error "Operation not permitted." ())
