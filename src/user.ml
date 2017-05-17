open Lwt
open Cohttp
open Cohttp_lwt_unix
open Containers

let user = "user"
let pswd = "pswd"
   
let auth_needed headers =
  let open Option in
  Cohttp.Cookie.Cookie_hdr.extract headers
  |> List.find_pred (fun (name,_) -> name = "id")
  |> function
  | None  -> true
  | Some _ -> false (*name = user && pass = pswd*)

let auth headers name pass =
  (* if name = user && pass = pswd then*)
  Lwt_io.print (Header.to_string headers) >>= fun _ ->
  let nh = Header.add_list headers [(Cohttp.Cookie.Set_cookie_hdr.make ~domain:"127.0.0.1" ~path:"/" ("id", "name") |> Cohttp.Cookie.Set_cookie_hdr.serialize ~version:`HTTP_1_0)]
  in Lwt_io.print (Header.to_string nh) >>= fun _ ->
  Server.respond_redirect
    ~headers:(nh)
    ~uri:(Uri.with_path Uri.empty "/")
    ()
 (* else Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/login") ()
  *)
