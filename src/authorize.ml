open Lwt
open Cohttp
open Cohttp_lwt_unix
open Containers
open User
open Database
   
let user = "root"
let pswd = "pswd"
         
let auth_needed dbs headers =
  let open Option in
  Cookie.Cookie_hdr.extract headers
  |> List.find_pred (fun (name,_) -> name = "auth_token")
  >>= (fun (name, hsh) -> Database.lookup_token dbs hsh)
  >>= (fun tok -> if is_expired tok then None else Some tok)
  >>= function (usr,_) -> Some usr

let auth dbs headers name pass =
  if name = user && pass = pswd
  then begin
      let hsh = User.get_token (User.of_string name)
                |> Database.push_token dbs
      in
      let cookie = Cookie.Set_cookie_hdr.(
          make ~domain:"127.0.0.1" ~path:"/" ("auth_token", hsh)
          |> serialize ~version:`HTTP_1_0)
      in
      let nh = Header.add_list headers [cookie] in
      Server.respond_redirect ~headers:(nh) ~uri:(Uri.with_path Uri.empty "/") ()
    end
  else Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/login") ()
