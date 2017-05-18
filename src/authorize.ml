open Lwt
open Cohttp
open Cohttp_lwt_unix
open Containers
open Yojson
open User
open Database

type login_entry = { name     : string
                   ; password : string
                   } [@@deriving yojson]

let login_entry_of_string s =
  let open Result in
  login_entry_of_yojson (Yojson.Safe.from_string s)
  >>= fun le ->
  Ok { name     = User.b64_dec le.name
     ; password = User.b64_dec le.password
     }
         
let auth_needed dbs headers =
  let open Option in
  Cookie.Cookie_hdr.extract headers
  |> List.find_pred (fun (name,_) -> name = "auth_token")
  >>= (fun (name, hsh) -> Database.lookup_token dbs hsh)
  >>= (fun tok -> if is_expired tok then None else Some tok)
  >>= function (usr,_) -> Some usr

let auth dbs headers body =
  let open Result in
  let header =
    login_entry_of_string body
    >>= fun le ->
    if le.name = "root" && le.password = "pswd"
    then begin
        let hsh = User.get_token (User.of_string le.name)
                  |> Database.push_token dbs
        in
        let cookie = Cookie.Set_cookie_hdr.(
            make ~domain:"127.0.0.1" ~path:"/" ("auth_token", hsh)
            |> serialize ~version:`HTTP_1_0)
        in
        Ok (Header.add_list headers [cookie])
      end
    else Error "wrong name or passwd"
  in match header with
     | Ok hd   -> Server.respond_redirect ~headers:(hd) ~uri:(Uri.with_path Uri.empty "/") ()
     | Error e -> Server.respond_error ~status:`Unauthorized ~body:e ()
