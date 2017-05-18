open Lwt
open Cohttp
open Cohttp_lwt_unix
open Containers
open Api
open User

let (%) = Fun.(%)

type server_settings = { path : string
                       ; port : int
                       }
                     
let home base =
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat base "index.html") ()

let login base =
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat base "login.html") ()

let resource base uri =
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat base uri) ()

let login_redirect = Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/login")

let not_found = Cohttp_lwt_unix.Server.respond_not_found
  
let get_handler ~settings
                ~database
  = let handler
          (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
          (req  : Cohttp_lwt_unix.Request.t)
          (body : Cohttp_lwt_body.t) =
      let headers  = Request.headers req in
      let uri      = Uri.path @@ Request.uri req in
      let meth     = Request.meth req in
      let uri_list = uri
                     |> String.split_on_char '/'
                     |> List.filter (not % String.equal "")
      in match meth, uri_list with
         | `GET, []         -> if   Option.is_none @@ Authorize.auth_needed database headers
                               then login_redirect () else home settings.path
         | `GET, ["login"]  -> login settings.path
         | _, "api" :: path -> Api.handle ~database meth path headers body
         | `GET, path       -> if   Option.is_none @@ Authorize.auth_needed database headers
                               then not_found ()      else resource settings.path uri
         | _                -> not_found ()
    in
    handler
   
let create ~settings
           ~database
  = let handler = get_handler ~settings ~database in 
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port settings.port))
                                  (Cohttp_lwt_unix.Server.make ~callback:handler ())
