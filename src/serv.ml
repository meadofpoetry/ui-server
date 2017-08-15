open Cohttp_lwt_unix
open Containers
open Redirect

let (%) = Fun.(%)

let home base =
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat base "index.html") ()

let resource base uri =
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat base uri) () 

module Settings = struct
  type t = { path : string
           ; port : int
           } [@@deriving yojson]
  let default = { path = Filename.concat Filename.current_dir_name "resources"
                ; port = 7777
                }
  let domain = "server"
end

module Conf = Config.Make(Settings)
  
let get_handler ~settings
                ~auth_filter
                ~routes
  =
  let open Settings in
  let handler
        (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
        (req  : Cohttp_lwt_unix.Request.t)
        (body : Cohttp_lwt_body.t) =
    ignore conn;
    let headers  = Request.headers req in
    let uri      = Uri.path @@ Request.uri req in
    let uri_list = uri
                   |> String.split_on_char '/'
                   |> List.filter (not % String.equal "")
    in
    let meth     = Request.meth req in
    let redir = auth_filter headers in
    match meth, uri_list with
    | `GET, []         -> redir (fun _ -> home settings.path)
    | _, "api" :: path -> Api_handler.handle routes redir meth path headers body
    | `GET, _          -> redir (fun _ -> resource settings.path uri)
    | _                -> not_found ()
  in
  handler
                 
let create config auth_filter routes =
  let settings = Conf.get config in
  let handler  = get_handler ~settings ~auth_filter ~routes in 
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port settings.port))
                                (Cohttp_lwt_unix.Server.make ~callback:handler ())
