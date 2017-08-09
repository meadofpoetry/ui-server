open Cohttp_lwt_unix
open Containers
open Redirect
open Config
open Database
open Pipeline

let (%) = Fun.(%)

type settings = { path : string
                ; port : int
                } [@@deriving yojson]

                     
let settings_default = { path = Filename.concat Filename.current_dir_name "resources"
                       ; port = 7777
                       }

let domain = "server"
                     
let home base =
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat base "index.html") ()

let resource base uri =
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat base uri) () 

module type SERV = sig
  type config
  type db
  type pipe
  val  create   : config -> db -> pipe -> unit Lwt.t
end

module Make (C : CONFIG) (Db : DATABASE) (P : PIPELINE)
       : (SERV with type config = C.t and type db = Db.t and type pipe = P.t) = struct

  type config = C.t
  type db     = Db.t
  type pipe   = P.t
  
  let get_handler ~settings
                  ~database
    =
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
      let redir = redirect_auth database headers in
      match meth, uri_list with
      | `GET, []         -> redir (fun _ -> home settings.path)
      | _, "api" :: path -> Api_handler.handle ~database meth path headers body
      | `GET, _          -> redir (fun _ -> resource settings.path uri)
      | _                -> not_found ()
    in
    handler

  let get_settings = C.get settings_of_yojson domain settings_default 
    
  let create config db _ =
    let cfg     = get_settings config in
    let handler = get_handler ~settings:cfg ~database:db in 
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port cfg.port))
                                  (Cohttp_lwt_unix.Server.make ~callback:handler ())

end
