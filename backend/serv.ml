open Containers
open Cohttp_lwt_unix
open Common.User
open Api.Redirect
open Api.Interaction
open Api.Template
open Api

module Api_handler = Api.Handler.Make(Common.User)
   
let (%) = Fun.(%)

let resource base uri =
  respond_file base uri () 

module Settings = struct
  type t = { path : string
           ; port : int
           } [@@deriving yojson]
  let default = { path = Filename.concat Filename.current_dir_name "resources"
                ; port = 7777
                }
  let domain = "server"
end

module Conf = Storage.Config.Make(Settings)
            
let get_handler ~settings
                ~auth_filter
                ~routes
                ~pages
  =
  let open Settings in
  let handler
        (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
        (req  : Cohttp_lwt_unix.Request.t)
        (body : Cohttp_lwt.Body.t) =
    let headers  = Request.headers req in
    let uri      = Request.uri req in
    let sep_uri  = Common.Uri.sep uri in
    let uri_string = Uri.path uri in
    let respond_page path id =
      let tbl = match id with
        | `Root     -> pages.root
        | `Operator -> pages.operator
        | `Guest    -> pages.guest
      in (try Hashtbl.find tbl (String.concat "/" path)
              |> fun page -> respond_string page ()
          with _ -> resource settings.path uri_string)
    in
    let meth      = Request.meth req in
    let redir     = auth_filter headers in
    let sock_data = (req, (fst conn)) in
    match meth, (Common.Uri.sep_path sep_uri) with
    | _, "api" :: path          -> Api_handler.handle routes redir meth (Common.Uri.upgrade_path sep_uri path) sock_data headers body
    | `GET, path                -> redir (respond_page path)
    | _                         -> not_found ()
  in
  handler

let create config auth_filter routes templates =
  let settings = Conf.get config in
  let tmpl     = Filename.concat settings.path "html/templates/base.html"
                 |> Containers.IO.File.read_exn (* FIXME *) in
  let pages    = Common.User.map_table
                   (fun u ts -> Api.Template.build_route_table tmpl (Common.User.to_string u) ts)
                   templates
  in
  let handler  = get_handler ~settings ~auth_filter ~routes ~pages in
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port settings.port))
                                (Cohttp_lwt_unix.Server.make ~callback:handler ())
