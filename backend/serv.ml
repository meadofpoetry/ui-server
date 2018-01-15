open Cohttp_lwt_unix
open Containers
open Api.Redirect
open Api.Interaction
open Api.Template

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
    ignore conn;
    let headers  = Request.headers req in
    let uri      = Uri.path @@ Request.uri req in
    let uri_list = uri
                   |> String.split_on_char '/'
                   |> List.filter (not % String.equal "")
    in
    let tmpl = Filename.concat settings.path "html/templates/base.html"
               |> CCIO.File.read_exn (* FIXME *) in
    let pages     = Api.Template.build_root_table tmpl pages in
    let meth      = Request.meth req in
    let redir     = auth_filter headers in
    let sock_data = (req, (fst conn)) in
    match meth, uri_list with
    | _, "api" :: path          -> Api_handler.handle routes redir meth path sock_data headers body
    | `GET, path                ->
       (try match Hashtbl.find pages path with
            | None -> respond_ok ()
            | Some page -> respond_string page ()
        with _ -> redir (fun _ -> resource settings.path uri))
    | _                         -> not_found ()
  in
  handler

let create config auth_filter routes pages =
  let settings = Conf.get config in
  let handler  = get_handler ~settings ~auth_filter ~routes ~pages in
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port settings.port))
                                (Cohttp_lwt_unix.Server.make ~callback:handler ())
