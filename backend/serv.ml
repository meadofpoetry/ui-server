open Containers
open Cohttp_lwt_unix
open Common.User
open Api.Redirect
open Api.Interaction
open Api.Template
open Api
open Common.Uri

module Api_handler = Api.Handler.Make(Common.User)

let ( % ) = Fun.( % )

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
    let uri = Common.Uri.sep @@ Request.uri req in
    let resource_path = Common.Uri.(Path.to_string uri.path) in
    let respond_page path id =
      let tbl = match id with
        | `Root -> pages.root
        | `Operator -> pages.operator
        | `Guest -> pages.guest in
      (try Dispatcher.dispatch tbl path
       with _ -> resource settings.path resource_path)
    in
    let meth = Request.meth req in
    let redir = auth_filter headers in
    let sock_data = (req, (fst conn)) in
    let root, path = Common.Uri.(Path.next uri.path) in
    match meth, root with
    | _, (Some "api") ->
       Api_handler.handle routes redir (Common.Uri.upgrade_path uri path)
         meth headers body sock_data
    | `GET, _ -> redir (respond_page uri)
    | _ -> not_found ()
  in
  handler

let create config auth_filter routes templates =
  let settings = Conf.get config in
  let read_file (name : string) =
    Filename.concat "html/templates" name
    |> Filename.concat settings.path
    |> Containers.IO.File.read_exn in
  let (files : Api.Template.template_files) =
    { base = read_file "base.html"
    ; nav = read_file "navigation.html"
    ; app_bar = read_file "app_bar.html"
    } in
  let pages =
    Common.User.map_table
      (Api.Template.build_route_table files)
      templates in
  let handler = get_handler ~settings ~auth_filter ~routes ~pages in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port settings.port))
    ~on_exn:(fun e -> Logs.err (fun m -> m "(Server) Exception: %s" (Printexc.to_string e)))
    (Cohttp_lwt_unix.Server.make ~callback:handler ())
