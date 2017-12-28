open Cohttp_lwt_unix
open Containers
open Api.Redirect
open Api.Interaction

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

module Template = struct
  let topo = []
  let path = ""
end
module Rsp = Responses.Make(Template)

module Conf = Storage.Config.Make(Settings)

let get_handler ~topo
                ~settings
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
    let (module T : Responses.Template) = (module struct
                                             let topo = topo
                                             let path = settings.path
                                           end) in
    let module R  = Responses.Make(T) in
    let meth      = Request.meth req in
    let redir     = auth_filter headers in
    let sock_data = (req, (fst conn)) in
    match meth, uri_list with
    | `GET, []                  -> redir (fun _ -> R.home ())
    | `GET, ["hardware"]        -> redir (fun _ -> R.hardware ())
    | `GET, ["pipeline"]        -> redir (fun _ -> R.pipeline ())
    | `GET, "input"::name::[id] -> (match Common.Topology.input_of_string name, CCInt.of_string id with
                                    | Ok input,Some id -> redir (fun _ -> R.input {input;id} ())
                                    | _ -> not_found ())
    | `GET, ["demo"]            -> redir (fun _ -> R.demo ())
    | _, "api" :: path          -> Api_handler.handle routes redir meth path sock_data headers body
    | `GET, _                   -> redir (fun _ -> resource settings.path uri)
    | _                         -> not_found ()
  in
  handler

let create topo config auth_filter routes =
  let settings = Conf.get config in
  let handler  = get_handler ~topo ~settings ~auth_filter ~routes in
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port settings.port))
                                (Cohttp_lwt_unix.Server.make ~callback:handler ())
