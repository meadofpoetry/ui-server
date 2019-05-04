open Containers
open Cohttp_lwt_unix
open Lwt.Infix
(*open Common.User
open Api.Redirect
open Api.Interaction
open Common.Uri*)

module Api_http = Api_cohttp.Make(Application_types.User)(Application_types.Body)

let ( % ) = Fun.( % )

let resource base uri =
  let fname = Filename.concat base uri in
  Cohttp_lwt_unix.Server.respond_file ~fname ()

module Settings = struct
  type t = { path : string
           ; port : int
           } [@@deriving yojson]
  let default = { path = Filename.concat Filename.current_dir_name "dist/resources"
                ; port = 8080
                }
  let domain = "server"
  let of_string s =
    Yojson.Safe.from_string s
    |> of_yojson
    |> function Ok v -> v | Error e -> failwith e
end
(*
module Conf = Storage.Config.Make(Settings)
 *)
let get_handler ~settings
      ~auth_filter
      ~routes
  =
  let open Settings in
  let handler
        (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
        (req  : Cohttp_lwt_unix.Request.t)
        (body : Cohttp_lwt.Body.t) =
    let headers  = Request.headers req in
    (* let uri =  Uri.sep @@ Request.uri req in*)
    let uri = Request.uri req in
    let resource_path = Netlib.Uri.path uri in
    let meth = Request.meth req in
    let env = Api_cohttp.env_of_headers headers in
    let sock_data = (req, (fst conn)) in

    let respond_page () =
      resource settings.path resource_path
    in
    (*let root, path = Common.Uri.(Path.next uri.path) in*)

    Cohttp_lwt.Body.to_string body
    >>= fun body ->
    Api_http.handle
      routes
      ~state:sock_data
      ~default:respond_page
      ~meth
      ~env
      ~redir:auth_filter
      uri
      body
      (*
    Netlib.Uri.Dispatcher.dispatch
      ~default:respond_page
    
    match meth, root with
    | _, (Some "api") ->
       Api_handler.handle routes redir (Common.Uri.upgrade_path uri path)
         meth headers body sock_data
    | `GET, _ -> redir (respond_page uri)
    | _ -> not_found ()*)
  in
  handler

let create kv auth_filter routes =
  let (>>=) = Lwt_result.bind in
  Kv.RW.parse ~default:Settings.default Settings.of_string kv ["server"]

  >>= fun settings ->
 (* let tmpl = Filename.concat settings.path "html/templates/base.html"
             |> Containers.IO.File.read_exn (* FIXME *) in *)
  let handler  = get_handler ~settings ~auth_filter ~routes in

  Lwt.return_ok
  @@ Cohttp_lwt_unix.Server.create
       ~mode:(`TCP (`Port settings.port))
       ~on_exn:(fun e ->
         Logs.err (fun m -> m "(Server) Exception: %s" (Printexc.to_string e)))
       (Cohttp_lwt_unix.Server.make ~callback:handler ())
