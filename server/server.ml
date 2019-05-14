open Lwt.Infix

module Api_http = Api_cohttp.Make(Application_types.User)(Application_types.Body)

let ( / ) = Filename.concat
          
module Config = struct

  type t = { https_enabled : bool
           ; tls_path : string
           ; tls_key : string option
           ; tls_cert : string option
           ; resources : string
           ; https_port : int
           ; http_port : int
           } [@@deriving eq, yojson]

  let to_string t =
    Yojson.Safe.to_string (to_yojson t)

  let of_string s =
    match of_yojson (Yojson.Safe.from_string s) with
    | Ok v -> v
    | Error e -> failwith e
               
  let default = { https_enabled = true
                ; tls_path = Xdg.data_dir / "ui_server/tls"
                ; tls_key = Some "server.key"
                ; tls_cert = Some "server.crt"
                ; resources = Xdg.data_dir / "ui_server/resources"
                ; https_port = 8443
                ; http_port = 8080
                }
              
end

module Serv_conf = Kv_v.RW (Config)
                 
type config = Serv_conf.t
            
let create_config kv =
  Serv_conf.create ~default:Config.default kv [ "server" ]

exception Server_reset

let is_none = function None -> true | _ -> false

let get_exn = function Some x -> x | _ -> failwith "get_exn: none"
                                        
let handler ~resources_path ~auth_filter ~routes =

  let resource base uri =
    let fname = Filename.concat base uri in
    Cohttp_lwt_unix.Server.respond_file ~fname ()
    >>= fun resp -> Lwt.return (`Response resp)
  in
  
  fun (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
      (req  : Cohttp_lwt_unix.Request.t)
      (body : Cohttp_lwt.Body.t)
  ->
  let headers  = Cohttp_lwt_unix.Request.headers req in
  (* let uri =  Uri.sep @@ Request.uri req in*)
  let uri = Cohttp_lwt_unix.Request.uri req in
  let path = Netlib.Uri.path uri in
  let meth = Cohttp_lwt_unix.Request.meth req in
  let env = Api_cohttp.env_of_headers headers in
  let sock_data = (req, (fst conn)) in

  let respond_page () =
    resource resources_path path
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

let create (config : config) auth_filter routes =  
  config#get >>= fun settings ->

  let resources_path = try
      Futil.Path.to_explicit_exn settings.resources
    with _ -> Config.default.resources
  in
  
  let callback = handler
                   ~resources_path
                   ~auth_filter
                   ~routes
  in

  let server = Cohttp_lwt_unix.Server.make_response_action ~callback () in

  let http_mode = `TCP (`Port settings.http_port) in

  let stop, wakener = Lwt.wait () in
  let http_server = Cohttp_lwt_unix.Server.create
                      ~stop
                      ~mode:http_mode
                      ~on_exn:(function Server_reset ->
                                         Lwt.wakeup wakener ()
                                      | e ->
                                         Logs.err (fun m -> m "(Server) Exception: %s" (Printexc.to_string e)))
                      server

  in

  if not settings.https_enabled
     || is_none settings.tls_key
     || is_none settings.tls_cert

  then http_server

  else begin

      try

        let tls_path = Futil.Path.to_explicit_exn settings.tls_path in
        
        let key_path = tls_path / get_exn settings.tls_key
        and crt_path = tls_path / get_exn settings.tls_cert in

        if not (Sys.file_exists key_path)
           || not (Sys.file_exists crt_path)

        then http_server

        else 

          let mode =
            `TLS
              (`Crt_file_path crt_path,
               `Key_file_path key_path,
               `No_password,
               `Port settings.https_port)
          in
          
          let tls_server = Cohttp_lwt_unix.Server.create ~mode server in
          
          Lwt.pick [ http_server; tls_server ]
          
      with _ -> http_server
    end
