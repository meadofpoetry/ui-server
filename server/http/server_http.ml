open Lwt.Infix
open Application_types
open Netlib.Uri
module Api_http = Api_cohttp.Make (User) (Body)
module Api_template = Api_cohttp_template.Make (User)
module Api_websocket = Api_websocket.Make (User) (Body) (Body_ws)

let ( / ) = Filename.concat

let make_icon path = Tyxml.Html.toelt @@ Components_tyxml.Icon.F.SVG.icon ~d:path ()

let get_certificate_info (path : string) =
  Futil_lwt.File.read path
  >>= function
  | Ok x -> Lwt.return (Certificate.of_x509 x)
  | Error _ -> Lwt.return_error "Certificate file read error"

let settings_of_config (config : Server.Config.t) =
  (match config.tls_cert with
  | None -> Lwt.return_ok None
  | Some name -> (
      get_certificate_info (config.tls_path / name)
      >>= function
      | Ok x -> Lwt.return_ok (Some (Uri.pct_decode name, x))
      | Error _ as e -> Lwt.return e))
  (* TODO cert parser may fail, but this is not a reason
     to return an error in this case *)
  >>= fun res ->
  let tls_cert =
    match res with
    | Ok x -> x
    | Error _ -> None
  in
  let tls_key =
    match config.tls_key with
    | None -> None
    | Some x -> Some (Uri.pct_decode x)
  in
  Lwt.return {Server_types.https_enabled = config.https_enabled; tls_cert; tls_key}

module Event = struct
  open Util_react

  let get_config (conf : Server.config) _user =
    let event =
      E.map_s (fun x ->
          settings_of_config x
          >>= fun x -> Lwt.return @@ Server_types.settings_to_yojson x)
      @@ S.changes conf#s
    in
    Lwt.return event
end

let get_config (conf : Server.config) _user _body _env _state =
  conf#get
  >>= fun config ->
  settings_of_config config
  >>= fun x -> Lwt.return (`Value (Server_types.settings_to_yojson x))

let set_https (conf : Server.config) _user body _env _state =
  match Util_json.Bool.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok https_enabled ->
      conf#get
      >>= fun settings ->
      let new_settings = {settings with https_enabled} in
      conf#set new_settings >>= fun () -> Lwt.return `Unit

let add_file validate setter (conf : Server.config) name _user body _env _state =
  conf#get
  >>= fun settings ->
  match validate settings name body with
  | Error e -> Lwt.return (`Error ("Invalid file, " ^ String.lowercase_ascii e))
  | Ok (name, body) -> (
      let path = settings.tls_path / name in
      Futil_lwt.File.write ~create:true path body
      >>= function
      | Error _ -> Lwt.return (`Error "could not save the file")
      | Ok () -> conf#set (setter settings (Some name)) >>= fun () -> Lwt.return `Unit)

let remove_file getter setter (conf : Server.config) _user _body _env _state =
  conf#get
  >>= fun settings ->
  match getter settings with
  | None -> Lwt.return `Unit
  | Some name ->
      let path = settings.tls_path / name in
      Futil_lwt.File.delete path
      (* XXX we set None even in case of error *)
      >>= fun _ -> conf#set (setter settings) >>= fun () -> Lwt.return `Unit

let validate_equal_filename (other : string option) ((name, _) as x) =
  match other with
  | None -> Ok x
  | Some other ->
      if String.equal other name then Error "filename already present" else Ok x

let validate_name ((name, _) as x) =
  if String.contains name '/' then Error "filename must not contain separators" else Ok x

let validate_crt_file ((_, file) as x) =
  match X509.Certificate.decode_pem @@ Cstruct.of_string file with
  | Error (`Msg m) -> Error m
  | Ok _ -> Ok x

let validate_key_file ((_, file) as x) =
  match X509.Private_key.decode_pem @@ Cstruct.of_string file with
  | Error (`Msg m) -> Error m
  | Ok _ -> Ok x

let validate_crt (conf : Server.Config.t) (name : string) (body : string) =
  let ( >>=? ) x f =
    match x with
    | Ok x -> f x
    | Error _ as e -> e
  in
  validate_name (name, body)
  >>=? validate_equal_filename conf.tls_key
  >>=? validate_crt_file

let validate_key (conf : Server.Config.t) (name : string) (body : string) =
  let ( >>=? ) x f =
    match x with
    | Ok x -> f x
    | Error _ as e -> e
  in
  validate_name (name, body)
  >>=? validate_equal_filename conf.tls_cert
  >>=? validate_key_file

let add_crt = add_file validate_crt (fun x v -> {x with tls_cert = v})

let add_key = add_file validate_key (fun x v -> {x with tls_key = v})

let remove_cert = remove_file (fun x -> x.tls_cert) (fun x -> {x with tls_cert = None})

let remove_key = remove_file (fun x -> x.tls_key) (fun x -> {x with tls_key = None})

let handlers (config : Server.config) =
  let open Api_http in
  Api_http.make
    ~prefix:"server"
    [ node
        ~doc:"Server configuration"
        ~meth:`GET
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (get_config config)
    ; node
        ~doc:"Restart"
        ~restrict:[`Operator; `Guest]
        ~meth:`POST
        ~path:Path.Format.("restart" @/ empty)
        ~query:Query.empty
        (fun _user _body _env _state ->
          Server.kill_server ();
          Lwt.return `Unit)
    ; node
        ~doc:"Set https flag"
        ~restrict:[`Operator; `Guest]
        ~meth:`POST
        ~path:Path.Format.("config/https-enabled" @/ empty)
        ~query:Query.empty
        (set_https config)
    ; node_raw
        ~doc:"Set tls certificate"
        ~restrict:[`Operator; `Guest]
        ~meth:`POST
        ~path:Path.Format.("config/crt" @/ String ^/ empty)
        ~query:Query.empty
        (add_crt config)
    ; node_raw
        ~doc:"Set tls private key"
        ~restrict:[`Operator; `Guest]
        ~meth:`POST
        ~path:Path.Format.("config/key" @/ String ^/ empty)
        ~query:Query.empty
        (add_key config)
    ; node_raw
        ~doc:"Delete tls certificate"
        ~restrict:[`Operator; `Guest]
        ~meth:`DELETE
        ~path:Path.Format.("config/crt" @/ empty)
        ~query:Query.empty
        (remove_cert config)
    ; node_raw
        ~doc:"Delete tls private key"
        ~restrict:[`Operator; `Guest]
        ~meth:`DELETE
        ~path:Path.Format.("config/key" @/ empty)
        ~query:Query.empty
        (remove_key config) ]

let ws (config : Server.config) =
  let open Api_websocket in
  make
    ~prefix:"server"
    [ event_node
        ~doc:"Server configuration"
        ~restrict:[`Operator; `Guest]
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Event.get_config config) ]

let pages : 'a. unit -> 'a Api_template.item list =
 fun () ->
  let open Api_template in
  let props =
    make_template_props
      ~title:"Настройки безопасности"
      ~post_scripts:[`Src "/js/page-server-settings.js"]
      ~stylesheets:["/css/page-server-settings.min.css"]
      ()
  in
  simple
    ~restrict:[`Operator; `Guest]
    ~priority:(`Index 10)
    ~title:"Безопасность"
    ~icon:(make_icon Components_tyxml.Svg_icons.server_security)
    ~path:(Path.of_string "settings/server")
    props
