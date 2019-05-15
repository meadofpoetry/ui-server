open Lwt.Infix
open Application_types
open Netlib.Uri

module Api_http = Api_cohttp.Make(User)(Body)

module Api_template = Api_cohttp_template.Make(User)

module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let make_icon path =
  let open Icon.SVG in
  let path = create_path path () in
  let icon = create [path] () in
  Tyxml.Html.toelt icon

let get_config (conf : Server.config) _user _body _env _state =
  conf#get >>= fun settings ->
  let res = Server_types.{ https_enabled = settings.https_enabled
                         ; tls_cert = settings.tls_cert
                         ; tls_key = settings.tls_key
            }
            |> Server_types.settings_to_yojson
  in
  Lwt.return (`Value res)

let set_https (conf : Server.config) flag _user _body _env _state =
  conf#get >>= fun settings ->
  let new_settings = { settings with https_enabled = flag } in
  conf#set new_settings >>= fun () ->
  Lwt.return `Unit
  
let add_file setter (conf : Server.config) name _user body _env _state =
  conf#get >>= fun settings ->
  
  match Futil.Path.of_string settings.tls_path with
  | Error _ -> Lwt.return (`Error "internal server error")
  | Ok path ->
     if String.contains name '/'
     then Lwt.return (`Error "filename must not contain separators")
     else
       let path = Futil.Path.append path [name] in
       Futil_lwt.File.write ~create:true path body
       >>= function
       | Error _ -> Lwt.return (`Error "could not save the file")
       | Ok () ->
          conf#set (setter settings (Some name))
          >>= fun () ->
          Lwt.return `Unit

let add_cert = add_file (fun x v -> { x with tls_cert = v })

let add_key = add_file (fun x v -> { x with tls_key = v })
  
let handlers (config : Server.config) =
  let open Api_http in
  Api_http.
  make ~prefix:"server"
    [ node ~doc:"Server configuration"
        ~meth:`GET
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (get_config config)
    ; node ~doc:"Restart"
        ~restrict:[`Operator; `Guest ]
        ~meth:`POST
        ~path:Path.Format.("config/server_restart" @/empty)
        ~query:Query.empty
        (fun _user _body _env _state ->
          Server.kill_server (); Lwt.return `Unit)
    ; node ~doc:"Set https flag"
        ~restrict:[`Operator; `Guest ]
        ~meth:`POST
        ~path:Path.Format.("config/https_enabled" @/empty)
        ~query:Query.[ "value", (module Single(Bool)) ]
        (set_https config)
    ; node_raw ~doc:"Set tls cert"
        ~restrict:[`Operator; `Guest ]
        ~meth:`POST
        ~path:Path.Format.("config/cert" @/ String ^/empty)
        ~query:Query.empty
        (add_cert config)
    ; node_raw ~doc:"Set tls key"
        ~restrict:[`Operator; `Guest ]
        ~meth:`POST
        ~path:Path.Format.("config/key" @/ String ^/empty)
        ~query:Query.empty
        (add_key config)
    ]
  
let pages () : Api_template.topmost Api_template.item list =
  let open Api_template in
  let props =
    { title = Some "Сервер"
    ; pre_scripts = []
    ; post_scripts = [ Src "/js/server.js" ]
    ; stylesheets = []
    ; content = []
    }
  in
  simple
    ~restrict:[`Operator; `Guest]
    ~priority:(`Index 10)
    ~title:"Сервер"
    ~icon:(make_icon Icon.SVG.Path.lan)
    ~path:(Path.of_string "settings/server")
    props
