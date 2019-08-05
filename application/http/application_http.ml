open Application_types
open Netlib.Uri

module Api_http = Api_cohttp.Make(User)(Body)

module Api_template = Api_cohttp_template.Make(User)

module Api_websocket = Api_websocket.Make(User)(Body)(Body_ws)

module Icon = Components_tyxml.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let icon x =
  let open Icon.SVG in
  let path = create_path x () in
  let icon = create [path] () in
  Tyxml.Html.toelt icon

let error_page_props error =
  let open Tyxml in
  let module Components = Components_tyxml.Bundle.Make(Xml)(Svg)(Html) in
  let error_code, status, message = match error with
    | `Not_found ->
      "404 - Page Not Found",
      "Страница не найдена.",
      "Потерялись? Вы можете вернуться на "
    | `Forbidden ->
      "403 - Forbidden",
      "Похоже, что доступ к данной странице Вам запрещён...",
      "Не расстраивайтесь, Вы можете вернуться на "
  in
  Api_template.make_template_props
    ~title:"Что-то пошло не так..."
    ~stylesheets:["/css/page-error.min.css"]
    ~content:(
      List.map Html.toelt
        Html.(
          [div ~a:[a_class ["error-page"]]
             [ div ~a:[a_class ["error-page__code"]]
                 [txt error_code]
             ; div ~a:[a_class ["error-page__status"]]
                 [txt status]
             ; div ~a:[a_class ["error-page__message"]]
                 [txt message]
             ; ul ~a:[a_class ["error-page__links"]]
                 [li [a ~a:[a_href @@ uri_of_string "/"]
                        [txt "домашнюю страницу."]]]
             ]
          ]))
    ()

let user_pages : 'a. unit -> 'a Api_template.item list =
  fun () ->
  let open Api_template in
  let props =
    make_template_props
      ~title:"Настройки пользователей"
      ~post_scripts:[Src "/js/page-user-settings.js"]
      ~stylesheets:["/css/page-user-settings.min.css"]
      () in
  simple
    ~priority:(`Index 1)
    ~title:"Пользователи"
    ~icon:(icon Components_tyxml.Svg_icons.account)
    ~path:(Path.of_string "settings/user")
    props

let user_handlers (users : Application.User_api.t) =
  let open Api_http in
  make ~prefix:"user"
    [ node ~doc:"Changes user password"
        ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("password" @/ empty)
        ~query:Query.empty
        (Application.User_api.set_password users)
    ]

let input topo (input : Topology.topo_input) =
  let open Api_template in
  let get_input_href (x : Topology.topo_input) =
    let name = Topology.input_to_string x.input in
    let id = string_of_int x.id in
    List.fold_left Filename.concat "" ["input"; name; id]
  in
  let path =
    Topology.get_paths topo
    |> List.find_opt (fun (i, _, _) -> Topology.equal_topo_input i input)
    |> (function Some (_, p, c) -> Some (p, c) | None -> None)
  in
  match path with
  | None -> failwith "input not found"
  | Some (boards, cpu) ->
     let title = Topology.get_input_name input in
     let boards =
       List.map (fun { Topology. control; manufacturer; model; version; _ } ->
           { Topology. control; manufacturer; model; version }) boards
       |> Util_json.List.to_yojson Topology.board_id_to_yojson
       |> Yojson.Safe.to_string in
     let cpu = (* TODO remove after 4.08 *)
       cpu
       |> (function Some (x : Topology.topo_cpu) -> Some x.process | None -> None)
       |> Util_json.Option.to_yojson Topology.process_type_to_yojson
       |> Yojson.Safe.to_string in
     let input_string = Topology.Show_topo_input.to_string input in
     let input_template =
       make_template_props
       ~title
       ~pre_scripts:[ Raw (Printf.sprintf "var input = \"%s\";\
                                           var boards = %s;\
                                           var cpu = %s;"
                             input_string boards cpu)]
       ~post_scripts:[Src "/js/input.js"]
       ()
     in
     let input_page =
       simple
         ~priority:(`Index input.id)
         ~title
         ~icon:(icon Components_tyxml.Svg_icons.arrow_right)
         ~path:(Path.of_string @@ get_input_href input)
         input_template
     in
     let pre = "input/" ^ get_input_href input in
     let stream_template =
       make_template_props
       ~title:("Входы / " ^ title)
       ~pre_scripts:[ Raw (Printf.sprintf "var input = \"%s\";\
                                           var boards = %s;\
                                           var cpu = %s;"
                             input_string boards cpu)
                    ; Src "/js/moment.min.js"
                    ; Src "/js/Chart.min.js"
                    ; Src "/js/chartjs-plugin-streaming.min.js"
                    ; Src "/js/chartjs-plugin-datalabels.min.js"
                    ]
       ~post_scripts:[Src "/js/stream.js"]
       () in
     let stream_page =
       parametric
         ~path:Path.Format.(pre @/ Stream.ID.fmt ^/ empty)
         stream_template
     in
     (*`Index input.id,*)
     input_page, stream_page

let topo_page_path = "topology"

let application_pages (app : Application.t) =
  let open Api_template in
  let hw_templates =
    Boards.Board.Ports.fold (fun _ (x : Boards.Board.t) acc -> x.templates @ acc)
      app.hw.boards []
  in
  let topo = React.S.value app.topo in
  let input_props, stream_templates =
    List.split
    @@ List.map (input topo)
    @@ Topology.get_inputs topo
  in
  let topology_props =
    make_template_props
      ~title:"Конфигурация"
      ~pre_scripts:[Src "/js/ResizeObserver.js"]
      ~post_scripts:[Src "/js/page-topology.js"]
      ~stylesheets:["/css/page-topology.min.css"]
      () in
  let logout =
    reference
      ?restrict:None
      ~priority:(`Index 999)
      ~title:"Выйти"
      ~icon:(icon Components_tyxml.Svg_icons.logout_variant)
      (Uri.of_string "logout") in
  subtree
    ~priority:(`Index 1)
    ~title:"Входы"
    ~icon:(icon Components_tyxml.Svg_icons.arrow_right_box)
    (List.flatten input_props)
  @ simple
    ~priority:(`Index 4)
    ~title:"Конфигурация"
    ~icon:(icon Components_tyxml.Svg_icons.tournament)
    ~path:(Path.of_string topo_page_path)
    topology_props
  @ List.flatten stream_templates
  @ hw_templates
  @ logout

let application_handlers (app : Application.t) =
  let open Api_http in
  make ~prefix:"application"
    [ node ~doc:"Sets streams that are received by PC process"
        ~restrict:[`Guest]
        ~meth:`POST
        ~path:Path.Format.("stream-table" @/ empty)
        ~query:Query.empty
        (Application_api.set_streams app)
    ; node ~doc:"Returns device topology"
        ~meth:`GET
        ~path:Path.Format.("topology" @/ empty)
        ~query:Query.empty
        (Application_api.get_topology app)
    ; node ~doc:"Returns stream table"
        ~meth:`GET
        ~path:Path.Format.("stream-table" @/ empty)
        ~query:Query.empty
        (Application_api.get_streams app)
    ; node ~doc:"Returns all streams"
        ~meth:`GET
        ~path:Path.Format.("streams" @/ empty)
        ~query:Query.["input", (module Single(Topology.Show_topo_input))]
        (Application_api.get_all_streams app)
    ; node ~doc:"Returns the source of a last suitable stream"
        ~meth:`GET
        ~path:Path.Format.("stream-source" @/ empty)
        ~query:Query.["id", (module Single(Stream.ID))]
        (Application_api.get_stream_source app)
    ; node ~doc:"Log for input (and stream)"
        ~meth:`GET
        ~path:Path.Format.("log" @/ empty)
        ~query:Query.[ "board", (module List(Int))
                     ; "cpu", (module List(String))
                     ; "input", (module List(Topology.Show_topo_input))
                     ; "id", (module List(Stream.ID))
                     ; "limit", (module Option(Int))
                     ; "from", (module Option(Time_uri.Show))
                     ; "to", (module Option(Time_uri.Show))
                     ; "duration", (module Option(Time_uri.Show_relative)) ]
        (Application_api.get_log app)
    ]

let application_ws (app : Application.t) =
  let open Api_websocket in
  (* TODO add closing event *)
  (*let socket_table = Api_websocket.make_socket_table () in*)
  make ~prefix:"application"
    [ event_node ~doc:"Pushes device topology to the client"
        ~path:Path.Format.("topology" @/ empty)
        ~query:Query.empty
        (Application_api.Event.get_topology app)
    ; event_node ~doc:"Pushes stream table to the client"
        ~path:Path.Format.("stream-table" @/ empty)
        ~query:Query.empty
        (Application_api.Event.get_streams app)
    ; event_node ~doc:"Log for input (and stream)"
        ~path:Path.Format.("log" @/ empty)
        ~query:Query.["input", (module List(Topology.Show_topo_input));
           "id", (module List(Stream.ID))]
        (Application_api.Event.get_log app)
    ]

let other_handlers () =
  let open Api_http in
  make
    [ node ~doc:"Home page redirect"
        ~meth:`GET
        ~path:Path.Format.empty
        ~query:Query.empty
        (fun _user _body _env _state ->
           let uri = Uri.make ~path:topo_page_path () in
           Lwt.return (`Redirect uri))
    ]

let tick ?(timeout = 1.) () =
  let ( >>= ) = Lwt.bind in
  let e, push = React.E.create () in
  let rec aux () =
    Lwt_unix.sleep timeout
    >>= fun () ->
    push ();
    aux () in
  e, aux ()

type t =
  { ping_loop : unit Lwt.t
  ; routes : Api_http.t
  ; not_found : User.t -> string
  ; forbidden : User.t -> string
  }

let make_error_page ~template templates status user =
  let template_props = error_page_props status in
  Api_template.make_page ~template templates template_props user

let create templates (app : Application.t)
    foreign_pages
    foreing_handlers
    foreign_ws =
  let (>>=) = Lwt_result.bind in

  Kv.RO.read templates [ "base.html" ]
  >>= fun template ->

  let proc_pages = match app.proc with
    | None -> []
    | Some proc -> proc#pages () in
  let settings_subtree =
    Api_template.subtree
      ~title:"Настройки"
      (Pc_control_http.network_pages ()
       @ user_pages ()
       @ foreign_pages) in
  let templates =
    settings_subtree
    @ application_pages app
    @ proc_pages
  in
  let application_api = application_handlers app in
  let board_api =
    Boards.Board.Ports.fold (fun _ x acc -> x.Boards.Board.http @ acc) app.hw.boards []
    |> Api_http.merge ~prefix:"board"
  in
  let proc_api_list = match app.proc with
    | None -> []
    | Some proc -> proc#http ()
  in
  let application_ws = application_ws app in
  let board_ws =
    Boards.Board.Map.fold (fun _ x acc -> x.Boards.Board.ws @ acc) app.hw.boards []
    |> Api_websocket.merge ~prefix:"board"
  in
  let proc_ws_list = match app.proc with
    | None -> []
    | Some proc -> proc#ws ()
  in
  let pages = Api_http.make @@ Api_template.make ~template templates in
  
  let api = Api_http.merge ~prefix:"api"
      ( foreing_handlers
        :: user_handlers app.users
        :: Pc_control_http.network_handlers app.network
        :: application_api
        :: board_api
        :: proc_api_list)
  in
  let other = other_handlers () in
  let ping, loop = tick () in
  let ws =
    Api_websocket.to_http ~prefix:"ws" ~ping
    @@ Api_websocket.merge
      ( foreign_ws
        :: Pc_control_http.network_ws app.network
        :: application_ws
        :: board_ws
        :: proc_ws_list )
  in
  Lwt.return_ok
    { ping_loop = loop
    ; routes = Api_http.merge [api; ws; pages; other]
    ; not_found = make_error_page ~template templates `Not_found
    ; forbidden = make_error_page ~template templates `Forbidden
    }
