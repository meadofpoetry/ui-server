open Application_types
open Netlib.Uri
module Api_http = Api_cohttp.Make (User) (Body)
module Api_template = Api_cohttp_template.Make (User)
module Api_websocket = Api_websocket.Make (User) (Body) (Body_ws)

let icon d = Components_tyxml.Icon.F.SVG.icon ~d ()

let logout_page_props () =
  let goodbye = "До новых встреч!" in
  let message =
    "Передумали и хотите обратно? Нажмите"
  in
  Api_template.make_template_props ~has_navigation_drawer:false
    ~has_top_app_bar:false
    ~stylesheets:[ "/css/page-logout.min.css" ]
    ~post_scripts:[ `Raw "logout();" ]
    ~content:
      (List.map Tyxml.Html.toelt
         Tyxml.Html.
           [
             div
               ~a:[ a_class [ "logout-page" ] ]
               [
                 icon Components_tyxml.Svg_icons.human_greeting;
                 div ~a:[ a_class [ "logout-page__goodbye" ] ] [ txt goodbye ];
                 div ~a:[ a_class [ "logout-page__message" ] ] [ txt message ];
                 Components_tyxml.Button.F.button_a ~label:"Войти"
                   ~appearance:Raised ~href:(uri_of_string "/") ();
               ];
           ])
    ()

let error_page_props error =
  let error_code, status, message =
    match error with
    | `Not_found ->
        ( "404",
          "Страница не найдена",
          "Потерялись? Вы можете вернуться на " )
    | `Forbidden ->
        ( "403",
          "Доступ запрещён",
          "Похоже, что доступ к данной странице \
           Вам запрещён... Не расстраивайтесь, \
           Вы можете вернуться на " )
  in
  Api_template.make_template_props
    ~title:"Что-то пошло не так..."
    ~stylesheets:[ "/css/page-error.min.css" ]
    ~content:
      (List.map Tyxml.Html.toelt
         Tyxml.Html.
           [
             div
               ~a:[ a_class [ "error-page" ] ]
               [
                 div ~a:[ a_class [ "error-page__code" ] ] [ txt error_code ];
                 div ~a:[ a_class [ "error-page__status" ] ] [ txt status ];
                 div ~a:[ a_class [ "error-page__message" ] ] [ txt message ];
                 ul
                   ~a:[ a_class [ "error-page__links" ] ]
                   [
                     li
                       [
                         a
                           ~a:[ a_href @@ uri_of_string "/" ]
                           [ txt "домашнюю страницу." ];
                       ];
                   ];
               ];
           ])
    ()

let user_pages : 'a. unit -> 'a Api_template.item list =
 fun () ->
  let open Api_template in
  let props =
    make_template_props ~title:"Настройки пользователей"
      ~post_scripts:[ `Src "/js/page-user-settings.js" ]
      ~stylesheets:[ "/css/page-user-settings.min.css" ]
      ()
  in
  simple ~priority:(`Index 1) ~title:"Пользователи"
    ~icon:(Tyxml.Html.toelt @@ icon Components_tyxml.Svg_icons.account)
    ~path:(Path.of_string "settings/user")
    props

let user_handlers (users : Application.User_api.t) =
  let open Api_http in
  make ~prefix:"user"
    [
      node ~doc:"Changes user password" ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("password" @/ empty)
        ~query:Query.empty
        (Application.User_api.set_password users);
      node ~doc:"Logout" ~meth:`GET
        ~path:Path.Format.("logout" @/ empty)
        ~query:Query.empty
        (fun _ _ _ _ ->
          let status = `Unauthorized in
          let headers =
            Cohttp.Header.of_list [ ("www-authenticate", "xBasic ") ]
          in
          let rsp =
            Lwt.Infix.(
              Cohttp_lwt_unix.Server.respond ~body:`Empty ~headers ~status ()
              >>= fun x -> Lwt.return (`Response x))
          in
          Lwt.return (`Instant rsp));
    ]

let input (app : Application.t) (input : Topology.topo_input) =
  let open Api_template in
  let get_input_href (x : Topology.topo_input) =
    let name = Topology.input_to_string x.input in
    let id = string_of_int x.id in
    List.fold_left Filename.concat "" [ "input"; name; id ]
  in
  let title = Topology.get_input_name input in
  simple ~priority:(`Index input.id) ~title
    ~icon:(Tyxml.Html.toelt @@ icon Components_tyxml.Svg_icons.arrow_right)
    ~path:(Path.of_string @@ get_input_href input)
    (Input_template.make_template input app.proc app.hw.boards
       (React.S.value app.topo))

let topo_page_path = "topology"

let application_pages (app : Application.t) =
  let open Api_template in
  let hw_templates =
    Boards.Board.Ports.fold
      (fun _ (x : Boards.Board.t) acc -> x.templates @ acc)
      app.hw.boards []
  in
  let topo = React.S.value app.topo in
  let input_props = List.map (input app) @@ Topology.get_inputs topo in
  let topology_props =
    make_template_props ~title:"Конфигурация"
      ~pre_scripts:[ `Src "/js/ResizeObserver.js" ]
      ~post_scripts:[ `Src "/js/page-topology.js" ]
      ~stylesheets:[ "/css/page-topology.min.css" ]
      ()
  in
  subtree ~priority:(`Index 1) ~title:"Входы"
    ~icon:(Tyxml.Html.toelt @@ icon Components_tyxml.Svg_icons.arrow_right_box)
    (List.flatten input_props)
  @ simple ~priority:(`Index 4) ~title:"Конфигурация"
      ~icon:(Tyxml.Html.toelt @@ icon Components_tyxml.Svg_icons.tournament)
      ~path:(Path.of_string topo_page_path)
      topology_props
  @ simple ~priority:(`Index 999) ~title:"Выйти"
      ~icon:(Tyxml.Html.toelt @@ icon Components_tyxml.Svg_icons.logout_variant)
      ~path:(Path.of_string "logout") (logout_page_props ())
  @ hw_templates

let application_handlers (app : Application.t) =
  let open Api_http in
  make ~prefix:"application"
    [
      node ~doc:"Sets streams that are received by PC process"
        ~restrict:[ `Guest ] ~meth:`POST
        ~path:Path.Format.("stream-table" @/ empty)
        ~query:Query.empty
        (Application_api.set_streams app);
      node ~doc:"Returns device topology" ~meth:`GET
        ~path:Path.Format.("topology" @/ empty)
        ~query:Query.empty
        (Application_api.get_topology app);
      node ~doc:"Returns stream table" ~meth:`GET
        ~path:Path.Format.("stream-table" @/ empty)
        ~query:Query.empty
        (Application_api.get_streams app);
      node ~doc:"Returns all streams" ~meth:`GET
        ~path:Path.Format.("streams" @/ empty)
        ~query:Query.[ ("input", (module Single (Topology.Show_topo_input))) ]
        (Application_api.get_all_streams app);
      node ~doc:"Returns the source of a last suitable stream" ~meth:`GET
        ~path:Path.Format.("stream-source" @/ empty)
        ~query:Query.[ ("id", (module Single (Stream.ID))) ]
        (Application_api.get_stream_source app);
      node ~doc:"Log for input (and stream)" ~meth:`GET
        ~path:Path.Format.("log" @/ empty)
        ~query:
          Query.
            [
              ("board", (module List (Int)));
              ("cpu", (module List (String)));
              ("input", (module List (Topology.Show_topo_input)));
              ("id", (module List (Stream.ID)));
              ("limit", (module Option (Int)));
              ("from", (module Option (Time_uri.Show)));
              ("to", (module Option (Time_uri.Show)));
              ("duration", (module Option (Time_uri.Show_relative)));
              ("order", (module Option (Api.Show_order)));
            ]
        (Application_api.get_log app);
    ]

let application_ws (app : Application.t) =
  let open Api_websocket in
  (* TODO add closing event *)
  (*let socket_table = Api_websocket.make_socket_table () in*)
  make ~prefix:"application"
    [
      event_node ~doc:"Pushes device topology to the client"
        ~path:Path.Format.("topology" @/ empty)
        ~query:Query.empty
        (Application_api.Event.get_topology app);
      event_node ~doc:"Pushes stream table to the client"
        ~path:Path.Format.("stream-table" @/ empty)
        ~query:Query.empty
        (Application_api.Event.get_streams app);
      event_node ~doc:"Log for input (and stream)"
        ~path:Path.Format.("log" @/ empty)
        ~query:
          Query.
            [
              ("input", (module List (Topology.Show_topo_input)));
              ("id", (module List (Stream.ID)));
            ]
        (Application_api.Event.get_log app);
    ]

let tick ?(timeout = 1.) () =
  let ( >>= ) = Lwt.bind in
  let e, push = React.E.create () in
  let rec aux () =
    Lwt_unix.sleep timeout >>= fun () ->
    push ();
    aux ()
  in
  (e, aux ())

type t = {
  ping_loop : unit Lwt.t;
  routes : Api_http.t;
  not_found : User.t -> string;
  forbidden : User.t -> string;
}

let make_error_page ~template templates status user =
  let template_props = error_page_props status in
  Api_template.make_page ~template templates template_props user

let create templates (app : Application.t) foreign_pages foreing_handlers
    foreign_ws =
  let ( >>= ) = Lwt_result.bind in
  Kv.RO.read templates [ "base.html" ] >>= fun template ->
  let proc_pages = match app.proc with None -> [] | Some proc -> proc#pages in
  let settings_subtree =
    Api_template.subtree ~title:"Настройки"
      ( Pc_control_http.network_pages ()
      @ Pc_control_http.software_updates_pages ()
      @ Pc_control_http.power_pages ()
      @ user_pages ()
      @ foreign_pages )
  in
  let templates = settings_subtree @ application_pages app @ proc_pages in
  let application_api = application_handlers app in
  let board_api =
    Boards.Board.Ports.fold
      (fun _ x acc -> x.Boards.Board.http @ acc)
      app.hw.boards []
    |> Api_http.merge ~prefix:"board"
  in
  let proc_api_list =
    match app.proc with None -> [] | Some proc -> proc#http
  in
  let board_ws =
    Boards.Board.Map.fold
      (fun _ x acc -> x.Boards.Board.ws @ acc)
      app.hw.boards []
    |> Api_websocket.merge ~prefix:"board"
  in
  let proc_ws_list = match app.proc with None -> [] | Some proc -> proc#ws in
  let pages =
    Api_http.make
      ( Api_http.node ~doc:"Home page redirect" ~meth:`GET
          ~path:Path.Format.empty ~query:Query.empty
          (fun _user _body _env _state ->
            let uri = Uri.make ~path:topo_page_path () in
            Lwt.return (`Redirect uri))
      :: Api_template.make ~template templates )
  in
  let api =
    Api_http.merge ~prefix:"api"
      ( foreing_handlers
      :: user_handlers app.users
      :: Pc_control_http.network_handlers app.network
      :: Pc_control_http.time_handlers app.timedate
      :: Pc_control_http.software_updates_handlers app.updates
      :: Pc_control_http.power_handlers
      :: application_api
      :: board_api
      :: proc_api_list )
  in
  let ping, loop = tick () in
  let ws =
    Api_websocket.to_http ~prefix:"ws" ~ping
    @@ Api_websocket.merge
         ( foreign_ws
         :: Pc_control_http.network_ws app.network
         :: Pc_control_http.software_updates_ws app.updates
         :: application_ws app
         :: board_ws
         :: proc_ws_list )
  in
  Lwt.return_ok
    {
      ping_loop = loop;
      routes = Api_http.merge [ api; ws; pages ];
      not_found = make_error_page ~template templates `Not_found;
      forbidden = make_error_page ~template templates `Forbidden;
    }
