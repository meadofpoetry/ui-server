open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_protocol
open Util_react
open Boards

let ( >>=? ) = Lwt_result.( >>= )

let ( >>= ) = Lwt.( >>= )

module Config = Kv_v.RW (Board_settings)

let ports_sync
    (src : Logs.src)
    (board : Topology.topo_board)
    (input : input signal)
    (streams : Stream.t list signal) : bool signal Boards.Board.Ports.t =
  List.fold_left
    (fun acc (p : Topology.topo_port) ->
      match input_of_enum p.port with
      | None -> Boards.Board.invalid_port src p.port
      | Some i ->
          let f a b =
            match a, b with
            | x, _ :: _ when equal_input i x -> true
            | _ -> false
          in
          let s = S.l2 ~eq:( = ) f input streams in
          Boards.Board.Ports.add p.port s acc)
    Boards.Board.Ports.empty
    board.ports

let ports_active (src : Logs.src) (board : Topology.topo_board) (input : input signal) :
    bool signal Boards.Board.Ports.t =
  List.fold_left
    (fun acc (p : Topology.topo_port) ->
      match input_of_enum p.port with
      | None -> Boards.Board.invalid_port src p.port
      | Some i ->
          let s = S.map ~eq:( = ) (equal_input i) input in
          Boards.Board.Ports.add p.port s acc)
    Boards.Board.Ports.empty
    board.ports

let get_input_source_from_env src (b : Topology.topo_board) =
  match Topology.Env.find_opt "input_source" b.env with
  | None -> None
  | Some s -> (
    match int_of_string_opt s with
    | Some x -> Some x
    | None ->
        Logs.warn ~src (fun m ->
            m "Failed to parse input source value from environment: %s" s);
        None)

let get_t2mi_source_from_env src (b : Topology.topo_board) =
  match Topology.Env.find_opt "t2mi_source" b.env with
  | None -> None
  | Some s -> (
    match int_of_string_opt s with
    | Some x -> Some x
    | None ->
        Logs.warn ~src (fun m ->
            m "Failed to parse T2-MI source value from environment: %s" s);
        None)

let update_config_with_env src (config : config) (b : Topology.topo_board) =
  let config =
    match get_input_source_from_env src b with
    | None -> config
    | Some input_source -> {config with input_source}
  in
  match get_t2mi_source_from_env src b with
  | None -> config
  | Some t2mi_source -> {config with t2mi_source}

let make_services_tab_template (b : Topology.topo_board) =
  object
    method stylesheets =
      [ "/css/Chart.min.css"
      ; "/css/board-niitv-tsan.min.css"
      ; "/css/board-niitv-tsan-page-input.min.css" ]

    method pre_scripts =
      [ `Src "/js/moment.min.js" (* TODO remove *)
      ; `Src "/js/Chart.min.js"
      ; `Src "/js/chartjs-plugin-datalabels.min.js" ]

    method post_scripts = [`Src "/js/board-niitv-tsan-page-input.js"]

    method content =
      List.map
        Tyxml.Html.toelt
        [ Board_niitv_tsan_page_services_tyxml.Markup.create
            ~control:b.control
            ~children:[Tyxml.Html.txt "services"]
            () ]

    method title = "Сервисы"

    method path = Topology.make_board_path b.control
  end

let make_pids_tab_template (b : Topology.topo_board) =
  object
    method stylesheets =
      [ "/css/Chart.min.css"
      ; "/css/board-niitv-tsan.min.css"
      ; "/css/board-niitv-tsan-page-input.min.css" ]

    method pre_scripts =
      [ `Src "/js/moment.min.js" (* TODO remove *)
      ; `Src "/js/Chart.min.js"
      ; `Src "/js/chartjs-plugin-datalabels.min.js" ]

    method post_scripts = [`Src "/js/board-niitv-tsan-page-input.js"]

    method content =
      List.map
        Tyxml.Html.toelt
        [Board_niitv_tsan_page_pids_tyxml.Markup.create ~control:b.control ()]

    method title = "PIDs"

    method path =
      Netlib.Uri.Path.(concat (Topology.make_board_path b.control) (of_string "pids"))
  end

let board_id = Board_niitv_tsan_types.board_id

let create
    (b : Topology.topo_board)
    (_streams : Stream.t list React.signal)
    (convert_streams :
      Topology.topo_board -> Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (send : Cstruct.t -> unit Lwt.t)
    (_db : Db.t)
    (kv : Kv.RW.t) : (Board.t, [> Board.error]) Lwt_result.t =
  Lwt.return @@ Boards.Board.create_log_src b
  >>=? fun (src : Logs.src) ->
  let default = update_config_with_env src Board_settings.default b in
  Config.create ~default kv ["board"; string_of_int b.control]
  >>=? fun (kv : config Kv_v.rw) ->
  Protocol.create src send (convert_streams b) kv
  >>=? fun (api : Protocol.api) ->
  kv#get
  >>= fun config ->
  let state =
    object
      method finalize () = Lwt.return ()
    end
  in
  let input =
    React.S.hold ~eq:equal_input config.input
    @@ React.E.map (fun (s : Parser.Status.t) -> s.input) api.notifs.status
  in
  let input_tabs =
    List.map (fun x ->
        `Input x, [make_services_tab_template b; make_pids_tab_template b])
    @@ Topology.topo_inputs_of_topo_board b
  in
  let board =
    { Board.http = Board_niitv_tsan_http.handlers b.control api
    ; ws = Board_niitv_tsan_http.ws b.control api
    ; templates = []
    ; control = b.control
    ; id = Topology.board_id_of_topo_board b
    ; streams_signal = api.notifs.streams
    ; log_source = (fun src -> Board_logger.create b.control api src)
    ; loop = api.loop
    ; push_data = api.push_data
    ; connection = api.notifs.state
    ; ports_sync = ports_sync src b input api.notifs.streams
    ; ports_active = ports_active src b input
    ; stream_handler = None
    ; state = (state :> < finalize : unit -> unit Lwt.t >)
    ; gui_tabs = input_tabs }
  in
  Lwt.return_ok board
