open Application_types
open Board_niitv_dvb4ch_types
open Board_niitv_dvb4ch_protocol
open Boards
module Config = Kv_v.RW (Board_settings)
module Api_template = Api_cohttp_template.Make (User)

let ( >>= ) = Lwt_result.bind

let rec has_sync = function
  | [] -> false
  | (_, ({ data; _ } : Measure.t ts)) :: tl -> (
      match data.lock, data.bitrate with
      | true, Some x when x > 0 -> true
      | _ -> has_sync tl)

let get_source_from_env src (b : Topology.topo_board) =
  match Topology.Env.find_opt "source" b.env with
  | None -> None
  | Some s -> (
      match int_of_string_opt s with
      | Some i -> Some i
      | None ->
          Logs.warn ~src (fun m ->
              m "Failed to parse source ID value from environment: %s" s);
          None)

let make_input_tab_template (b : Topology.topo_board) =
  ( Topology.make_board_path b.control,
    Api_template.make_template_props
      ~title:"RF"
      ~stylesheets:
        [
          "/css/Chart.min.css";
          "/css/board-niitv-dvb4ch.min.css";
          "/css/board-niitv-dvb4ch-page-input.min.css";
        ]
      ~pre_scripts:
        [
          `Src "/js/moment.min.js";
          `Src "/js/Chart.min.js";
          `Src "/js/chartjs-plugin-streaming.min.js";
          `Src "/js/chartjs-plugin-datalabels.min.js";
          `Src "/js/chartjs-plugin-annotation.min.js";
        ]
      ~post_scripts:[ `Src "/js/board-niitv-dvb4ch-page-input.js" ]
      () )

let board_id = Board_niitv_dvb4ch_types.board_id

let create
    (b : Topology.topo_board)
    (_ : Stream.t list React.signal)
    (convert_streams :
      Topology.topo_board -> Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (send : Cstruct.t -> unit Lwt.t)
    (db : Db.t)
    (kv : Kv.RW.t) : (Board.t, [> Board.error ]) Lwt_result.t =
  Lwt.return @@ Boards.Board.create_log_src b >>= fun (src : Logs.src) ->
  let default =
    match get_source_from_env src b with
    | None -> Board_settings.default
    | Some source -> { Board_settings.default with source }
  in
  Config.create ~default kv [ "board"; string_of_int b.control ]
  >>= fun (cfg : Device.config Kv_v.rw) ->
  Protocol.create src send (convert_streams b) cfg b.control db
  >>= fun (api : Protocol.api) ->
  let state =
    object
      method finalize () = Lwt.return ()
    end
  in
  let input_tabs =
    List.map (fun x -> `Input x, [ make_input_tab_template b ])
    @@ Topology.topo_inputs_of_topo_board b
  in
  let (board : Board.t) =
    {
      http = Board_niitv_dvb4ch_http.handlers b.control api;
      ws = Board_niitv_dvb4ch_http.ws b.control api;
      templates = [];
      control = b.control;
      id = Topology.board_id_of_topo_board b;
      streams_signal = api.notifs.streams;
      log_source = (fun _ -> React.E.never) (* TODO implement source *);
      loop = api.loop;
      push_data = api.push_data;
      connection = api.notifs.state;
      ports_sync =
        List.fold_left
          (fun acc (p : Topology.topo_port) ->
            (match p.port with
            | 0 ->
                React.E.map has_sync api.notifs.measures |> React.S.hold ~eq:( = ) false
            | x -> Boards.Board.invalid_port src x)
            |> fun x -> Board.Ports.add p.port x acc)
          Board.Ports.empty
          b.ports;
      ports_active =
        List.fold_left
          (fun acc (p : Topology.topo_port) ->
            (match p.port with
            | 0 -> React.S.const true
            | x -> Boards.Board.invalid_port src x)
            |> fun x -> Board.Ports.add p.port x acc)
          Board.Ports.empty
          b.ports;
      stream_handler = None;
      state = (state :> < finalize : unit -> unit Lwt.t >);
      gui_tabs = input_tabs;
    }
  in
  Lwt.return_ok board
