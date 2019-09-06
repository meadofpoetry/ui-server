open Application_types
open Components
open Topo_types

module CSS = struct
  let root = "topology-board"

  let header = BEM.add_element root "header"

  let body = BEM.add_element root "body"
end

let rec eq_port (p1 : Topology.topo_port) (p2 : Topology.topo_port) =
  p1.port = p2.port
  &&
  match p1.child, p2.child with
  | Input i1, Input i2 -> Topology.equal_topo_input i1 i2
  | Board b1, Board b2 -> eq_board b1 b2
  | _, _ -> false

and eq_board (b1 : Topology.topo_board) (b2 : Topology.topo_board) =
  let open Topology in
  String.equal b1.model b2.model
  && String.equal b1.manufacturer b2.manufacturer
  && equal_version b1.version b2.version
  && b1.control = b2.control

let eq_node_entry (e1 : Topo_node.node_entry) (e2 : Topo_node.node_entry) =
  match e1, e2 with
  | `CPU c1, `CPU c2 -> Topology.equal_topo_cpu c1 c2
  | `Entry e1, `Entry e2 -> (
    match e1, e2 with
    | Board b1, Board b2 -> eq_board b1 b2
    | Input i1, Input i2 -> Topology.equal_topo_input i1 i2
    | _ -> false)
  | _ -> false

let get_board_name ({manufacturer; model; _} : Topology.topo_board) =
  match manufacturer, model with
  | "DekTec", "DTM-3200" -> "Приёмник TSoIP"
  | "NIITV", "TS2IP" -> "Передатчик TSoIP"
  | "NIITV", "TSAN" -> "Анализатор TS"
  | "NIITV", "DVB4CH" -> "Приёмник DVB"
  | _ -> Printf.sprintf "%s %s" manufacturer model

(* TODO add UI notification when the request has failed. *)
let port_setter (b : Topology.topo_board) port state =
  match b.manufacturer, b.model, b.version with
  | "NIITV", "TSAN", _ ->
      let open Board_niitv_tsan_http_js in
      Http_device.set_port ~port state b.control
      |> Lwt_result.map_err Api_js.Http.error_to_string
  | _ -> Lwt_result.fail "Device has no switchable ports"

(* TODO replace with something more abstract.
   Maybe the device should define a CSS style (color) on its own *)
let get_board_type ({manufacturer; model; _} : Topology.topo_board) =
  match manufacturer, model with
  | "DekTec", "DTM-3200" -> "IP2TS"
  | "NIITV", "TS2IP" -> "TS2IP"
  | "NIITV", "TSAN" -> "TS"
  | "NIITV", "DVB4CH" -> "DVB"
  | _ -> ""

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let make_board_niitv_tsan_settings state socket control =
  let open React in
  let open Board_niitv_tsan_http_js in
  let open Board_niitv_tsan_widgets in
  Http_device.get_t2mi_mode control
  >>= fun mode ->
  Http_streams.get_streams ~incoming:true control
  >>= fun streams ->
  Http_device.Event.get_t2mi_mode socket control
  >>= fun (mid, e_mode) ->
  Http_streams.Event.get_streams ~incoming:true socket control
  >>= fun (sid, e_strm) ->
  let w = T2mi_settings.make (S.value state) mode streams control in
  let notif =
    E.merge
      (fun _ -> w#notify)
      ()
      [ E.map (fun x -> `Mode x) e_mode
      ; E.map (fun x -> `Incoming_streams x) e_strm
      ; E.map (fun x -> `State x) @@ S.changes state ]
  in
  w#set_on_destroy (fun () ->
      E.stop ~strong:true notif;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket mid);
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket sid));
  Lwt.return_ok w#widget

let make_board_niitv_dvb4ch_settings state socket control =
  let open React in
  let open Board_niitv_dvb4ch_http_js in
  let open Board_niitv_dvb4ch_widgets in
  let rec get_plps acc = function
    | [] -> Lwt.return_ok acc
    | id :: tl ->
        Lwt_result.(
          Http_receivers.get_plp_list ~id control >>= fun x -> get_plps (x :: acc) tl)
  in
  Http_device.get_mode control
  >>= fun mode ->
  Http_device.get_receivers control
  >>= fun receivers ->
  get_plps [] receivers
  >>= fun plps ->
  Http_device.Event.get_mode socket control
  >>= fun (mode_id, e_mode) ->
  Http_receivers.Event.get_plp_list socket control
  >>= fun (plps_id, e_plps) ->
  (* FIXME *)
  let w = Settings.make (S.value state) mode plps (Some receivers) control in
  let notif =
    E.merge
      (fun _ -> w#notify)
      ()
      [ E.map (fun x -> `Mode x) e_mode
      ; E.map (fun x -> `PLPs x) e_plps
      ; E.map (fun x -> `State x) @@ S.changes state ]
  in
  w#set_on_destroy (fun () ->
      React.E.stop ~strong:true notif;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket plps_id);
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket mode_id));
  Lwt.return_ok w#widget

let make_board_dektec_dtm3200_settings state socket control =
  let open React in
  let open Board_dektec_dtm3200_types in
  let open Board_dektec_dtm3200_http_js in
  let open Board_dektec_dtm3200_widgets_js in
  Http_device.get_config control
  >>= fun {nw; ip_receive; _} ->
  Http_device.Event.get_config socket control
  >>= fun (id, event) ->
  let w = Widget_settings.make (S.value state) nw ip_receive control in
  let notif =
    E.merge
      (fun _ -> w#notify)
      ()
      [E.map (fun x -> `Config x) event; E.map (fun x -> `State x) @@ S.changes state]
  in
  w#set_on_destroy (fun () ->
      E.stop ~strong:true notif;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket id));
  Lwt.return_ok w#widget

let make_board_page (signal : Topology.topo_board React.signal) socket =
  let {Topology.manufacturer; model; version; control; _} = React.S.value signal in
  let state = React.S.map (fun (x : Topology.topo_board) -> x.connection) signal in
  match manufacturer, model, version with
  | "NIITV", "TSAN", _ -> make_board_niitv_tsan_settings state socket control
  | "NIITV", "DVB4CH", _ -> make_board_niitv_dvb4ch_settings state socket control
  | "DekTec", "DTM-3200", _ -> make_board_dektec_dtm3200_settings state socket control
  | _ -> Lwt.return_error "No settings available for the device"

module Header = struct
  class t (board : Topology.topo_board) () =
    let title = get_board_name board in
    let settings =
      match Topology.Env.find_opt "show-settings" board.env with
      | Some "false" -> None
      | _ ->
          let icon = Icon.SVG.(make_simple Path.settings)#root in
          let button = Icon_button.make ~icon () in
          button#add_class Topo_block.CSS.header_action_settings;
          Some button
    in
    object (self)
      inherit Topo_block.Header.t ?action:settings ~title () as super

      method! init () : unit =
        super#init ();
        super#add_class CSS.header

      method! layout () : unit =
        super#layout ();
        Option.iter Widget.layout self#settings_icon

      method settings_icon = settings
    end

  let create (board : Topology.topo_board) : t = new t board ()
end

module Body = struct
  class t (board : Topology.topo_board) () =
    object
      inherit Topo_block.Body.t (List.length board.ports) () as super

      method! init () : unit =
        super#init ();
        super#add_class CSS.body
    end

  let create (board : Topology.topo_board) : t = new t board ()
end

type event = [`State of Topology.topo_board]

class t
  ~(connections : (#Topo_node.t * connection_point) list)
  (socket : Api_js.Websocket.JSON.t)
  (board : Topology.topo_board) =
  let e_settings, push_settings = React.E.create () in
  let header = Header.create board in
  let body = Body.create board in
  let state, push_state = React.S.create board in
  object (self)
    val mutable _click_listener = None

    inherit
      Topo_block.t
        ~port_setter:(port_setter board) ~node:(`Entry (Board board)) ~connections
          ~header ~body () as super

    method! init () : unit =
      super#init ();
      self#notify (`State board);
      super#add_class CSS.root;
      super#set_attribute "data-board" @@ get_board_type board;
      Option.iter
        (fun (w : Icon_button.t) ->
          let listener =
            Js_of_ocaml_lwt.Lwt_js_events.clicks w#root (fun _ _ ->
                let name = get_board_name self#board in
                let widget = self#make_settings_widget () in
                push_settings (widget, name);
                Lwt.return_unit)
          in
          _click_listener <- Some listener)
        header#settings_icon

    method! destroy () : unit =
      super#destroy ();
      Option.iter Lwt.cancel _click_listener;
      _click_listener <- None

    method! layout () : unit =
      super#layout ();
      header#layout ()

    method settings_event : (Widget.t * string) React.event = e_settings

    method board : Topology.topo_board = React.S.value state

    method notify : event -> unit =
      function
      | `State x -> (
          push_state x;
          super#set_state x.connection;
          match x.connection with
          | `Fine -> self#set_ports x.ports
          | _ -> List.iter (fun p -> p#set_state `Unavailable) self#paths)

    (* Private methods *)
    method private make_settings_widget () : Widget.t =
      Widget.create
      @@ Components_lab.Loader.make_widget_loader
      @@ make_board_page state socket

    method private set_ports (l : Topology.topo_port list) : unit =
      let find (port : Topology.topo_port) (p : Topo_path.t) : bool =
        eq_node_entry p#left_node (`Entry port.child)
      in
      List.iter
        (fun (x : Topology.topo_port) ->
          match List.find_opt (find x) self#paths with
          | None -> ()
          | Some path ->
              let state =
                match x.has_sync, x.listening with
                | true, true -> `Sync
                | false, true -> `Sync_lost
                | _, _ -> `Muted
              in
              path#set_state state)
        l
  end

let create ~connections (socket : Api_js.Websocket.JSON.t) (board : Topology.topo_board)
    =
  new t ~connections socket board
