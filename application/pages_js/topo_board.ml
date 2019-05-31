open Application_types
open Components
open Topo_types

let base_class = "topology__board"

let rec eq_port
    (p1 : Topology.topo_port)
    (p2 : Topology.topo_port) =
  (p1.port = p2.port)
  && (match p1.child,p2.child with
      | Input i1, Input i2 -> Topology.equal_topo_input i1 i2
      | Board b1, Board b2 -> eq_board b1 b2
      | _, _ -> false)

and eq_board
    (b1 : Topology.topo_board)
    (b2 : Topology.topo_board) =
  let open Topology in
  String.equal b1.model b2.model
  && String.equal b1.manufacturer b2.manufacturer
  && equal_version b1.version b2.version
  && (b1.control = b2.control)

let eq_node_entry (e1 : Topo_node.node_entry) (e2 : Topo_node.node_entry) =
  match e1, e2 with
  | `CPU c1, `CPU c2 -> Topology.equal_topo_cpu c1 c2
  | `Entry e1, `Entry e2 ->
     begin match e1,e2 with
     | Board b1, Board b2 -> eq_board b1 b2
     | Input i1, Input i2 -> Topology.equal_topo_input i1 i2
     | _ -> false
     end
  | _ -> false

let get_board_name ({ manufacturer; model; _ } : Topology.topo_board) =
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
let get_board_type ({ manufacturer; model; _ } : Topology.topo_board) =
  match manufacturer, model with
  | "DekTec", "DTM-3200" -> "IP2TS"
  | "NIITV", "TS2IP" -> "TS2IP"
  | "NIITV", "TSAN" -> "TS"
  | "NIITV", "DVB4CH" -> "DVB"
  | _ -> ""

let make_board_page (b : Topology.topo_board) =
  let ( >>= ) = Lwt.( >>= ) in
  let ( >>=? ) x f = Lwt_result.(
      map_err Api_js.Http.error_to_string @@ x >>= f) in
  match b.manufacturer, b.model, b.version with
  | "NIITV", "TSAN", _ ->
    let open Board_niitv_tsan_http_js in
    let open Board_niitv_tsan_widgets_js in
    Some (fun state socket ->
        let t =
          Http_device.get_t2mi_mode b.control
          >>=? fun mode ->
          let widget = Widget_t2mi_settings.make state mode b.control in
          Lwt.return_ok widget#widget in
        Ui_templates.Loader.create_widget_loader t)
  | "NIITV", "DVB4CH", _ ->
    let open Board_niitv_dvb_http_js in
    let open Board_niitv_dvb_widgets_js in
    Some (fun state socket ->
        let t =
          Http_device.get_mode b.control
          >>=? fun mode -> Http_device.get_info b.control
          >>=? fun info -> Http_device.Event.get_mode socket b.control
          >>= function
          | Error `Timeout _ -> Lwt.return_error "Timeout"
          | Error `Error (r, _) -> Lwt.return_error (Uri.to_string r)
          | Ok (id, event) ->
            let receivers = Some info.receivers in
            let widget = Widget_settings.make state mode receivers b.control in
            let event = React.E.map (fun x -> widget#notify (`Mode x)) event in
            widget#set_on_destroy (fun () -> React.E.stop ~strong:true event);
            Lwt.return_ok widget#widget in
        Ui_templates.Loader.create_widget_loader t)
  | "DekTec", "DTM-3200", _ ->
    let open Board_dektec_dtm3200_http_js in
    let open Board_dektec_dtm3200_widgets_js in
    Some (fun state socket ->
        let t =
          Http_device.get_config b.control
          >>=? fun { nw; ip_receive; _ } ->
          let widget = Widget_settings.make state nw ip_receive b.control in
          (* let event =
           *   React.E.map (fun (x : Board_dektec_dtm3200_types.config) ->
           *       widget#notify (`Ip_receive_mode x.ip_receive);
           *       widget#notify (`Nw_mode x.nw))
           *   @@ Http_device.Event.get_config socket b.control in
           * widget#set_on_destroy (fun () -> React.E.stop ~strong:true event); *)
          Lwt.return_ok widget#widget in
        Ui_templates.Loader.create_widget_loader t)
  | _ -> None

module Header = struct

  class t (has_settings_button : bool)
          (board : Topology.topo_board) () =
    let _class = BEM.add_element base_class "header" in
    let title = get_board_name board in
    let settings = match has_settings_button with
      | false -> None
      | true ->
         let icon = Icon.SVG.(make_simple Path.settings) in
         let button = Icon_button.make ~icon () in
         Some button in
    object(self)
      inherit Topo_block.Header.t ?action:settings ~title () as super

      method! init () : unit =
        super#init ();
        self#add_class _class

      method! layout () : unit =
        super#layout ();
        Utils.Option.iter Widget.layout self#settings_icon

      method settings_icon =
        settings
    end

  let create (has_settings_button : bool) (board : Topology.topo_board) : t =
    new t has_settings_button board ()

end

module Body = struct

  let _class = BEM.add_element base_class "body"

  class t (board : Topology.topo_board) () =
  object
    inherit Topo_block.Body.t (List.length board.ports) () as super
    method! init () : unit =
      super#init ();
      super#add_class _class
  end

  let create (board : Topology.topo_board) : t =
    new t board ()

end

type event =
  [ `State of Topology.topo_board
  ]

class t ~(connections : (#Topo_node.t * connection_point) list)
    (socket : Api_js.Websocket.JSON.t)
    (board : Topology.topo_board) =
  let e_settings, push_settings = React.E.create () in
  let make_board_page = make_board_page board in
  let header = Header.create (Utils.Option.is_some make_board_page) board in
  let body = Body.create board in
  object(self)
    val mutable _state = board
    val mutable _click_listener = None

    inherit Topo_block.t
        ~port_setter:(port_setter board)
        ~node:(`Entry (Board board))
        ~connections ~header ~body () as super

    method! init () : unit =
      super#init ();
      self#notify (`State _state);
      super#add_class base_class;
      super#set_attribute "data-board" @@ get_board_type _state;
      Utils.Option.iter (fun (w : Icon_button.t) ->
          let listener =
            Events.clicks w#root (fun _ _ ->
                let name = get_board_name self#board in
                let widget = self#make_settings_widget () in
                push_settings (widget, name);
                Lwt.return_unit) in
          _click_listener <- Some listener)
        header#settings_icon

    method! destroy () : unit =
      super#destroy ();
      Utils.Option.iter Lwt.cancel _click_listener;
      _click_listener <- None

    method! layout () : unit =
      super#layout ();
      header#layout ()

    method settings_event : (Widget.t * string) React.event = e_settings

    method board : Topology.topo_board = _state

    method notify : event -> unit = function
      | `State x ->
        _state <- x;
        super#set_state x.connection;
        match x.connection with
        | `Fine -> self#set_ports x.ports;
        | _ -> List.iter (fun p -> p#set_state `Unavailable) self#paths

    (* Private methods *)

    method private make_settings_widget () : Widget.t =
      match make_board_page with
      | None ->
        let icon = Icon.SVG.(make_simple Path.stop) in
        let ph =
          Ui_templates.Placeholder.With_icon.make
            ~icon
            ~text:"Нет доступных настроек для платы"
            () in
        ph#widget
      | Some make -> Widget.coerce @@ make _state.connection socket

    method private set_ports (l : Topology.topo_port list) : unit =
      let find (port : Topology.topo_port) (p : Topo_path.t) : bool =
        eq_node_entry p#left_node (`Entry port.child) in
      List.iter (fun (x : Topology.topo_port) ->
          match List.find_opt (find x) self#paths with
          | None -> ()
          | Some path ->
            let state = match x.has_sync, x.listening with
              | true, true -> `Sync
              | false, true -> `Sync_lost
              | _, _ -> `Muted in
            path#set_state state) l
  end

let create ~connections
    (socket : Api_js.Websocket.JSON.t)
    (board : Topology.topo_board) =
  new t ~connections socket board
