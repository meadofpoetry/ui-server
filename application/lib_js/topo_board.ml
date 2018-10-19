open Containers
open Components
open Topo_types
open Lwt_result.Infix
open Common

let base_class = "topology__board"

let rec eq_port p1 p2 =
  let open Common.Topology in
  (p1.port = p2.port)
  && (match p1.child,p2.child with
      | Input i1, Input i2 -> equal_topo_input i1 i2
      | Board b1, Board b2 -> eq_board b1 b2
      | _                  -> false)

and eq_board b1 b2 =
  let open Topology in
  equal_board_type b1.typ b2.typ
  && String.equal b1.model b2.model
  && String.equal b1.manufacturer b2.manufacturer
  && equal_version b1.version b2.version
  && (b1.control = b2.control)
  && equal_env b1.env b2.env
  && (Equal.list eq_port) b1.ports b2.ports

let eq_node_entry (e1 : Topo_node.node_entry) (e2 : Topo_node.node_entry) =
  let open Common.Topology in
  match e1, e2 with
  | `CPU c1, `CPU c2 -> equal_topo_cpu c1 c2
  | `Entry e1, `Entry e2 ->
     begin match e1,e2 with
     | Board b1, Board b2 -> eq_board b1 b2
     | Input i1, Input i2 -> equal_topo_input i1 i2
     | _ -> false
     end
  | _ -> false

let get_board_name (board : Topology.topo_board) = match board.typ with
  | "IP2TS" -> "Приёмник TSoIP"
  | "TS2IP" -> "Передатчик TSoIP"
  | "TS" -> "Анализатор TS"
  | "DVB" -> "Приёмник DVB"
  | s -> s

let port_setter (b : Topology.topo_board) port state =
  match b.typ, b.model, b.manufacturer, b.version with
  | "TS", "qos", "niitv", 1 ->
     Board_qos_niit_js.Requests.Device.HTTP.post_port ~port ~state b.control
     |> Lwt_result.map (fun _ -> ())
     |> Lwt_result.map_err (fun _ -> "failed switching port")
  | "DVB", "rf", "niitv", 1 -> Lwt_result.fail "ports not switchable"
  | "TS2IP", "ts2ip", "niitv", 1 -> Lwt_result.fail "ports not switchable"
  | "IP2TS", "dtm-3200", "dektec", 1 -> Lwt_result.fail "ports not switchable"
  | _ -> Lwt_result.fail "Unknown board"

let make_board_page (board : Topology.topo_board) =
  match board.typ, board.model, board.manufacturer, board.version with
  | "TS", "qos", "niitv", 1 ->
     let open Board_qos_niit_js in
     let getter = fun () ->
       Topo_page.make board
       |> Ui_templates.Loader.create_widget_loader
       |> Widget.coerce in
     Some getter
  | "DVB", "rf", "niitv", 1 ->
     let open Board_dvb_niit_js in
     let getter = fun () ->
       let factory = new Widget_factory.t board.control () in
       let ({ widget; _ } : 'a Dashboard.Item.item) = factory#create Settings in
       widget#set_on_destroy @@ Some factory#destroy;
       widget#widget in
     Some getter
  | "TS2IP", "ts2ip", "niitv", 1 ->
     None (* FIXME *)
  | "IP2TS", "dtm-3200", "dektec", 1 ->
     let open Board_ip_dektec_js in
     let getter = fun () ->
       let factory = new Widget_factory.t board.control () in
       let ({ widget; _ } : 'a Dashboard.Item.item) =
         factory#create @@ Settings None in
       widget#set_on_destroy @@ Some factory#destroy;
       widget#widget in
     Some getter
  | _ -> None

module Header = struct

  class t (has_settings_button : bool)
          (board : Topology.topo_board) () =
    let _class = Markup.CSS.add_element base_class "header" in
    let title = get_board_name board in
    let subtitle = Printf.sprintf "%s" board.model in
    let settings = match has_settings_button with
      | false -> None
      | true ->
         let icon = Icon.SVG.(create_simple Path.settings) in
         let button = new Icon_button.t ~icon () in
         Some button in
    object(self)
      inherit Topo_block.Header.t ?action:settings ~title ~subtitle () as super

      method init () : unit =
        super#init ();
        self#add_class _class

      method settings_icon =
        settings

      method layout () : unit =
        super#layout ();
        Option.iter (fun x -> x#layout ()) self#settings_icon

    end

  let create (has_settings_button : bool)
        (board : Topology.topo_board) =
    new t has_settings_button board ()

end

module Body = struct

  class t (board : Topology.topo_board) () =
    let _class = Markup.CSS.add_element base_class "body" in
    object(self)
      inherit Topo_block.Body.t (List.length board.ports) ()
      initializer
        self#add_class _class
    end

  let create (board : Topology.topo_board) =
    new t board ()

end

class t ~(connections : (#Topo_node.t * connection_point) list)
        (board : Topology.topo_board)
        () =
  let e_settings, push_settings = React.E.create () in
  let eq = Topology.equal_state in
  let s, push = React.S.create ~eq board.connection in
  let make_settings = make_board_page board in
  let header = Header.create (Option.is_some make_settings) board in
  let body = Body.create board in
  object(self)
    val mutable _board = board
    val mutable _click_listener = None

    inherit Topo_block.t
              ~port_setter:(port_setter board)
              ~node:(`Entry (Board board))
              ~connections ~header ~body () as super

    method init () : unit =
      super#init ();
      self#set_board _board;
      self#add_class base_class;
      self#set_attribute "data-board" _board.typ;
      Option.iter (fun (w : #Widget.t) ->
          let listener =
            w#listen_click_lwt (fun _ _ ->
                let name = get_board_name self#board in
                let widget = self#make_settings_widget () in
                push_settings (widget, name);
                Lwt.return_unit) in
          _click_listener <- Some listener)
        header#settings_icon

    method destroy () : unit =
      super#destroy ();
      Option.iter Lwt.cancel _click_listener;
      _click_listener <- None

    method layout () :unit =
      super#layout ();
      header#layout ()

    method settings_event : (Widget.t * string) React.event =
      e_settings

    method s_state = s

    method board = _board

    method set_board x =
      _board <- x;
      push x.connection;
      super#set_state x.connection;
      match x.connection with
      | `Fine -> self#set_ports x.ports;
      | _ -> List.iter (fun p -> p#set_state `Unavailable)
               self#paths

    (* Private methods *)

    method private make_settings_widget () : Widget.t =
      match make_settings with
      | None ->
         let icon = Icon.SVG.(create_simple Path.stop) in
         let ph =
           Ui_templates.Placeholder.create_with_icon
             ~icon
             ~text:"Нет доступных настроек для платы"
             () in
         ph#widget
      | Some make -> make ()

    method private set_ports l  =
      List.iter (fun (x : Topology.topo_port) ->
          match List.find_opt (fun p -> eq_node_entry p#left_node
                                          (`Entry x.child)) self#paths with
          | Some path ->
             let state = match x.has_sync, x.listening with
               | true, _ -> `Sync
               | _, true -> `Sync_lost
               | _, _    -> `Muted in
             path#set_state state
          | None      -> ()) l
  end

let create ~connections (board : Topology.topo_board) =
  new t ~connections board ()
