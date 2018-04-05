open Containers
open Components

let port_section_height = 50
let base_class          = "topology__board"

let get_board_name (board:Common.Topology.topo_board) = match board.typ with
  | "IP2TS" -> "Приёмник TSoIP"
  | "TS2IP" -> "Передатчик TSoIP"
  | "TS"    -> "Анализатор TS"
  | "DVB"   -> "Приёмник DVB"
  | s       -> s

module Header = struct

  class t (board:Common.Topology.topo_board) () =
    let _class     = Markup.CSS.add_element base_class "header" in
    let title      = get_board_name board in
    let subtitle   = Printf.sprintf "%s" board.model in
    let settings   = new Icon.Button.Font.t ~icon:"settings" () in
    object(self)
      inherit Topo_block.Header.t ~action:settings#widget ~title ~subtitle ()
      initializer
        self#add_class _class
      method settings_icon = settings
      method layout = self#settings_icon#layout
    end

  let create (board:Common.Topology.topo_board) =
    new t board ()

end

module Body = struct

  class t (board:Common.Topology.topo_board) () =
    let _class = Markup.CSS.add_element base_class "body" in
    object(self)
      inherit Topo_block.Body.t (List.length board.ports) ()
      initializer
        self#add_class _class
    end

  let create (board:Common.Topology.topo_board) =
    new t board ()

end

let rec eq_port p1 p2 =
  let open Common.Topology in
  (p1.port = p2.port)
  && (match p1.child,p2.child with
      | Input i1, Input i2 -> equal_topo_input i1 i2
      | Board b1, Board b2 -> eq_board b1 b2
      | _                  -> false)
and eq_board b1 b2 =
  let open Common.Topology in
  equal_board_type b1.typ b2.typ
  && String.equal b1.model b2.model
  && String.equal b1.manufacturer b2.manufacturer
  && equal_version b1.version b2.version
  && (b1.control = b2.control)
  && equal_env b1.env b2.env
  && (Equal.list eq_port) b1.ports b2.ports

let eq_node_entry (e1:Topo_node.node_entry) (e2:Topo_node.node_entry) =
  let open Common.Topology in
  match e1,e2 with
  | `CPU c1, `CPU c2     -> equal_topo_cpu c1 c2
  | `Entry e1, `Entry e2 -> (match e1,e2 with
                             | Board b1, Board b2 -> eq_board b1 b2
                             | Input i1, Input i2 -> equal_topo_input i1 i2
                             | _                  -> false)
  | _                    -> false

class t ~(connections:#Topo_node.t list)
        (board:Common.Topology.topo_board)
        () =
  let s,push     = React.S.create board.connection in
  let header     = Header.create board in
  let body       = Body.create board in
  let e_settings = React.E.map (fun _ -> board) header#settings_icon#e_click in
  object(self)
    val mutable _board = board

    inherit Topo_block.t ~node:(`Entry (Board board)) ~connections ~header ~body () as super

    method layout       = header#layout
    method e_settings   = e_settings
    method s_state      = s
    method board        = _board
    method set_board x  = _board <- x;
                          push x.connection;
                          super#set_state x.connection;
                          match x.connection with
                          | `Fine -> self#set_ports x.ports;
                          | _     -> List.iter (fun p -> p#set_state `Muted) self#paths

    method private set_ports l  =
      List.iter (fun (x:Common.Topology.topo_port) ->
          match List.find_opt (fun p -> eq_node_entry p#left_node (`Entry x.child)) self#paths with
          | Some path -> path#set_state (if x.listening then `Active else `Muted)
          | None      -> ()) l

    initializer
      self#set_board _board;
      self#add_class base_class;
      self#set_attribute "data-board" _board.typ
  end

let create ~connections (board:Common.Topology.topo_board) =
  new t ~connections board ()
