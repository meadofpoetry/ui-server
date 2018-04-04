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

class t ~(s_state:Common.Topology.state React.signal)
        ~(connections:#Topo_node.t list)
        (board:Common.Topology.topo_board) () =
  let header     = Header.create board in
  let body       = Body.create board in
  let e_settings = React.E.map (fun _ -> s_state,board) header#settings_icon#e_click in
  object(self)
    val mutable board = board
    inherit Topo_block.t ~s_state ~connections ~header ~body ()
    method board      = board
    method e_settings = e_settings
    initializer
      self#add_class base_class;
      self#set_attribute "data-board" board.typ
  end

let create ~connections ~s_state (board:Common.Topology.topo_board) =
  new t ~connections ~s_state board ()
