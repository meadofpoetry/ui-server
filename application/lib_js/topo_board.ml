open Components

let port_section_height = 50
let base_class          = "topology__board"

module Header = struct

  class t (board:Common.Topology.topo_board) () =
    let _class     = Markup.CSS.add_element base_class "header" in
    let title      = Printf.sprintf "%s" board.typ in
    let subtitle   = Printf.sprintf "%s" board.model in
    let settings   = new Icon.Button.Font.t ~icon:"settings" () in
    object(self)
      inherit Topo_node.Header.t ~action:settings#widget ~title ~subtitle ()
      initializer
        self#add_class _class;
      method settings_icon = settings
    end

  let create (board:Common.Topology.topo_board) =
    new t board ()

end

module Body = struct

  class t (board:Common.Topology.topo_board) () =
  object
    inherit Topo_node.Body.t (List.length board.ports) ()
  end

  let create (board:Common.Topology.topo_board) =
    new t board ()

end

class t (board:Common.Topology.topo_board) () =
  let header     = Header.create board in
  let body       = Body.create board in
  let e_settings = React.E.map (fun _ -> board) header#settings_icon#e_click in
  object(self)
    inherit Topo_node.t ~header ~body ()
    method e_settings = e_settings
    initializer
      self#add_class base_class;
      self#set_attribute "data-board" board.typ
  end

let create (board:Common.Topology.topo_board) =
  new t board ()
