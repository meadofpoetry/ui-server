open Components

let port_section_height = 50
let base_class          = "topology__board"

module Header = struct

  class t (board:Common.Topology.topo_board) () =
    let _class     = Markup.CSS.add_element base_class "header" in
    let title      = Printf.sprintf "%s" board.typ in
    let subtitle   = Printf.sprintf "%s" board.model in
    let title_w    = new Card.Primary.title title () in
    let subtitle_w = new Card.Primary.subtitle subtitle () in
    let box        = new Box.t ~vertical:true ~widgets:[title_w#widget;subtitle_w#widget] () in
    let settings   = new Icon.Button.Font.t ~icon:"settings" () in
    object(self)
      inherit Card.Primary.t ~widgets:[box#widget;settings#widget] ()
      initializer
        self#add_class _class;
        method settings_icon = settings
    end

  let create (board:Common.Topology.topo_board) =
    new t board ()

end

module Body = struct

  class t (board:Common.Topology.topo_board) () =
  object(self)
    inherit Card.Media.t ~widgets:[] ()
    initializer
      self#style##.height := Js.string @@ Utils.px ((List.length board.ports) * port_section_height)
  end

  let create (board:Common.Topology.topo_board) =
    new t board ()

end

class t (board:Common.Topology.topo_board) () =
  let header     = Header.create board in
  let body       = Body.create board in
  let e_settings = React.E.map (fun _ -> board) header#settings_icon#e_click in
  object(self)
    inherit Card.t ~widgets:[header#widget;body#widget] ()
    method e_settings = e_settings
    initializer
      self#add_class base_class;
      self#set_attribute "data-board" board.typ
  end

let create (board:Common.Topology.topo_board) =
  new t board ()
