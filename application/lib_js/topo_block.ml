open Containers
open Components
open Topo_types

let port_section_height = 50
let base_class          = "topology__block"
let fine_class          = Markup.CSS.add_modifier base_class "fine"
let init_class          = Markup.CSS.add_modifier base_class "init"
let fail_class          = Markup.CSS.add_modifier base_class "fail"

module Header = struct

  class t ?action ?subtitle ~title () =
    let _class     = Markup.CSS.add_element base_class "header" in
    let title_w    = new Card.Primary.title title () in
    let subtitle_w = Option.map (fun x -> (new Card.Primary.subtitle x ())#widget) subtitle in
    let box        = new Box.t
                         ~vertical:true
                         ~widgets:([]
                                   |> List.cons_maybe subtitle_w
                                   |> List.cons title_w#widget)
                         () in
    object(self)
      inherit Card.Primary.t ~widgets:([]
                                       |> List.cons_maybe action
                                       |> List.cons box#widget)
                             ()
      initializer
        Option.iter (fun a -> a#add_class @@ Markup.CSS.add_element _class "action") action;
        self#add_class _class
    end

end

module Body = struct

  class t n () =
    let _class = Markup.CSS.add_element base_class "body" in
    let elt    = Dom_html.createDiv Dom_html.document in
    object(self)
      inherit Widget.widget elt ()
      method set_n n =
        self#style##.height := Js.string @@ Utils.px (n * port_section_height)

      initializer
        self#set_n n;
        self#add_class _class;
    end

end

class t ~port_setter
        ~(connections:(#Topo_node.t * connection_point) list)
        ~(node:Topo_node.node_entry)
        ~(header:#Header.t)
        ~(body:#Body.t)
        () =
  let card = new Card.t ~widgets:[header#widget;body#widget] () in
  object(self)
    inherit Topo_node.parent ~port_setter ~node ~connections ~body:body#root card#root ()

    method private set_state : Common.Topology.state -> unit = function
      | `Fine        -> self#add_class    fine_class;
                        self#remove_class init_class;
                        self#remove_class fail_class
      | `Init        -> self#add_class    init_class;
                        self#remove_class fine_class;
                        self#remove_class fail_class
      | `No_response -> self#add_class    fail_class;
                        self#remove_class init_class;
                        self#remove_class fine_class
    initializer
      body#set_n @@ List.length connections;
      self#add_class base_class
  end
