open Containers
open Components
open Topo_types
open Common

let port_section_height = 50
let base_class = "topology__block"
let fine_class = Markup.CSS.add_modifier base_class "fine"
let init_class = Markup.CSS.add_modifier base_class "init"
let fail_class = Markup.CSS.add_modifier base_class "fail"

module Header = struct

  let _class = Markup.CSS.add_element base_class "header"
  let action_class = Markup.CSS.add_element _class "action"

  class t ?action ?subtitle ~title () =
    let title_w = new Card.Primary.title title () in
    let subtitle_w =
      Option.map (fun x ->
          (new Card.Primary.subtitle x ())#widget)
        subtitle in
    let box =
      new Vbox.t
        ~widgets:([]
                  |> List.cons_maybe subtitle_w
                  |> List.cons title_w#widget)
        () in
    let widgets =
      []
      |> List.cons_maybe @@ Option.map Widget.coerce action
      |> List.cons box#widget in
    object(self)

      inherit Card.Primary.t ~widgets () as super

      method init () : unit =
        super#init ();
        Option.iter (fun a -> a#add_class action_class) action;
        self#add_class _class

    end

end

module Body = struct

  let _class = Markup.CSS.add_element base_class "body"

  class t n () =
    let elt = Js_of_ocaml.Dom_html.(createDiv document) in
    object(self)
      inherit Widget.t elt () as super

      method init () : unit =
        super#init ();
        self#set_n n;
        self#add_class _class;

      method set_n n =
        self#style##.height := Utils.px_js (n * port_section_height)

    end

end

class virtual t ~port_setter
        ~(connections : (#Topo_node.t * connection_point) list)
        ~(node : Topo_node.node_entry)
        ~(header : #Header.t)
        ~(body : #Body.t)
        () =
  let card = new Card.t ~widgets:[header#widget; body#widget] () in
  object(self)
    inherit Topo_node.parent
              ~port_setter
              ~node
              ~connections
              ~body:body#root
              card#root () as super

    method virtual settings_event : (Widget.t * string) React.event

    method private set_state : Topology.state -> unit = function
      | `Fine ->
         self#add_class fine_class;
         self#remove_class init_class;
         self#remove_class fail_class
      | `Init ->
         self#add_class init_class;
         self#remove_class fine_class;
         self#remove_class fail_class
      | `No_response ->
         self#add_class fail_class;
         self#remove_class init_class;
         self#remove_class fine_class

    method init () : unit =
      super#init ();
      body#set_n @@ List.length connections;
      self#add_class base_class

  end
