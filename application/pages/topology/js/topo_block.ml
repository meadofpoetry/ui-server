open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Topo_types

let port_section_height = 50

module CSS = struct
  let root = "topology-block"
  let header = BEM.add_element root "header"
  let header_action = BEM.add_element header "action"
  let header_action_settings = BEM.add_modifier header_action "settings"
  let body = BEM.add_element root "body"

  let fine = BEM.add_modifier root "fine"
  let init = BEM.add_modifier root "init"
  let fail = BEM.add_modifier root "fail"
end

let ( % ) f g x = f (g x)

module Header = struct

  class t ?action ?subtitle ~title () =
    let title_w = Card.Primary.make_title title in
    let subtitle_w = Utils.Option.map
        (Widget.coerce % Card.Primary.make_subtitle)
        subtitle in
    let box =
      Box.make ~dir:`Column
        ([]
         |> Utils.List.cons_maybe subtitle_w
         |> List.cons title_w#widget) in
    let widgets =
      []
      |> Utils.List.cons_maybe @@ Utils.Option.map Widget.coerce action
      |> List.cons box#widget in
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Card.Markup.create_primary (List.map Widget.to_markup widgets) () in
    object(self)

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        Utils.Option.iter (fun a -> a#add_class CSS.header_action) action;
        self#add_class CSS.header

    end

end

module Body = struct

  class t n () =
    object(self)
      inherit Widget.t Dom_html.(createDiv document) () as super

      method! init () : unit =
        super#init ();
        super#add_class CSS.body;
        self#set_n n

      method set_n n =
        super#root##.style##.height := Utils.px_js (n * port_section_height)

    end

end

class virtual t ~port_setter
        ~(connections : (#Topo_node.t * connection_point) list)
        ~(node : Topo_node.node_entry)
        ~(header : #Header.t)
        ~(body : #Body.t)
        () =
  let card = Card.make [header#widget; body#widget] in
  object
    inherit Topo_node.parent
              ~port_setter
              ~node
              ~connections
              ~body:body#root
              card#root () as super

    method! init () : unit =
      super#init ();
      super#add_class CSS.root;
      body#set_n @@ List.length connections

    method virtual settings_event : (Widget.t * string) React.event

    method private set_state : Application_types.Topology.state -> unit = function
      | `Fine ->
         super#add_class CSS.fine;
         super#remove_class CSS.init;
         super#remove_class CSS.fail
      | `Init ->
         super#add_class CSS.init;
         super#remove_class CSS.fine;
         super#remove_class CSS.fail
      | `No_response | `Detect ->
         super#add_class CSS.fail;
         super#remove_class CSS.init;
         super#remove_class CSS.fine
  end
