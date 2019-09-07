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

let cons_maybe x l =
  match x with
  | None -> l
  | Some x -> x :: l

module Header = struct
  class t ?action ?subtitle ~title () =
    let title_w = Tyxml_js.To_dom.of_element @@ Card.Markup_js.create_title title in
    let subtitle_w =
      Option.map (Tyxml_js.To_dom.of_element % Card.Markup_js.create_subtitle) subtitle
    in
    (* FIXME do not create widgets *)
    let box =
      Box.make ~dir:`Column
      @@ List.map Widget.create ([] |> cons_maybe subtitle_w |> List.cons title_w)
    in
    let widgets =
      [] |> cons_maybe @@ Option.map Widget.coerce action |> List.cons box#widget
    in
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Card.Markup_js.create_primary (List.map Widget.to_markup widgets)
    in
    object (self)
      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        Option.iter (fun a -> a#add_class CSS.header_action) action;
        self#add_class CSS.header
    end
end

module Body = struct
  class t n () =
    object (self)
      inherit Widget.t Dom_html.(createDiv document) () as super

      method! init () : unit =
        super#init ();
        super#add_class CSS.body;
        self#set_n n

      method set_n n =
        let height = n * port_section_height in
        super#root##.style##.height := Js.string @@ Printf.sprintf "%dpx" height
    end
end

class virtual t
  ~port_setter
  ~(connections : (#Topo_node.t * connection_point) list)
  ~(node : Topo_node.node_entry)
  ~(header : #Header.t)
  ~(body : #Body.t)
  () =
  object
    inherit
      Topo_node.parent
        ~port_setter ~node ~connections ~body:body#root
        (Tyxml_js.To_dom.of_element @@ Card.Markup_js.create [header#markup; body#markup])
        () as super

    method! init () : unit =
      super#add_class CSS.root;
      body#set_n @@ List.length connections;
      super#init ()

    method! destroy () : unit =
      header#destroy ();
      body#destroy ();
      super#destroy ()

    method virtual settings_event : (Widget.t * string) React.event

    method private set_state : Application_types.Topology.state -> unit =
      function
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
