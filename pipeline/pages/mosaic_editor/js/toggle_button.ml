open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

module CSS = struct
  let root = "mdc-toggle-button"

  let group = root ^ "-group"

  let label = BEM.add_element root "label"

  let selected = BEM.add_modifier root "selected"
end

module Markup = struct

  open Tyxml_js.Html
  open Components_tyxml.Utils

  let create ?(classes = []) ?attrs ?(selected = false) ~content () =
    let classes =
      classes
      |> cons_if selected CSS.selected
      |> List.cons Button.CSS.root
      |> List.cons CSS.root in
    button ~a:([a_class classes] <@> attrs)
      [span ~a:[a_class [CSS.label]] content]

  let create_group ?(classes = []) ?attrs buttons =
    let classes = CSS.group :: classes in
    div ~a:([a_class classes] <@> attrs) buttons
end

module Event = struct
  class type change =
    object
      inherit [unit] Widget.custom_event
    end

  let change : change Js.t Dom_html.Event.typ =
    Dom_html.Event.make "toggle-button-group:change"
end

class t (elt : Dom_html.element Js.t) () = object
  inherit Button.t ~on_click:(fun _ _ _ ->
      ignore @@ Element.toggle_class elt CSS.selected;
      Lwt.return_unit) elt () as super

  method! private create_ripple () : Ripple.t =
    Ripple.attach ~unbounded:true super#root
end

let make ?selected content : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?selected ~content () in
  new t elt ()

let make_group (buttons : t list) =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create_group (List.map Widget.to_markup buttons) in
  Widget.create elt

