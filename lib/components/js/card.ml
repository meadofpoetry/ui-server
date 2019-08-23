open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Card
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

(* Sections *)

module Actions = struct

  let make_buttons widgets : Widget.t =
    List.iter (fun x ->
        x#add_class CSS.action;
        x#add_class CSS.action_button)
      widgets;
    let children = List.map Widget.to_markup widgets in
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_action_buttons children () in
    Widget.create elt

  let make_icons widgets : Widget.t =
    List.iter (fun x ->
        x#add_class CSS.action;
        x#add_class CSS.action_icon)
      widgets;
    let children = List.map Widget.to_markup widgets in
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_action_icons children () in
    Widget.create elt

  let make widgets : Widget.t =
    let children = List.map Widget.to_markup widgets in
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_actions children () in
    Widget.create elt

end

module Primary = struct

  let make_overline (text : string) : Typography.Text.t =
    let (elt : Dom_html.headingElement Js.t) =
      Tyxml_js.To_dom.of_h5
      @@ Markup.create_overline text () in
    Typography.Text.attach elt

  let make_title ?large (text : string) : Typography.Text.t =
    let (elt : Dom_html.headingElement Js.t) =
      Tyxml_js.To_dom.of_h2
      @@ Markup.create_title ?large text () in
    Typography.Text.attach elt

  let make_subtitle (text : string) : Typography.Text.t =
    let (elt : Dom_html.headingElement Js.t) =
      Tyxml_js.To_dom.of_h3
      @@ Markup.create_subtitle text () in
    Typography.Text.attach elt

  let make content : Widget.t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_section
      @@ Markup.create_primary (List.map Widget.to_markup content) () in
    Widget.create elt

end

module Media = struct

  let make widgets : Widget.t =
    let children = List.map Widget.to_markup widgets in
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_section
      @@ Markup.create_media children () in
    Widget.create elt

end

class t (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t elt () as super

    method outlined : bool =
      super#has_class CSS.outlined

    method set_outlined (x : bool) : unit =
      super#toggle_class ~force:x CSS.outlined
  end

let make ?outlined ?tag widgets : t =
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?outlined ?tag
    @@ List.map Widget.to_markup widgets in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
