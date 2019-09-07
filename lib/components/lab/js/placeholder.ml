open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Components_lab_tyxml.Placeholder
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

module Selector = struct
  let icon = Printf.sprintf ".%s > *:not(.%s)" CSS.content CSS.text

  let text = Printf.sprintf ".%s" CSS.text
end

class t (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t elt () as super

    method icon : Dom_html.element Js.t option =
      Element.query_selector super#root Selector.icon

    method set_error (x : bool) : unit = super#toggle_class ~force:x CSS.error

    method set_text ?(loading = false) s =
      match Element.query_selector elt Selector.text with
      | None ->
          let text =
            Tyxml_js.To_dom.of_element @@ Markup_js.create_text_string ~loading s
          in
          Dom.appendChild super#root text
      | Some x ->
          x##.textContent := Js.some @@ Js.string s;
          if loading
          then Dom.appendChild x (Tyxml_js.To_dom.of_element @@ Markup_js.create_dots ())
  end

let make ?classes ?attrs ?error ?loading widget text =
  let widget = Tyxml_js.Of_dom.of_element widget in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup_js.create_simple ?classes ?attrs ?error ?loading widget text
  in
  new t elt ()

let make_progress ?classes ?attrs ?(text = "Загрузка") ?size ?progress () =
  let progress =
    match progress with
    | None -> Widget.coerce @@ Circular_progress.make ?size ~indeterminate:true ()
    | Some x -> Widget.coerce x
  in
  let x = make ?classes ?attrs ~loading:true progress#root text in
  x#set_on_destroy (fun () -> progress#destroy ());
  x

let make_error ?classes ?attrs ?icon text =
  let icon =
    match icon with
    | None -> Tyxml_js.To_dom.of_element @@ Icon.SVG.Markup_js.create_of_d error_svg_path
    | Some x -> x
  in
  make ?classes ?attrs ~error:true icon text

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t) ()
