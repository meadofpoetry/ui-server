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
            Tyxml_js.To_dom.of_element
            @@ Markup_js.create_text ~loading ~text:(`Text s) ()
          in
          Dom.appendChild super#root text
      | Some x ->
          x##.textContent := Js.some @@ Js.string s;
          if loading
          then Dom.appendChild x (Tyxml_js.To_dom.of_element @@ Markup_js.create_dots ())
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?attrs ?error ?loading ?icon ?text () =
  Markup_js.create ?classes ?attrs ?error ?loading ?icon ?text ()
  |> Tyxml_js.To_dom.of_element
  |> attach

let make_progress ?classes ?attrs ?(text = `Text "Загрузка") ?size ?icon () =
  let icon =
    match icon with
    | None -> Circular_progress.Markup_js.create ?size ~indeterminate:true ()
    | Some x -> x
  in
  let x = make ?classes ?attrs ~loading:true ~icon ~text () in
  x

let make_error ?classes ?attrs ?icon ?text () =
  let icon =
    match icon with
    | None -> Icon.SVG.Markup_js.create ~d:error_svg_path ()
    | Some x -> x
  in
  make ?classes ?attrs ~error:true ~icon ?text ()
