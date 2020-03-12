open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Components_lab_tyxml.Placeholder
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

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
            @@ D.placeholder_text ~loading ~text:(`Text s) ()
          in
          Dom.appendChild super#root text
      | Some x ->
          x##.textContent := Js.some @@ Js.string s;
          if loading then
            Dom.appendChild x
              (Tyxml_js.To_dom.of_element @@ D.placeholder_dots ())
  end

let attach (elt : #Dom_html.element Js.t) : t =
  new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?a ?error ?loading ?icon ?text () =
  D.placeholder ?classes ?a ?error ?loading ?icon ?text ()
  |> Tyxml_js.To_dom.of_element
  |> attach

let make_progress ?classes ?a ?(text = `Text "Загрузка") ?size ?icon ()
    =
  let icon =
    match icon with
    | None -> Circular_progress.D.circular_progress ?size ~indeterminate:true ()
    | Some x -> x
  in
  let x = make ?classes ?a ~loading:true ~icon ~text () in
  x

let make_error ?classes ?a ?icon ?text () =
  let icon =
    match icon with None -> Icon.D.SVG.icon ~d:error_svg_path () | Some x -> x
  in
  make ?classes ?a ~error:true ~icon ?text ()
