open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

include Components_lab_tyxml.Placeholder
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let make ?classes ?attrs ?error ?loading widget text =
  let widget = Tyxml_js.Of_dom.of_element widget in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.make_simple ?classes ?attrs ?error ?loading widget text in
  Widget.create elt

let make_progress ?classes ?attrs ?(text = "Загрузка") ?size ?progress () =
  let progress = match progress with
    | None -> Widget.coerce @@ Circular_progress.make ?size ~indeterminate:true ()
    | Some x -> Widget.create x in
  let x = make ?classes ?attrs ~loading:true progress#root text in
  x#set_on_destroy (fun () -> progress#destroy ());
  x

let make_error ?classes ?attrs ?icon text =
  let icon = match icon with
    | None -> Widget.coerce @@ Icon.SVG.(make_simple error_svg_path)
    | Some x -> Widget.create x in
  make ?classes ?attrs ~error:true icon#root text
