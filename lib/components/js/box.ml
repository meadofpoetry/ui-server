open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Box
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

class t (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t elt () as super

    method set_vertical (x : bool) : unit =
      if x
      then (
        super#remove_class CSS.vertical;
        super#add_class CSS.horizontal)
      else (
        super#remove_class CSS.horizontal;
        super#add_class CSS.vertical)

    method set_wrap (x : wrap) : unit = super#add_class @@ CSS.wrap x

    method set_justify_content x = super#add_class @@ CSS.justify_content x

    method set_align_items x = super#add_class @@ CSS.align_items x

    method set_align_content x = super#add_class @@ CSS.align_content x
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t) ()

let make
    ?classes
    ?a
    ?tag
    ?justify_content
    ?align_items
    ?align_content
    ?wrap
    ?vertical
    ?children
    () =
  D.box
    ?classes
    ?a
    ?tag
    ?justify_content
    ?align_items
    ?align_content
    ?wrap
    ?vertical
    ?children
    ()
  |> Tyxml_js.To_dom.of_element
  |> attach
