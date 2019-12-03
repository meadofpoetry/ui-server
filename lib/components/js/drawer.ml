open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Drawer
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

module Parent = Side_sheet.Make_parent (struct
  include CSS

  let slide = `Leading
end)

class drawer (elt : Dom_html.element Js.t) () =
  object
    inherit Parent.t elt ()

    method! private focus_active_navigation_item () : unit =
      "." ^ Components_tyxml.Item_list.CSS.item_activated
      |> Element.query_selector elt
      |> fun i -> Option.iter (fun e -> e##focus) i
  end

include (Parent : module type of Parent with type t = drawer)

(** Attach widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?children () =
  D.drawer ?classes ?a ?children () |> Tyxml_js.To_dom.of_aside |> attach
