open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Tab_indicator
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

module Selector = struct
  let content = Printf.sprintf ".%s" CSS.content
end

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val content = Element.query_selector_exn elt Selector.content

    inherit Widget.t elt () as super

    method fade : bool = super#has_class CSS.fade

    method set_fade (x : bool) : unit = super#toggle_class ~force:x CSS.fade

    method active : bool = super#has_class CSS.active

    method set_active ?(previous : 'self option) (x : bool) : unit =
      if x
      then if self#fade then super#add_class CSS.active else self#activate_slide previous
      else super#remove_class CSS.active

    method private activate_slide : t option -> unit =
      function
      | None -> super#add_class CSS.active
      | Some prev ->
          let old_content = Element.query_selector_exn prev#root Selector.content in
          let old = old_content##getBoundingClientRect in
          let cur = content##getBoundingClientRect in
          (* Calculate the dimensions based on the dimensions of the previous
             indicator *)
          let width_delta =
            match Js.Optdef.to_option old##.width, Js.Optdef.to_option old##.width with
            | Some pw, Some cw -> pw /. cw
            | _ -> failwith "mdc_tab_indicator: width property not found"
          in
          let x_position = old##.left -. cur##.left in
          super#add_class CSS.no_transition;
          let s = Printf.sprintf "translateX(%gpx) scaleX(%g)" x_position width_delta in
          content##.style##.transform := Js.string s;
          (* Force repaint before updating classes and transform to ensure
             the transform properly takes effect *)
          content##getBoundingClientRect |> ignore;
          super#remove_class CSS.no_transition;
          super#add_class CSS.active;
          content##.style##.transform := Js.string ""
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?active ?fade ?icon ?content () =
  D.tab_indicator ?classes ?a ?active ?fade ?icon ?content ()
  |> Tyxml_js.To_dom.of_span
  |> attach
