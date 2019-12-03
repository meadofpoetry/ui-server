open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Linear_progress
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

module Selector = struct
  let buffer = Printf.sprintf ".%s" CSS.buffer

  let primary_bar = Printf.sprintf ".%s" CSS.primary_bar
end

class t (elt : Dom_html.element Js.t) () =
  object (self)
    inherit Widget.t elt () as super

    val mutable progress = 0.

    val transform : Js.js_string Js.t =
      Js.string
      @@ Animation.get_correct_property_name ~window:Dom_html.window "transform"

    val buffer = Element.query_selector elt Selector.buffer

    val primary_bar = Element.query_selector elt Selector.primary_bar

    method indeterminate : bool = super#has_class CSS.indeterminate

    method set_indeterminate (indeterminate : bool) =
      super#toggle_class ~force:indeterminate CSS.indeterminate;
      if indeterminate
      then (
        self#set_scale 1. primary_bar;
        self#set_scale 1. buffer)
      else self#set_scale progress primary_bar

    method set_progress x =
      progress <- x;
      if not @@ self#indeterminate then self#set_scale x primary_bar

    method set_buffer x = if not @@ self#indeterminate then self#set_scale x buffer

    method reversed : bool = super#has_class CSS.reversed

    method set_reverse (x : bool) : unit = super#toggle_class ~force:x CSS.reversed

    method open_ () : unit Lwt.t =
      if not @@ super#has_class CSS.closed
      then Lwt.return_unit
      else
        let thread = Js_of_ocaml_lwt.Lwt_js_events.transitionend super#root in
        super#remove_class CSS.closed;
        Lwt.pick [Js_of_ocaml_lwt.Lwt_js.sleep 1.; thread]

    method close () : unit Lwt.t =
      if super#has_class CSS.closed
      then Lwt.return_unit
      else
        let thread = Js_of_ocaml_lwt.Lwt_js_events.transitionend super#root in
        super#add_class CSS.closed;
        Lwt.pick [Js_of_ocaml_lwt.Lwt_js.sleep 1.; thread]

    method private set_scale value =
      function
      | None -> ()
      | Some (elt : Dom_html.element Js.t) ->
          let scale = Js.string @@ Printf.sprintf "scaleX(%g)" value in
          (Js.Unsafe.coerce elt##.style)##setProperty transform scale
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t) ()

let make
    ?classes
    ?a
    ?indeterminate
    ?reversed
    ?closed
    ?buffering_dots
    ?buffer
    ?primary_bar
    ?secondary_bar
    ?children
    () : t =
  D.linear_progress
    ?classes
    ?a
    ?indeterminate
    ?reversed
    ?closed
    ?buffering_dots
    ?buffer
    ?primary_bar
    ?secondary_bar
    ?children
    ()
  |> Tyxml_js.To_dom.of_div
  |> attach
