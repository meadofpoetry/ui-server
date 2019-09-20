open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Components_lab_tyxml.Glide
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

module Selector = struct
  let slide = "." ^ CSS.slide

  let track = "." ^ CSS.track
end

let iter_nodes f (nodes : _ #Dom.nodeList Js.t) =
  let rec aux = function
    | i when i = nodes##.length -> ()
    | i ->
        f (Js.Opt.get (nodes##item i) (fun () -> assert false));
        aux (succ i)
  in
  aux 0

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val track = Element.query_selector_exn elt Selector.track

    val transform =
      Animation.get_correct_property_name ~window:Dom_html.window "transform"

    val mutable active = 0

    val mutable gap = 0
    (** A size of the gap added between slides. *)

    val mutable per_view = 1
    (** A number of slides visible on the single viewport. *)

    inherit Widget.t elt () as super

    method! init () : unit =
      self#set_items_style ();
      super#init ()

    method! layout () =
      self#set_items_style ();
      super#layout ()

    method set_per_view ?(layout = true) (n : int) =
      per_view <- n;
      if layout then self#layout ()

    method set_gap ?(layout = true) (n : int) =
      gap <- n;
      if layout then self#layout ()

    method set_active (n : int) : unit =
      let shift = -.(100. /. float_of_int per_view) *. float_of_int n in
      let style = Printf.sprintf "translate3d(%g%%, 0, 0)" shift in
      Element.set_style_property track transform style

    method items : Dom_html.element Js.t list =
      Element.query_selector_all super#root Selector.slide

    method private items_ : Dom_html.element Dom.nodeList Js.t =
      super#root##querySelectorAll (Js.string Selector.slide)

    method private set_items_style () =
      let pct = 100. /. float_of_int per_view in
      let margin = float_of_int gap /. 2. in
      let width =
        match gap with
        | 0 -> Printf.sprintf "%g%%" pct
        | n -> Printf.sprintf "calc(%g%% - %dpx)" pct n
      in
      let style =
        Printf.sprintf "width: %s; min-width: %s; margin: 0 %gpx" width width margin
      in
      iter_nodes (fun item -> item##.style##.cssText := Js.string style) self#items_
  end

let attach elt = new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?a ?slides ?children () =
  D.glide ?classes ?a ?slides ?children () |> Tyxml_js.To_dom.of_div |> attach
