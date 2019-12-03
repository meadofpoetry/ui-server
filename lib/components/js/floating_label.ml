open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Floating_label
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val mutable listeners = []

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ seq_loop
              (make_event Dom_html.Event.animationend)
              super#root
              self#handle_shake_animation_end ]
          @ listeners);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

    method shake (should_shake : bool) : unit =
      super#toggle_class ~force:should_shake CSS.shake

    method float (should_float : bool) : unit =
      if should_float
      then super#add_class CSS.float_above
      else (
        super#remove_class CSS.float_above;
        super#remove_class CSS.shake)

    method width : int = super#root##.scrollWidth

    method private handle_shake_animation_end _ _ : unit Lwt.t =
      super#remove_class CSS.shake;
      Lwt.return_unit
  end

let float (x : t) v = x#float v

let shake (x : t) v = x#shake v

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?float_above ?for_ ?label ?children () =
  D.floating_label ?classes ?a ?float_above ?for_ ?label ?children ()
  |> Tyxml_js.To_dom.of_label
  |> attach
