open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Line_ripple
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      (* Attach event listeners *)
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ seq_loop
              (make_event @@ Dom_html.Event.make "transitionend")
              super#root
              (fun e _ -> Lwt.return @@ self#handle_transition_end e) ]
          @ listeners);
      super#init ()

    method! destroy () : unit =
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

    method activate () : unit =
      self#remove_class CSS.deactivating;
      self#add_class CSS.active
    (** Activates the line ripple *)

    method deactivate () : unit = self#add_class CSS.deactivating
    (** Deactivates the line ripple *)

    method set_ripple_center (x_coordinate : float) : unit =
      let value = Js.string @@ Printf.sprintf "%gpx center" x_coordinate in
      (Js.Unsafe.coerce super#root##.style)##.transformOrigin := value
    (** Sets the center of the ripple animation to the given X coordinate. *)

    (* Private methods *)
    method private handle_transition_end (e : Dom_html.event Js.t) : unit =
      let prop = Js.to_string (Js.Unsafe.coerce e)##.propertyName in
      if String.equal prop "opacity"
      then
        (* Wait for the line ripple to be either transparent or opaque
         before emitting the animation end event *)
        let is_deactivating = self#has_class CSS.deactivating in
        if is_deactivating
        then (
          self#remove_class CSS.active;
          self#remove_class CSS.deactivating)
    (** Handles a transition end event *)
  end

let activate (x : t) = x#activate ()

let deactivate (x : t) = x#deactivate ()

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?children () : t =
  D.line_ripple ?classes ?a ?children () |> Tyxml_js.To_dom.of_div |> attach

let make_r ?classes ?a ?children () : t =
  R.line_ripple ?classes ?a ?children () |> Tyxml_js.To_dom.of_div |> attach
