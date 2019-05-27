open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Floating_label
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t (elt : Dom_html.element Js.t) () =
object(self)
  val mutable _animationend_listener = None

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    let listener =
      Events.listen_lwt super#root Events.Typ.animationend (fun _ _ ->
          self#handle_shake_animation_end ();
          Lwt.return_unit) in
    _animationend_listener <- Some listener

  method! destroy () : unit =
    super#destroy ();
    Option.iter Lwt.cancel _animationend_listener;
    _animationend_listener <- None

  method shake (should_shake : bool) : unit =
    super#toggle_class ~force:should_shake CSS.shake

  method float (should_float : bool) : unit =
    if should_float
    then super#add_class CSS.float_above
    else (super#remove_class CSS.float_above;
          super#remove_class CSS.shake)

  method width : int =
    super#root##.scrollWidth

  (* Private methods *)

  method private handle_shake_animation_end () : unit =
    super#remove_class CSS.shake
end

let float (x : t) v = x#float v

let shake (x : t) v = x#shake v

let make ?(for_ : string option) (label : string) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?for_ label () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
