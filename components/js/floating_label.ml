open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Floating_label.Make(Xml)(Svg)(Html)

type event = Dom_html.animationEvent Js.t

class t ?(for_ : string option) (label : string) () =
  let elt = To_dom.of_element @@ Markup.create ?for_ label () in
  object(self)

    val mutable _listener = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      super#listen_lwt Widget.Event.animationend (fun e _ ->
          Lwt.return @@ self#shake_animation_end_handler e)
      |> fun x -> _listener <- Some x

    method! destroy () : unit =
      super#destroy ();
      Option.iter Lwt.cancel _listener;
      _listener <- None

    method shake (should_shake : bool) : unit =
      super#toggle_class ~force:should_shake Markup.shake_class

    method float (should_float : bool) : unit =
      if should_float
      then super#add_class Markup.float_above_class
      else (super#remove_class Markup.float_above_class;
            super#remove_class Markup.shake_class)

    method width : int =
      super#offset_width

    (* Private methods *)

    method private shake_animation_end_handler (_ : event) : unit =
      super#remove_class Markup.shake_class

  end
