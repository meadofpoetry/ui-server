open Containers
open Tyxml_js

module Markup = Components_markup.Floating_label.Make(Xml)(Svg)(Html)

type event = Dom_html.animationEvent Js.t

class t ?(for_ : string option) (label : string) () =
  let elt = Markup.create ?for_ label () |> To_dom.of_element in
  object(self)

    val mutable _listener = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      self#listen_lwt Widget.Event.animationend (fun e _ ->
          Lwt.return @@ self#shake_animation_end_handler e)
      |> fun x -> _listener <- Some x

    method! destroy () : unit =
      super#destroy ();
      Option.iter Lwt.cancel _listener;
      _listener <- None

    method shake (should_shake : bool) : unit =
      self#add_or_remove_class should_shake Markup.shake_class

    method float (should_float : bool) : unit =
      if should_float
      then self#add_class Markup.float_above_class
      else (self#remove_class Markup.float_above_class;
            self#remove_class Markup.shake_class)

    method width : int =
      self#offset_width

    (* Private methods *)

    method private shake_animation_end_handler (_ : event) : unit =
      self#remove_class Markup.shake_class

  end
