open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Icon_button.Make(Xml)(Svg)(Html)

class t ?(on = false) ?(ripple = true) ?on_change ?on_icon ?disabled ~icon () =
  let state, set_state = React.S.create on in
  let elt = Markup.create
              ?on_icon:(Option.map Widget.to_markup on_icon)
              (Widget.to_markup icon) ()
            |> To_dom.of_button in
  object(self)

    val mutable on_change = on_change
    val mutable _ripple : Ripple.t option = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      Option.iter self#set_disabled disabled;
      if on then self#set_on true;
      if ripple
      then let ripple = Ripple.attach_to ~unbounded:true super#widget in
           _ripple <- Some ripple;
      Option.iter (fun i ->
          i#add_class Markup.icon_class;
          i#add_class Markup.icon_on_class) on_icon;
      icon#add_class Markup.icon_class;
      match on_icon with
      | None -> ()
      | Some _ ->
         super#listen_lwt' Widget.Event.click (fun _ _ ->
             self#toggle (); Lwt.return_unit)

    method! layout () : unit =
      super#layout ();
      Option.iter Ripple.layout _ripple

    method! destroy () : unit =
      super#destroy ();
      Option.iter Ripple.destroy _ripple;
      _ripple <- None

    method set_on_change (f : bool -> unit) : unit =
      on_change <- Some f

    method disabled : bool =
      Js.to_bool elt##.disabled

    method set_disabled (x : bool) : unit =
      elt##.disabled := Js.bool x

    method toggle () : unit =
      self#set_on (not self#on);
      set_state self#on;
      Option.iter (fun f -> f self#on) on_change

    method s_state : bool React.signal = state

    method on : bool =
      super#has_class Markup.on_class

    method set_on (x : bool) : unit =
      if not @@ Equal.bool self#on x
      then (
        Option.iter (fun f -> f x) on_change;
        set_state x;
        super#toggle_class ~force:x Markup.on_class)

  end

(** Create new icon button widget from scratch *)
let make ?on ?ripple ?on_change ?on_icon ?disabled ~icon () : t =
  let elt = To_dom.of_button @@ Markup.create ?on_icon icon () in
  Element.(To_dom.of_element icon)
  new t ?on ?ripple ?on_change ?on_icon ?disabled ~icon ()
