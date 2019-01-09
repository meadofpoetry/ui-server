open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Icon_button.Make(Xml)(Svg)(Html)

class t ?(on = false) ?(ripple = true) ?on_change ?on_icon ?disabled ~icon () =
  let state, set_state = React.S.create on in
  let elt = Markup.create
              ?on_icon:(Option.map Widget.to_markup on_icon)
              (Widget.to_markup icon) ()
            |> To_dom.of_button in
  object(self)

    val mutable _ripple : Ripple.t option = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      Option.iter self#set_disabled disabled;
      if on then self#set_on true;
      if ripple
      then
        (let ripple = Ripple.attach_to ~unbounded:true (self :> Widget.t) in
         _ripple <- Some ripple);
      Option.iter (fun i ->
          i#add_class Markup.icon_class;
          i#add_class Markup.icon_on_class) on_icon;
      icon#add_class Markup.icon_class;
      match on_icon with
      | None -> ()
      | Some _ ->
         (* FIXME keep *)
         self#listen_lwt Widget.Event.click (fun _ _ ->
             self#toggle ();
             Lwt.return_unit) |> Lwt.ignore_result

    method! layout () : unit =
      super#layout ();
      Option.iter (fun r -> r#layout ()) _ripple

    method! destroy () : unit =
      super#destroy ();
      Option.iter (fun r -> r#destroy ()) _ripple;
      _ripple <- None

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
      self#has_class Markup.on_class
    method set_on (x : bool) : unit =
      if not @@ Equal.bool self#on x
      then Option.iter (fun f -> f x) on_change;
      set_state x;
      self#add_or_remove_class x Markup.on_class

end
