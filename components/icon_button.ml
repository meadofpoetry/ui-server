open Containers
open Tyxml_js

module Markup = Components_markup.Icon_button.Make(Xml)(Svg)(Html)

class t ?(on = false) ?on_change ?on_icon ~icon () =
  let state, set_state = React.S.create on in
  let elt = Markup.create
              ?on_icon:(Option.map Widget.to_markup on_icon)
              (Widget.to_markup icon) ()
            |> To_dom.of_button in
  object(self)

    inherit Widget.t elt ()

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

    initializer
      if on then self#set_on true;
      (* FIXME implement ripple normally *)
      (let r = Ripple.attach self in
       self#add_class "mdc-ripple-surface";
       r##.unbounded := Js.bool true;
       Ripple.set_unbounded self);
      Option.iter (fun i ->
          i#add_class Markup.icon_class;
          i#add_class Markup.icon_on_class) on_icon;
      icon#add_class Markup.icon_class;
      match on_icon with
      | None -> ()
      | Some _ ->
         self#listen_lwt Widget.Event.click (fun _ _ ->
             self#toggle ();
             Lwt.return_unit) |> Lwt.ignore_result

end
