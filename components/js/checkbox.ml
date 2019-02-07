open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Checkbox.Make(Xml)(Svg)(Html)

class t ?(ripple = true) ?state ?on_change ?input_id () =

  let elt = Markup.create ?input_id () |> To_dom.of_div in
  let input_elt =
    elt##querySelector (Js.string ("." ^ Markup.native_control_class))
    |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in

  object(self)
    val mutable _ripple : Ripple.t option = None

    inherit Widget.radio_or_cb_widget ?state ?on_change ~input_elt elt () as super

    method set_indeterminate (x : bool) : unit =
      (Js.Unsafe.coerce input_elt)##.indeterminate := Js.bool x

    method indeterminate : bool =
      Js.to_bool (Js.Unsafe.coerce input_elt)##.indeterminate

    method! init () : unit =
      super#init ();
      if ripple then
        let adapter = Ripple.make_default_adapter (self :> Widget.t) in
        let is_unbounded = fun () -> true in
        let is_surface_active = fun () ->
          Ripple.Util.get_matches_property input_elt ":active" in
        let register_handler = fun typ f ->
          Dom_events.listen input_elt (Dom_events.Typ.make typ) (fun _ e ->
              f e; true) in
        let adapter =
          { adapter with is_unbounded
                       ; is_surface_active
                       ; register_handler } in
        let ripple = new Ripple.t adapter () in
        _ripple <- Some ripple;

    method! layout () : unit =
      super#layout ();
      Option.iter (fun r -> r#layout ()) _ripple

    method! destroy () : unit =
      super#destroy ();
      Option.iter (fun r -> r#destroy ()) _ripple;
      _ripple <- None

    method! set_disabled (x : bool) : unit =
      super#set_disabled x;
      super#toggle_class ~force:x Markup.disabled_class

  end
