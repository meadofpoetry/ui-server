open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Radio.Make(Xml)(Svg)(Html)

class ['a] t ?(ripple = true) ?input_id ~name ~(value : 'a) () =
  let elt = Markup.create ?input_id ~name () |> Tyxml_js.To_dom.of_i in
  let input_elt =
    elt##querySelector (Js.string ("." ^ Markup.native_control_class))
    |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in
  object(self)
    val mutable _ripple : Ripple.t option = None

    inherit Widget.radio_or_cb_widget ~input_elt elt () as super

    val mutable value : 'a = value

    method set_value (x : 'a) : unit =
      value <- x

    method value : 'a =
      value

    method! init () : unit =
      super#init ();
      if ripple then
        let adapter = Ripple.make_default_adapter (self :> Widget.t) in
        let is_unbounded = fun () -> true in
        let is_surface_active = fun () -> false in
        let register_handler = fun typ f ->
          Dom_events.listen input_elt (Dom_events.Typ.make typ) (fun _ e ->
              f e; true) in
        let adapter =
          { adapter with is_unbounded
                       ; is_surface_active
                       ; register_handler } in
        let ripple = new Ripple.t adapter () in
        _ripple <- Some ripple

    method! layout () : unit =
      super#layout ();
      Option.iter (fun r -> r#layout ()) _ripple

    method! destroy () : unit =
      super#destroy ();
      Option.iter (fun r -> r#destroy ()) _ripple;
      _ripple <- None

  end
