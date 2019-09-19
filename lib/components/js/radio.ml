open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Radio
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

module Selector = struct
  let native_control = Printf.sprintf "input.%s" CSS.native_control
end

class t ?on_change (elt : Dom_html.element Js.t) () =
  object (self)
    val input_elt : Dom_html.inputElement Js.t =
      let element = Element.query_selector_exn elt Selector.native_control in
      Js.Opt.get (Dom_html.CoerceTo.input element) (fun () -> assert false)

    val mutable ripple_ : Ripple.t option = None

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      ripple_ <- Some (self#create_ripple ());
      super#init ()

    method! initial_sync_with_dom () : unit =
      (match on_change with
      | None -> ()
      | Some _ ->
          listeners <-
            Js_of_ocaml_lwt.Lwt_js_events.(
              [ changes input_elt (fun _ _ ->
                    self#notify_change ();
                    Lwt.return_unit) ]
              @ listeners));
      super#initial_sync_with_dom ()

    method! layout () : unit =
      Option.iter Ripple.layout ripple_;
      super#layout ()

    method! destroy () : unit =
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      (* Destroy internal components *)
      Option.iter Ripple.destroy ripple_;
      ripple_ <- None;
      super#destroy ()

    method value : string = Js.to_string input_elt##.value

    method set_value (s : string) : unit = input_elt##.value := Js.string s

    method disabled : bool = Js.to_bool input_elt##.disabled

    method set_disabled (x : bool) : unit =
      input_elt##.disabled := Js.bool x;
      super#toggle_class ~force:x CSS.disabled

    method checked : bool = Js.to_bool input_elt##.checked

    method toggle ?(notify = false) ?(force : bool option) () : unit =
      let v =
        match force with
        | None -> not self#checked
        | Some x -> x
      in
      input_elt##.checked := Js.bool v;
      if notify then self#notify_change ()

    method input_element : Dom_html.inputElement Js.t = input_elt

    method ripple : Ripple.t option = ripple_

    (* Private methods *)
    method private notify_change () : unit =
      Option.iter (fun f -> f self#checked) on_change

    method private create_ripple () : Ripple.t =
      let adapter = Ripple.make_default_adapter super#root in
      let is_unbounded () = true in
      let is_surface_active () = false in
      let adapter =
        { adapter with
          event_target = Element.coerce input_elt
        ; is_unbounded
        ; is_surface_active }
      in
      new Ripple.t adapter ()
  end

let attach ?on_change (elt : #Dom_html.element Js.t) : t =
  new t ?on_change (Element.coerce elt) ()

let make
    ?classes
    ?a
    ?input_id
    ?checked
    ?disabled
    ?name
    ?outer_circle
    ?inner_circle
    ?background
    ?native_control
    ?children
    ?on_change
    () =
  D.radio
    ?classes
    ?a
    ?input_id
    ?checked
    ?disabled
    ?name
    ?outer_circle
    ?inner_circle
    ?background
    ?native_control
    ?children
    ()
  |> Tyxml_js.To_dom.of_element
  |> attach ?on_change
