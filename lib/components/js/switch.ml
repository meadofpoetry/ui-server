open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Switch
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

module Selector = struct
  let native_control = Printf.sprintf "input.%s" CSS.native_control
end

class t ?on_change (elt : #Dom_html.element Js.t) () =
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
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ changes input_elt (fun _ _ ->
                super#toggle_class ~force:self#checked CSS.checked;
                self#notify_change ()) ]
          @ listeners);
      input_elt##.checked := input_elt##.checked;
      super#initial_sync_with_dom ()

    method! layout () : unit =
      Option.iter Ripple.layout ripple_;
      super#layout ()

    method! destroy () : unit =
      (* Destroy internal components *)
      Option.iter Ripple.destroy ripple_;
      ripple_ <- None;
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

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
      super#toggle_class ~force:v CSS.checked;
      if notify then Lwt.async self#notify_change

    method input_element : Dom_html.inputElement Js.t = input_elt

    method ripple : Ripple.t option = ripple_

    (* Private methods *)
    method private notify_change () : unit Lwt.t =
      match on_change with
      | None -> Lwt.return_unit
      | Some f -> f (self :> t)

    method private create_ripple () : Ripple.t =
      let selector = "." ^ CSS.thumb_underlay in
      let (surface : Dom_html.element Js.t) =
        Js.Opt.get
          (elt##querySelector (Js.string selector))
          (fun () -> failwith (CSS.root ^ ": no ripple surface element found"))
      in
      Ripple.attach ~unbounded:true surface
  end

let attach ?on_change (elt : #Dom_html.element Js.t) : t = new t ?on_change elt ()

let make ?input_id ?classes ?a ?checked ?disabled ?track ?thumb_underlay ?on_change () =
  D.switch ?input_id ?classes ?a ?checked ?disabled ?track ?thumb_underlay ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?on_change
