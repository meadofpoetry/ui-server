open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Form_field
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

class type input_widget =
  object
    inherit Widget.t

    method input_element : Dom_html.inputElement Js.t

    method ripple : Ripple.t option
  end

let id_ref = ref (int_of_float @@ Unix.time ())

let get_id () =
  incr id_ref;
  Printf.sprintf "form-input-%d" !id_ref

module Selector = struct
  let label = Printf.sprintf ".%s > label" CSS.root

  let input = Printf.sprintf ".%s > :not(label)" CSS.root
end

class ['a] t
  ~(input : [`Attach of Dom_html.element Js.t -> (#input_widget as 'a) | `Widget of 'a])
  (elt : Dom_html.element Js.t)
  () =
  object (self)
    val label : Dom_html.element Js.t option = Element.query_selector elt Selector.label

    val input : #input_widget as 'a =
      match input with
      | `Attach f -> f (Element.query_selector_exn elt Selector.input)
      | `Widget x -> x

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      (match label with
      | None -> ()
      | Some label ->
          listeners <-
            Js_of_ocaml_lwt.Lwt_js_events.([clicks label self#handle_click] @ listeners));
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

    method input : 'a = input

    method label : string =
      match label with
      | None -> ""
      | Some label ->
          Js.Opt.get (Js.Opt.map label##.textContent Js.to_string) (fun () -> "")

    method set_label (s : string) =
      match label with
      | None -> ()
      | Some label -> label##.textContent := Js.some @@ Js.string s

    (* Private methods *)
    method private handle_click _ _ : unit Lwt.t =
      self#activate_ripple ()
      >>= Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame
      >>= self#deactivate_ripple

    method private activate_ripple () : unit Lwt.t =
      match input#ripple with
      | None -> Lwt.return ()
      | Some (r : Ripple.t) -> r#activate ()

    method private deactivate_ripple () : unit Lwt.t =
      match input#ripple with
      | None -> Lwt.return ()
      | Some (r : Ripple.t) -> r#deactivate ()
  end

let attach f (elt : #Dom_html.element Js.t) : 'a t =
  new t ~input:(`Attach f) (Element.coerce elt) ()

let make ?classes ?a ?align_end ?label_for ~attach_input ?label ~input () =
  D.form_field ?classes ?a ?align_end ?label_for ~input ?label ()
  |> Tyxml_js.To_dom.of_div
  |> fun elt -> new t ~input:(`Attach attach_input) elt ()

let make_of_widget ?classes ?a ?align_end ~label ~(input : #input_widget) () =
  let id = Js.to_string @@ input#input_element##.id in
  let label_for =
    match id with
    | "" ->
        let id = get_id () in
        input#input_element##.id := Js.string id;
        id
    | id -> id
  in
  D.form_field ?classes ?a ?align_end ~label_for ~input:input#markup ~label ()
  |> Tyxml_js.To_dom.of_div
  |> fun elt -> new t ~input:(`Widget input) elt ()
