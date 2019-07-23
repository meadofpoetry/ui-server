open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Form_field
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

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

class ['a] t ~(input : #input_widget as 'a) (elt : Dom_html.element Js.t) () =
object(self)
  val label_elt : Dom_html.element Js.t option =
    Element.query_selector elt Selector.label
  val mutable _click_listener = None

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    match label_elt with
    | None -> ()
    | Some label ->
       let listener =
         Events.clicks label (fun _ _ -> self#handle_click ()) in
       _click_listener <- Some listener

  method! destroy () : unit =
    super#destroy ();
    match _click_listener with
    | None -> ()
    | Some x -> Lwt.cancel x; _click_listener <- None

  method input : 'a = input

  method label : string =
    match label_elt with
    | None -> ""
    | Some label ->
       Js.Opt.get
         (Js.Opt.map label##.textContent Js.to_string)
         (fun () -> "")

  method set_label (s : string) =
    match label_elt with
    | None -> ()
    | Some label -> label##.textContent := Js.some @@ Js.string s

  (* Private methods *)

  method private handle_click () : unit Lwt.t =
    self#activate_ripple ()
    >>= Utils.Animation.request
    >>= fun _ -> self#deactivate_ripple ()

  method private activate_ripple () : unit Lwt.t =
    match input#ripple with
    | None -> Lwt.return ()
    | Some (r : Ripple.t) -> r#activate ()

  method private deactivate_ripple () : unit Lwt.t =
    match input#ripple with
    | None -> Lwt.return ()
    | Some (r : Ripple.t) -> r#deactivate ()
end

let make ?align_end ~label (input : #input_widget as 'a) : 'a t =
  let id = Js.to_string @@ input#input_element##.id in
  let for_id = match id with
    | "" ->
       let id = get_id () in
       input#input_element##.id := Js.string id;
       id
    | id -> id in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?align_end
         ~label:(Markup.create_label ~for_id label ())
         ~input:(Widget.to_markup input)
         () in
  new t ~input elt ()

let attach (f : #Dom_html.element Js.t -> 'a)
      (elt : #Dom_html.element Js.t) : 'a t =
  let input = match Element.query_selector elt Selector.input with
    | None -> failwith "form_field: no input element found"
    | Some x -> f x in
  new t ~input (Element.coerce elt) ()
