open Js_of_ocaml
open Js_of_ocaml_tyxml
open Pipeline_types
open Page_mosaic_editor_tyxml.Widget
open Components

module Attr = struct

  let typ = "data-type"

  let pid = "data-pid"

  let domain = "data-domain"

  let aspect = "data-aspect"

  let description = "data-description"

  let width = "data-width"

  let height = "data-height"

  let left = "data-left"

  let top = "data-top"

  let invalid_value a v =
    failwith @@ Printf.sprintf "invalid `%s` attribute value (%s)" typ v

  let get_float_attribute (elt : #Dom_html.element Js.t) attr : float =
    match Element.get_attribute elt attr with
    | None -> 0.
    | Some x ->
      match float_of_string_opt x with
      | None -> 0.
      | Some x -> x

  let get_relative_position (elt : Dom_html.element Js.t) =
    { Position.
      x = get_float_attribute elt left
    ; y = get_float_attribute elt top
    ; w = get_float_attribute elt width
    ; h = get_float_attribute elt height
    }

  let get_position ~parent_size (elt : Dom_html.element Js.t) =
    let pos = get_relative_position elt in
    Position.(to_wm_position @@ of_relative ~parent_size pos)

  let set_position
      ~parent_size
      (elt : Dom_html.element Js.t)
      (pos : Wm.position) =
    let string_of_float = Printf.sprintf "%g" in
    let pos = Position.(to_relative ~parent_size @@ of_wm_position pos) in
    Element.(
      set_attribute elt left (string_of_float pos.x);
      set_attribute elt top (string_of_float pos.y);
      set_attribute elt width (string_of_float pos.w);
      set_attribute elt height (string_of_float pos.h))

  let get_typ (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string typ))
      (fun () -> failwith @@ Printf.sprintf "no `%s` attribute found" typ)
      (fun s ->
         let s = Js.to_string s in
         match widget_type_of_string s with
         | Some x -> x
         | None -> invalid_value typ s)

  let set_typ (elt : Dom_html.element Js.t) (t : Wm.widget_type) =
    let t = widget_type_to_string t in
    Element.set_attribute elt typ t

  let get_domain (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string domain))
      (fun () -> Wm.Nihil)
      (fun s ->
         let json = Yojson.Safe.from_string (Js.to_string s) in
         match Wm.domain_of_yojson json with
         | Ok x -> x
         | Error e -> failwith e)

  let set_domain (elt : Dom_html.element Js.t) = function
    | Wm.Nihil -> ()
    | d ->
      let v = domain_attr_value d in
      Element.set_attribute elt domain v

  let get_pid (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string pid))
      (fun () -> None)
      (fun x -> Some (Js.parseInt x))

  let set_pid (elt : Dom_html.element Js.t) = function
    | None -> ()
    | Some x -> Element.set_attribute elt pid (string_of_int x)

  let get_aspect (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string aspect))
      (fun () -> None)
      (fun s ->
         let s = Js.to_string s in
         match String.split_on_char 'x' s with
         | [w; h] -> Some (int_of_string w, int_of_string h)
         | _ -> invalid_value aspect s)

  let set_aspect (elt : Dom_html.element Js.t) = function
    | None -> ()
    | Some x -> Element.set_attribute elt aspect (aspect_attr_value x)

  let get_description (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string description))
      (fun () -> "")
      Js.to_string

  let set_description (elt : Dom_html.element Js.t) v =
    Element.set_attribute elt description v

end

let layer_of_element (elt : Dom_html.element Js.t) : int =
  let zi = (Dom_html.window##getComputedStyle elt)##.zIndex in
  try Js.parseInt zi with _ -> 0 (* TODO implement normally *)

let widget_of_element ?parent_size
    (elt : Dom_html.element Js.t) : string * Wm.widget =
  let id = Js.to_string elt##.id in
  id,
  let position = match parent_size with
    | None -> None
    | Some x ->
      try Some (Attr.get_position ~parent_size:x elt)
      with _ -> None in
  { type_ = Attr.get_typ elt
  ; domain = Attr.get_domain elt
  ; pid = Attr.get_pid elt
  ; position
  ; layer = layer_of_element elt
  ; aspect = Attr.get_aspect elt
  ; description = Attr.get_description elt
  }

let set_attributes ?id
    ?parent_size
    (elt : Dom_html.element Js.t)
    (widget : Wm.widget) : unit =
  Attr.set_typ elt widget.type_;
  Attr.set_domain elt widget.domain;
  Attr.set_pid elt widget.pid;
  Attr.set_aspect elt widget.aspect;
  Attr.set_description elt widget.description;
  (match parent_size, widget.position with
   | None, _ | _, None -> ()
   | Some parent_size, Some position -> Attr.set_position ~parent_size elt position);
  elt##.style##.zIndex := Js.string (string_of_int widget.layer);
  match id with
  | None -> ()
  | Some id -> elt##.id := Js.string id

let elements (elt : Dom_html.element Js.t) =
  let selector =
    Printf.sprintf ".%s"
    @@ Page_mosaic_editor_tyxml.Container_editor.CSS.widget in
  Dom.list_of_nodeList
  @@ elt##querySelectorAll (Js.string selector)

let widgets_of_container ~parent_size
    (cell : Dom_html.element Js.t) : (string * Wm.widget) list =
  List.map (widget_of_element ~parent_size) @@ elements cell
