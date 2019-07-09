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

  let invalid_value a v =
    failwith @@ Printf.sprintf "invalid `%s` attribute value (%s)" typ v

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

let position_of_element (elt : Dom_html.element Js.t) : Wm.position =
  { left = 0
  ; right = 100
  ; top = 0
  ; bottom = 100
  }

let layer_of_element (elt : Dom_html.element Js.t) : int =
  let zi = (Dom_html.window##getComputedStyle elt)##.zIndex in
  try Js.parseInt zi with _ -> 0 (* TODO implement normally *)

let of_element (elt : Dom_html.element Js.t) : string * Wm.widget =
  let id = Js.to_string elt##.id in
  id,
  { type_ = Attr.get_typ elt
  ; domain = Attr.get_domain elt
  ; pid = Attr.get_pid elt
  ; position = Some (position_of_element elt)
  ; layer = layer_of_element elt
  ; aspect = Attr.get_aspect elt
  ; description = Attr.get_description elt
  }

let apply_to_element ?id (elt : Dom_html.element Js.t) (widget : Wm.widget) : unit =
  Attr.set_typ elt widget.type_;
  Attr.set_domain elt widget.domain;
  Attr.set_pid elt widget.pid;
  Attr.set_aspect elt widget.aspect;
  Attr.set_description elt widget.description;
  match id with
  | None -> ()
  | Some id -> elt##.id := Js.string id

let elements (elt : Dom_html.element Js.t) =
  let selector =
    Printf.sprintf ".%s"
    @@ Page_mosaic_editor_tyxml.Container_editor.CSS.widget in
  Dom.list_of_nodeList
  @@ elt##querySelectorAll (Js.string selector)

let of_container (cell : Dom_html.element Js.t) : (string * Wm.widget) list =
  List.map of_element @@ elements cell
