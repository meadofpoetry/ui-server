open Js_of_ocaml
open Js_of_ocaml_tyxml
open Pipeline_types

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
      (fun s -> match Js.to_string s with
         | "audio" -> Wm.Audio
         | "video" -> Video
         | s -> invalid_value typ s)

  let get_domain (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string domain))
      (fun () -> Wm.Nihil)
      (fun s ->
         let json = Yojson.Safe.from_string (Js.to_string s) in
         match Wm.domain_of_yojson json with
         | Ok x -> x
         | Error e -> failwith e)

  let get_pid (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string pid))
      (fun () -> None)
      (fun x -> Some (Js.parseInt x))

  let get_aspect (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string aspect))
      (fun () -> None)
      (fun s ->
         let s = Js.to_string s in
         match String.split_on_char 'x' s with
         | [w; h] -> Some (int_of_string w, int_of_string h)
         | _ -> invalid_value aspect s)

  let get_description (elt : Dom_html.element Js.t) =
    Js.Opt.case (elt##getAttribute (Js.string description))
      (fun () -> "")
      Js.to_string
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

let update_element
    (elt : Dom_html.element Js.t)
    (widget : string * Wm.widget) : unit =
  ()

let elements (elt : Dom_html.element Js.t) =
  let selector =
    Printf.sprintf ".%s"
    @@ Page_mosaic_editor_tyxml.Container_editor.CSS.widget in
  Dom.list_of_nodeList
  @@ elt##querySelectorAll (Js.string selector)

let of_container (cell : Dom_html.element Js.t) : (string * Wm.widget) list =
  List.map of_element @@ elements cell
