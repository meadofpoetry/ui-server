open Js_of_ocaml
open Js_of_ocaml_tyxml
open Pipeline_types
open Page_mosaic_editor_tyxml.Widget
open Components

module Attr = struct

  let id = "data-id"

  let typ = "data-type"

  let pid = "data-pid"

  let domain = "data-domain"

  let aspect = "data-aspect"

  let description = "data-description"

  let width = "data-width"

  let height = "data-height"

  let left = "data-left"

  let top = "data-top"

  let attributes =
    [ id
    ; typ
    ; pid
    ; domain
    ; aspect
    ; description
    ; width
    ; height
    ; left
    ; top
    ]

  let invalid_value a v =
    failwith @@ Printf.sprintf "invalid `%s` attribute value (%s)" typ v

  let get_float_attribute (elt : #Dom_html.element Js.t) attr : float =
    match Element.get_attribute elt attr with
    | None -> 0.
    | Some x ->
      match float_of_string_opt x with
      | None -> 0.
      | Some x -> x

  let get_id (elt : Dom_html.element Js.t) =
    match Element.get_attribute elt id with
    | None -> ""
    | Some s -> s

  let set_id (elt : Dom_html.element Js.t) (id' : string) =
    Element.set_attribute elt id id'

  let get_position (elt : Dom_html.element Js.t) : Position.t option =
    try Some { x = get_float_attribute elt left
             ; y = get_float_attribute elt top
             ; w = get_float_attribute elt width
             ; h = get_float_attribute elt height
             }
    with _ -> None

  let string_of_float = Printf.sprintf "%g"

  let set_position (elt : Dom_html.element Js.t) (pos : Position.t) =
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

module Z_index = struct

  type item =
    { item : Dom_html.element Js.t
    ; z_index : int
    ; selected : bool
    }

  let of_element (elt : Dom_html.element Js.t) : int =
    let zi = (Dom_html.window##getComputedStyle elt)##.zIndex in
    try Js.parseInt zi with _ -> 0

  let set (elt : Dom_html.element Js.t) (z : int) : unit =
    elt##.style##.zIndex := Js.string (string_of_int z)

  let rec create_all_z_list
      ~(selected : Dom_html.element Js.t list)
      items =
    let selected = List.map of_element selected in
    List.map (fun (x : Dom_html.element Js.t) ->
        let z_index = of_element x in
        { item = x
        ; z_index = of_element x
        ; selected = List.mem z_index selected
        }) items

  let rec pack (zib_items : item list) =
    List.mapi (fun cnt x -> { x with z_index = cnt + 1 }) zib_items

  let rec get_upper_selected_z
      (counter : int) (* initial 1 *)
      (selected_list_len : int)
      (zib_items : item list) : int =
    match zib_items with
    | [] -> -1
    | x :: tl ->
      if x.selected && counter = selected_list_len
      then x.z_index
      else get_upper_selected_z
          (if x.selected then (counter + 1) else counter)
          selected_list_len tl

  let rec get_first_selected_z
      (zib_items : item list) : int =
    match zib_items with
    | [] -> (-1)
    | x :: tl ->
      if x.selected then x.z_index
      else get_first_selected_z tl

  (* separate selected and not selected items,
     assign z numbers continuosly*)
  let rec separate_selected
      (is_selected : bool)
      (z_begin : int)
      (z_end : int)
      (zib_items : item list) =
    List.filter (fun x ->
        x.selected = is_selected
        && z_begin <= x.z_index
        && x.z_index <= z_end)
      zib_items
end

let title (w : Wm.widget) : string =
  let typ = match w.type_ with
    | Wm.Video -> "Видео"
    | Audio -> "Аудио" in
  match w.pid with
  | None -> typ
  | Some pid -> Printf.sprintf "%s. PID %d" typ pid

let widget_of_element (elt : Dom_html.element Js.t) : string * Wm.widget =
  Attr.get_id elt,
  { type_ = Attr.get_typ elt
  ; domain = Attr.get_domain elt
  ; pid = Attr.get_pid elt
  ; position = Attr.get_position elt
  ; layer = Z_index.of_element elt
  ; aspect = Attr.get_aspect elt
  ; description = Attr.get_description elt
  }

let copy_attributes
    (from : Dom_html.element Js.t)
    (to_ : Dom_html.element Js.t) =
  let copy attr =
    let attr = Js.string attr in
    Js.Opt.iter (from##getAttribute attr)
      (fun x -> to_##setAttribute attr x) in
  List.iter copy Attr.attributes

let set_attributes ?id
    (elt : Dom_html.element Js.t)
    (widget : Wm.widget) : unit =
  Attr.set_typ elt widget.type_;
  Attr.set_domain elt widget.domain;
  Attr.set_pid elt widget.pid;
  Attr.set_aspect elt widget.aspect;
  Attr.set_description elt widget.description;
  (match widget.position with
   | None -> ()
   | Some position -> Attr.set_position elt position);
  elt##.style##.zIndex := Js.string (string_of_int widget.layer);
  match id with
  | None -> ()
  | Some id -> Attr.set_id elt id

let elements (elt : Dom_html.element Js.t) =
  let selector =
    Printf.sprintf ".%s"
    @@ Page_mosaic_editor_tyxml.Container_editor.CSS.widget in
  Dom.list_of_nodeList
  @@ elt##querySelectorAll (Js.string selector)

let widgets_of_container (cell : Dom_html.element Js.t) : (string * Wm.widget) list =
  List.map widget_of_element @@ elements cell

let get_relative_position (x : Dom_html.element Js.t) : Position.t =
  { x = Js.parseFloat x##.style##.left
  ; y = Js.parseFloat x##.style##.top
  ; w = Js.parseFloat x##.style##.width
  ; h = Js.parseFloat x##.style##.height
  }
