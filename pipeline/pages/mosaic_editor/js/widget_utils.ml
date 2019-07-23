open Js_of_ocaml
open Js_of_ocaml_tyxml
open Pipeline_types
open Page_mosaic_editor_tyxml
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

  let get_position (elt : Dom_html.element Js.t) : Wm.position option =
    try Some { x = get_float_attribute elt left
             ; y = get_float_attribute elt top
             ; w = get_float_attribute elt width
             ; h = get_float_attribute elt height
             }
    with _ -> None

  let string_of_float = Printf.sprintf "%g"

  let set_position (pos : Wm.position) (elt : Dom_html.element Js.t) =
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

module Z_index : sig
  type item =
    { item : Dom_html.element Js.t
    ; z_index : int
    ; selected : bool
    }

  val get : Dom_html.element Js.t -> int
  val set : int -> Dom_html.element Js.t -> unit
  val make_item_list :
    selected:Dom_html.element Js.t list
    -> Dom_html.element Js.t list
    -> item list
  val pack : item list -> unit
  val max_selected : Dom_html.element Js.t list -> int
  val min_selected : Dom_html.element Js.t list -> int
  val validate : Dom_html.element Js.t list -> unit
end = struct

  type item =
    { item : Dom_html.element Js.t
    ; z_index : int
    ; selected : bool
    }

  let get (elt : Dom_html.element Js.t) : int =
    try Js.parseInt @@ (Dom_html.window##getComputedStyle elt)##.zIndex
    with _ -> 0

  let set (z : int) (elt : Dom_html.element Js.t) : unit =
    elt##.style##.zIndex := Js.string (string_of_int z)

  let make_item_list ~(selected : Dom_html.element Js.t list) items =
    List.map (fun x ->
        { item = x
        ; z_index = get x
        ; selected = List.exists (Element.equal x) selected
        }) items

  let pack (zib_items : item list) =
    List.iteri (fun cnt x -> set cnt x.item) zib_items

  let max_selected = function
    | [] -> 0
    | x :: tl -> List.fold_left (fun acc x -> max acc (get x)) (get x) tl

  let min_selected = function
    | [] -> 0
    | x :: tl -> List.fold_left (fun acc x -> min acc (get x)) (get x) tl

  let rec separate
      ((acc_i : Dom_html.element Js.t list), (acc_ni : Dom_html.element Js.t list))
      (items : Dom_html.element Js.t list)
      (all_items : Dom_html.element Js.t list)
    : (Dom_html.element Js.t list * Dom_html.element Js.t list) =
    match items with
    | [] -> (acc_i, acc_ni)
    | hd :: tl ->
      let is_intersect =
        List.fold_left
          (fun acc (v : Dom_html.element Js.t) ->
             if not acc
             then (Position.Normalized.collides
                     (Position.Normalized.of_element hd)
                     (Position.Normalized.of_element v))
                  && not (Element.equal hd v)
             else true)
          false all_items in
      let (acc_i, acc_ni) =
        if is_intersect
        then (hd :: acc_i, acc_ni)
        else (acc_i, hd :: acc_ni)
      in
      separate (acc_i, acc_ni) tl all_items

  (* заменитель стандартного List.sort_uniq, т.к. с ним не работало:
     List.sort_uniq (fun x y -> if (Element.equal x y) then 0 else (-1) ) acc
     заменил на uniq acc
  *)
  let uniq l =
    let rec tail_uniq a l =
      match l with
      | [] -> a
      | hd :: tl ->
        tail_uniq (hd::a) (List.filter (fun x -> not (Element.equal x hd)) tl) in
    tail_uniq [] l

  let rec get_group_for_item
      (acc : Dom_html.element Js.t list)
      (search_item_rect : Position.Normalized.t)
      (items : Dom_html.element Js.t list) =
    match items with
    | [] ->
      let result = uniq acc in
      List.filter (fun v ->
          List.exists (Element.equal v) result)
        result
    | hd :: tl ->
      let siblings =
        List.fold_left
          (fun acc (v : Dom_html.element Js.t) ->
             if (Position.Normalized.collides
                   (search_item_rect)
                   (Position.Normalized.of_element v))
             then v :: acc else acc)
          [] items
      in
      let bounds = Position.Normalized.bounding_rect
          (List.map (fun v ->
               Position.Normalized.of_element v)
              siblings) in
      let (bounds, rect_growth) =
        if (bounds.h = search_item_rect.h && bounds.w = search_item_rect.w)
        || (bounds.h <= search_item_rect.h && bounds.w < search_item_rect.w)
        || (bounds.h < search_item_rect.h && bounds.w <= search_item_rect.w)
        then (search_item_rect, false)
        else (bounds, true) in
      let acc = siblings @ acc in
      if rect_growth
      then get_group_for_item acc bounds items
      else get_group_for_item acc bounds tl

  let rec get_all_groups
      (acc : (Dom_html.element Js.t list) list)
      (items : Dom_html.element Js.t list)
    : (Dom_html.element Js.t list) list =
    match items with
    | [] -> acc
    | hd :: tl ->
      let siblings = get_group_for_item
          [] (Position.Normalized.of_element hd) items in
      let items = List.filter (fun v ->
          not @@ List.exists (Element.equal v) siblings) tl in
      let acc = siblings :: acc in
      get_all_groups acc items

  let validate (items : Dom_html.element Js.t list) : unit =
    let (list_intersect, list_non_intersect) = separate ([], []) items items in
    let list_intersect_groups = get_all_groups [] list_intersect in
    List.iter (set 0) list_non_intersect;
    List.iter (List.iteri set) list_intersect_groups

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
  ; layer = Z_index.get elt
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
   | Some position -> Attr.set_position position elt);
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
