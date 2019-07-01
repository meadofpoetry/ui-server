open Js_of_ocaml
open Components
open Page_mosaic_editor_tyxml.Container_editor

module Attr = struct
  let row = "data-row"
  let col = "data-col"
end

type direction = Col | Row

type cell_position =
  { col : int
  ; row : int
  ; col_span : int
  ; row_span : int
  }

let make_cell_position ?(col_span = 1) ?(row_span = 1) ~col ~row () =
  { col
  ; row
  ; col_span
  ; row_span
  }

type event = Touch of Dom_html.touchEvent Js.t
           | Mouse of Dom_html.mouseEvent Js.t

let coerce_event = function
  | Touch e -> (e :> Dom_html.event Js.t)
  | Mouse e -> (e :> Dom_html.event Js.t)

let cell_of_event
    (items : Dom_html.element Js.t list)
    (e : Dom_html.event Js.t) : Dom_html.element Js.t option =
  let target = Dom_html.eventTarget e in
  let selector = Printf.sprintf ".%s, .%s" CSS.cell CSS.root in
  let nearest_parent = Js.Opt.to_option @@ Element.closest target selector in
  match nearest_parent with
  | None -> None
  | Some parent ->
    if not @@ Element.matches parent ("." ^ CSS.cell)
    then None
    else List.find_opt (Element.equal parent) items

(* FIXME merge with same function in `Resizable` module *)
let get_cursor_position ?touch_id = function
  | Mouse event ->
    begin match Js.Optdef.(to_option event##.pageX,
                           to_option event##.pageY) with
    | Some page_x, Some page_y -> page_x, page_y
    | _ -> failwith "no page coordinates in mouse event"
    end
  | Touch event ->
    let (e : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
    let touches = e##.changedTouches in
    let rec aux acc i =
      if i >= touches##.length then acc else
        let touch = Js.Optdef.get (touches##item i) (fun () -> assert false) in
        match touch_id with
        | None -> Some touch
        | Some id ->
          if touch##.identifier = id then Some touch else
            aux acc (succ i) in
    (match aux None 0 with
     | None -> failwith "no touch event found"
     | Some t -> t##.pageX, t##.pageY)

let insert_at_idx i x l =
  let rec aux l acc i x = match l with
    | [] -> List.rev_append acc [x]
    | y :: l' when i = 0 -> List.rev_append acc (x :: y :: l')
    | y :: l' -> aux l' (y :: acc) (pred i) x
  in
  let i = if i < 0 then List.length l + i else i in
  aux l [] i x

let remove_at_idx i l0 =
  let rec aux l acc i = match l with
    | [] -> l0
    | _ :: l' when i = 0 -> List.rev_append acc l'
    | y :: l' -> aux l' (y :: acc) (pred i)
  in
  let i = if i < 0 then List.length l0 + i else i in
  aux l0 [] i

let first_non_zero f a =
  let rec aux = function
    | n when n < 0 -> None
    | n ->
      match f a.(n) with
      | None -> aux (pred n)
      | Some v ->
        if v <> 0. then Some (n, v)
        else aux (pred n) in
  aux (pred @@ Array.length a)

let fr_to_pixels track_values computed_values =
  match first_non_zero (function
      | Fr x -> Some x
      | _ -> None) track_values with
  | None -> 0.
  | Some (track, v) -> computed_values.(track) /. v

let percentage_to_pixels track_values computed_values =
  match first_non_zero (function
      | Pc x -> Some x
      | _ -> None) track_values with
  | None -> 0.
  | Some (track, v) -> computed_values.(track) /. v

class type stylish =
  object
    method style : Dom_html.cssStyleDeclaration Js.t Js.prop
  end

let get_matched_css_rules (elt : #Dom_html.element Js.t) : #stylish Js.t list =
  let stylesheets = (Js.Unsafe.coerce elt)##.ownerDocument##.styleSheets in
  let make_list coll =
    let rec aux acc = function
      | n when n < 0 -> acc
      | n ->
        let v = coll##item n in
        aux (v :: acc) (pred n) in
    aux [] coll##.length in
  List.filter (fun x ->
      try Element.matches elt (Js.to_string x##.selectorText)
      with _ -> false)
  @@ List.concat
  @@ List.map (fun x ->
      try make_list x##.cssRules
      with _ -> [])
  @@ make_list stylesheets

let get_styles (rule : string) (elt : Dom_html.element Js.t) =
  let rule = Js.string rule in
  let matched = get_matched_css_rules elt in
  let get_style x = Js.Unsafe.get x##.style rule in
  let styles = get_style elt :: (List.map get_style matched) in
  Utils.List.filter_map (fun (x : Js.js_string Js.t Js.optdef) ->
      Js.Optdef.case x
        (fun () -> None)
        (fun x -> match Js.to_string x with
           | "" -> None
           | s -> Some s))
    styles

let get_size_at_track ?(gap = 0.) (tracks : float array) =
  let gap = gap *. (float_of_int @@ pred @@ Array.length tracks) in
  let rec aux sum = function
    | n when n = Array.length tracks -> sum
    | n -> aux (sum +. tracks.(n)) (succ n) in
  (aux 0. 0) +. gap

let get_cell_position (cell : Dom_html.element Js.t) =
  let parse_span n s =
    let s = String.split_on_char ' ' s in
    match s with
    | ["auto"] -> 1
    | ["span"; v] -> int_of_string v
    | [s] -> int_of_string s - n
    | _ -> failwith "unknown cell span value" in
  let style = Dom_html.window##getComputedStyle cell in
  let col = Js.parseInt (Js.Unsafe.coerce style)##.gridColumnStart in
  let row = Js.parseInt (Js.Unsafe.coerce style)##.gridRowStart in
  let col_end = Js.to_string (Js.Unsafe.coerce style)##.gridColumnEnd in
  let row_end = Js.to_string (Js.Unsafe.coerce style)##.gridRowEnd in
  { col
  ; row
  ; col_span = parse_span col col_end
  ; row_span = parse_span row row_end
  }

let set_cell_row ?(span = 1) (row : int) (cell : Dom_html.element Js.t) =
  let v = Js.string @@ Printf.sprintf "%d / span %d" row span in
  (Js.Unsafe.coerce cell##.style)##.gridRow := v;
  cell##setAttribute (Js.string Attr.row) v

let set_cell_col ?(span = 1) (col : int) (cell : Dom_html.element Js.t) =
  let v = Js.string @@ Printf.sprintf "%d / span %d" col span in
  (Js.Unsafe.coerce cell##.style)##.gridColumn := v;
  cell##setAttribute (Js.string Attr.col) v

let set_cell_position { col; row; col_span; row_span }
    (cell : Dom_html.element Js.t) =
  set_cell_col ~span:col_span col cell;
  set_cell_row ~span:row_span row cell

let find_first_cell dir n cells =
  snd
  @@ Utils.Option.get
  @@ List.fold_left (fun acc cell ->
      let pos = get_cell_position cell in
      let main, aux = match dir with
        | Col -> pos.col, pos.row
        | Row -> pos.row, pos.col in
      match acc with
      | None -> if main = n then Some (aux, cell) else None
      | Some (aux', x) ->
        if n = main && aux < aux'
        then Some (aux, cell)
        else acc) None cells

let get_parent_grid (cell : Dom_html.element Js.t) =
  let rec aux elt =
    Js.Opt.case (Element.get_parent elt)
      (fun () -> failwith "parent grid not found")
      (fun elt ->
         if Element.has_class elt CSS.grid
         then elt else aux elt) in
  aux cell

let gen_cells ~f ~rows ~cols =
  let rec gen_rows acc row =
    let rec gen_cols acc col =
      if col = 0 then acc
      else
        let elt = f ~col ~row () in
        gen_cols (elt :: acc) (pred col) in
    if row = 0 then acc
    else gen_rows (gen_cols acc cols) (pred row) in
  gen_rows [] rows

let is_merge_possible (cells : Dom_html.element Js.t list) : bool =
  (* TODO implement *)
  (* XXX Merge is only possible when a group of cells forms a rectangle
     and all cells belong to the same parent grid. *)
  true

let get_cell' f = function
  | [] -> invalid_arg "list is empty"
  | x :: tl ->
    snd @@ List.fold_left (fun ((pos', _) as acc) x ->
        let pos = get_cell_position x in
        if f pos' pos then (pos, x) else acc)
      (get_cell_position x, x) tl

let get_topmost_cell cells =
  get_cell' (fun acc pos -> pos.row < acc.row) cells

let get_bottommost_cell cells =
  get_cell' (fun acc pos -> pos.row > acc.row) cells

let get_leftmost_cell cells =
  get_cell' (fun acc pos -> pos.col < acc.col) cells

let get_rightmost_cell cells =
  get_cell' (fun acc pos -> pos.col > acc.col) cells


