open Js_of_ocaml
open Components

module CSS = Markup.CSS.Container_grid

module Attr = struct
  let row = "data-row"
  let col = "data-col"
end

type event = Touch of Dom_html.touchEvent Js.t
           | Mouse of Dom_html.mouseEvent Js.t

let coerce_event = function
  | Touch e -> (e :> Dom_html.event Js.t)
  | Mouse e -> (e :> Dom_html.event Js.t)

let cell_of_event
    (items : Dom_html.element Dom.nodeList Js.t)
    (e : Dom_html.event Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option
  @@ Js.Opt.bind e##.target (fun (target : Dom_html.element Js.t) ->
      let selector = Printf.sprintf ".%s, .%s" CSS.cell CSS.root in
      let nearest_parent = Element.closest target selector in
      Js.Opt.bind nearest_parent (fun (parent : Dom_html.element Js.t) ->
          if not @@ Element.matches parent ("." ^ CSS.cell)
          then Js.null
          else Element.find (Element.equal parent) items))

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

let split_string ~suffix pattern =
  let pattern_len = String.length pattern in
  let len = String.length suffix in
  if len > pattern_len
  then None
  else
    let sub = String.sub pattern (pattern_len - len) len in
    if String.uppercase_ascii sub = String.uppercase_ascii suffix
    then Some (String.sub pattern 0 (pattern_len - len))
    else None

type value =
  [ `Auto
  | `Px of float
  | `Fr of float
  | `Pc of float
  ]

let pp_value ppf = function
  | `Auto -> Format.pp_print_string ppf "auto"
  | `Px x ->
    Format.pp_print_float ppf x;
    Format.pp_print_string ppf "px"
  | `Fr x ->
    Format.pp_print_float ppf x;
    Format.pp_print_string ppf "fr"
  | `Pc x ->
    Format.pp_print_float ppf x;
    Format.pp_print_string ppf "%"
  | _ -> Format.pp_print_string ppf ""

let value_to_string = function
  | `Auto -> "auto"
  | `Px x -> Printf.sprintf "%gpx" x
  | `Fr x -> Printf.sprintf "%gfr" x
  | `Pc x -> Printf.sprintf "%g%%" x

let value_of_string : string -> value = function
  | "auto" -> `Auto
  | s ->
    let rec aux = function
      | [] -> failwith @@ Printf.sprintf "parse: unknown unit (%s)" s
      | suffix :: tl ->
        match split_string ~suffix s with
        | None -> aux tl
        | Some x ->
          match float_of_string_opt x with
          | None -> failwith @@ Printf.sprintf "parse: bad value (%s)" x
          | Some x ->
            match suffix with
            | "px" -> `Px x
            | "fr" -> `Fr x
            | "%" -> `Pc x
            | s -> failwith @@ Printf.sprintf "parse: unknown unit (%s)" s
    in
    aux ["px"; "fr"; "%"]

let value_of_string_opt (s : string) : value option =
  try Some (value_of_string s) with _ -> None

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
      | `Fr x -> Some x
      | _ -> None) track_values with
  | None -> 0.
  | Some (track, v) -> computed_values.(track) /. v

let percentage_to_pixels track_values computed_values =
  match first_non_zero (function
      | `Pc x -> Some x
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
  let style = Dom_html.window##getComputedStyle cell in
  Js.parseInt (Js.Unsafe.coerce style)##.gridColumnStart,
  Js.parseInt (Js.Unsafe.coerce style)##.gridRowStart

let set_cell_row (cell : Dom_html.element Js.t) (row : int) =
  let v = Js.string @@ string_of_int row in
  (Js.Unsafe.coerce cell##.style)##.gridRow := v;
  cell##setAttribute (Js.string Attr.row) v

let set_cell_col (cell : Dom_html.element Js.t) (col : int) =
  let v = Js.string @@ string_of_int col in
  (Js.Unsafe.coerce cell##.style)##.gridColumn := v;
  cell##setAttribute (Js.string Attr.col) v

let set_cell_position ~col ~row (cell : Dom_html.element Js.t) =
  set_cell_col cell col;
  set_cell_row cell row
