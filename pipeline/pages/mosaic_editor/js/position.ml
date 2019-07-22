open Js_of_ocaml
open Components

include Page_mosaic_editor_tyxml.Position

let ( % ) f g x = f (g x)

type aspect = int * int

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : float
  }

type line_align_direction =
  | Htop
  | Hcenter
  | Hbottom
  | Vleft
  | Vcenter
  | Vright
  | Nill

let fabs (v1 : float) = if v1 >= 0.0 then v1 else (-. v1)

let show_line (x : line) : string =
  Printf.sprintf "is vertical: %B, is multiple: %B, is center: %B, origin: %g"
    x.is_vertical x.is_multiple x.is_center x.origin

(** Checks if two elements collide, returns [true] if so and [false] otherwise *)
let collides (pos1 : t) (pos2 : t) =
  if (pos1.x +. pos1.w <= pos2.x) then false
  else if (pos1.x >= pos2.x +. pos2.w) then false
  else if (pos1.y +. pos1.h <= pos2.y) then false
  else if (pos1.y >= pos2.y +. pos2.h) then false
  else true

(** Changes width and height to correspond provided aspect **)
let fix_aspect (p : t) (aspect : int * int) =
  let w =
    let w = int_of_float p.w in
    if w mod (fst aspect) <> 0 then
      let w = w / (fst aspect) * (fst aspect) in
      if w = 0 then (fst aspect) else w
    else w
  in
  let h =
    let h = int_of_float p.h in
    if h mod (snd aspect) <> 0 then
      let h = (h / (snd aspect)) * (snd aspect) in
      if h = 0 then (snd aspect) else h
    else h
  in
  let sw = w / (fst aspect) in
  let sh = h / (snd aspect) in
  let w, h =
    if sw > sh
    then (fst aspect) * sh, h
    else w, (snd aspect) * sw
  in
  { p with w = float_of_int w; h = float_of_int h }

let fix_aspect2
    (dir : direction)
    (p : t)
    (orig_pos : t)
    (aspect : int * int) =
  let asp =
    if fst aspect = 0
    then 1.0
    else (float_of_int (snd aspect)) /. (float_of_int (fst aspect)) in
  let asp = if asp <= 0.0 then 1.0 else asp in
  match dir with
  | NW -> { p with y = orig_pos.y ; h = p.w *. asp  }
  | NE -> { p with y = orig_pos.y ; h = p.w *. asp }
  | SW -> { p with h = p.w *. asp }
  | SE -> { p with h = p.w *. asp }
  (* not tested *)
  | N -> p
  | S -> p
  | W -> p
  | E -> p

(** Changes top and left coordinates to correspond parent dimentions *)
let fix_xy ?min_x ?min_y ?max_x ?max_y ?(parent_w = 1.) ?(parent_h = 1.) (p : t) =
  let x =
    if p.x < 0. then 0.
    else if p.x +. p.w > parent_w then parent_w -. p.w
    else p.x in
  let y =
    if p.y < 0. then 0.
    else if p.y +. p.h > parent_h then parent_h -. p.h
    else p.y
  in
  let x = match max_x with
    | Some max -> if x > max then max else x
    | None -> x
  in
  let x = match min_x with
    | Some min -> if x < min then min else x
    | None -> x
  in
  let y = match max_y with
    | Some max -> if y > max then max else y
    | None -> y
  in
  let y = match min_y with
    | Some min -> if y < min then min else y
    | None -> y
  in
  { p with x; y }

(** Changes width to correspond provided constraints *)
let fix_w ?max_w ?(min_w = 0.) ?(parent_w = 1.) (p : t) =
  let w = match max_w with
    | Some max ->
      if p.w > max then max
      else if p.w < min_w then min_w
      else p.w
    | None -> if p.w < min_w then min_w else p.w
  in
  let w =
    if p.x +. w > parent_w
    then parent_w -. p.x
    else if p.x < 0.0
    then w +. p.x
    else w 
  in
  let w = if w < min_w
    then min_w
    else w
  in    
  { p with w }

(** Changes height to correspond provided constraints *)
let fix_h ?max_h ?(min_h = 0.) ?(parent_h = 1.) (p : t) =
  let h = match max_h with
    | Some max ->
      if p.h > max then max
      else if p.h < min_h then min_h
      else p.h
    | None -> if p.h < min_h then min_h else p.h
  in
  let h = if p.y +. h > parent_h
    then parent_h -. p.y
    else if p.y < 0.0
    then h +. p.y
    else h
  in
  let h = if h < min_h
    then min_h
    else h
  in
  { p with h }

(** Changes width and height to correspond provided constraints *)
let fix_wh ?max_w ?min_w ?max_h ?min_h ?parent_w ?parent_h =
  (fix_h ?max_h ?min_h ?parent_h) % (fix_w ?max_w ?min_w ?parent_w)

let fix ?min_x ?min_y ?max_x ?max_y
    ?max_w ?min_w ?max_h ?min_h ?parent_w ?parent_h =
  fix_xy ?min_x ?min_y ?max_x ?max_y ?parent_w ?parent_h
  % fix_wh ?max_w ?min_w ?max_h ?min_h ?parent_w ?parent_h

let apply_to_element ~unit (pos : t) (elt : #Dom_html.element Js.t) =
  let fn = match unit with
    | `Px -> Printf.sprintf "%gpx"
    | `Pct | `Norm -> Printf.sprintf "%g%%" in
  let pos = match unit with
    | `Px | `Pct -> pos
    | `Norm ->
      { x = pos.x *. 100.
      ; y = pos.y *. 100.
      ; w = pos.w *. 100.
      ; h = pos.h *. 100.
      } in
  elt##.style##.width := Js.string @@ fn pos.w;
  elt##.style##.left := Js.string @@ fn pos.x;
  elt##.style##.height := Js.string @@ fn pos.h;
  elt##.style##.top := Js.string @@ fn pos.y

let of_element (elt : #Dom_html.element Js.t) : t =
  { x = float_of_int elt##.offsetLeft
  ; y = float_of_int elt##.offsetTop
  ; w = float_of_int elt##.offsetWidth
  ; h = float_of_int elt##.offsetHeight
  }

let of_client_rect (r : Dom_html.clientRect Js.t) : t =
  { x = r##.left
  ; y = r##.top
  ; w = Js.Optdef.get r##.width (fun () -> r##.right -. r##.left)
  ; h = Js.Optdef.get r##.height (fun () -> r##.bottom -. r##.top)
  }

let bounding_rect : t list -> t = function
  | [] -> empty
  | [x] -> x
  | hd :: tl ->
    let acc = hd.x, hd.y, hd.x +. hd.w, hd.y +. hd.h in
    let (x, y, r, b) =
      List.fold_left (fun (x, y, r, b) (pos : t) ->
          let pos_right = pos.w +. pos.x in
          let pos_bottom = pos.h +. pos.y in
          min x pos.x,
          min y pos.y,
          max r pos_right,
          max b pos_bottom)
        acc tl in
    { x; y; w = r -. x; h = b -. y }

(* min_distance - pixels
   return: (other element align as line_align_direction *
            minimum distance of several lines of one align as int)
*)
let line_find_closest_align
    (pos : t)
    (siblings : t list)
    (min_distance : float)
    (line_align : line_align_direction) =
  let rec count_aligns line_align_val (distance: (line_align_direction * float)) = function
    | [] -> distance
    | (hd : t) :: tl ->
      let (distance : line_align_direction * float) = match line_align with
        | Htop ->
          let dist1 = pos.y -. hd.y in
          let dist2 = pos.y -. hd.y -. hd.h /. 2.0 in
          let dist3 = pos.y -. hd.y -. hd.h in
          if (fabs dist1 < min_distance)
          && (fabs (snd distance) > fabs dist1)
          && (fabs dist1 <= fabs dist2)
          && (fabs dist1 <= fabs dist3)
          then Htop, dist1
          else if (fabs dist2 < min_distance)
               && (fabs (snd distance) > fabs dist2)
               && (fabs dist2 <= fabs dist1)
               && (fabs dist2 <= fabs dist3)
          then Hcenter, dist2
          else if (fabs dist3 < min_distance)
               && (fabs (snd distance) > fabs dist3)
               && (fabs dist3 <= fabs dist1)
               && (fabs dist3 <= fabs dist2)
          then Hbottom, dist3
          else distance
        | Hcenter ->
          let dist1 = pos.y +. pos.h /. 2.0 -. hd.y in
          let dist2 = pos.y +. pos.h /. 2.0 -. hd.y -. hd.h /. 2.0 in
          let dist3 = pos.y +. pos.h /. 2.0 -. hd.y -. hd.h in
          if (fabs dist1 < min_distance)
          && (fabs (snd distance) > fabs dist1)
          && (fabs dist1 <= fabs dist2)
          && (fabs dist1 <= fabs dist3)
          then Htop, dist1
          else if (fabs dist2 < min_distance)
               && (fabs (snd distance) > fabs dist2)
               && (fabs dist2 <= fabs dist1)
               && (fabs dist2 <= fabs dist3)
          then Hcenter, dist2
          else if (fabs dist3 < min_distance)
               && (fabs (snd distance) > fabs dist3)
               && (fabs dist3 <= fabs dist1)
               && (fabs dist3 <= fabs dist2)
          then Hbottom, dist3
          else distance
        | Hbottom ->
          let dist1 = pos.y +. pos.h -. hd.y in
          let dist2 = pos.y +. pos.h -. hd.y -. hd.h /. 2.0 in
          let dist3 = pos.y +. pos.h -. hd.y -. hd.h in
          if (fabs dist1 < min_distance)
          && (fabs (snd distance) > fabs dist1)
          && (fabs dist1 <= fabs dist2)
          && (fabs dist1 <= fabs dist3)
          then Htop, dist1
          else if (fabs dist2 < min_distance)
               && (fabs (snd distance) > fabs dist2)
               && (fabs dist2 <= fabs dist1)
               && (fabs dist2 <= fabs dist3)
          then Hcenter, dist2
          else if (fabs dist3 < min_distance)
               && (fabs (snd distance) > fabs dist3)
               && (fabs dist3 <= fabs dist1)
               && (fabs dist3 <= fabs dist2)
          then Hbottom, dist3
          else distance
        | Vleft ->
          let dist1 = pos.x -. hd.x in
          let dist2 = pos.x -. hd.x -. hd.w /. 2.0 in
          let dist3 = pos.x -. hd.x -. hd.w in
          if (fabs dist1 < min_distance)
          && (fabs (snd distance) > fabs dist1)
          && (fabs dist1 <= fabs dist2)
          && (fabs dist1 <= fabs dist3)
          then Vleft, dist1
          else if (fabs dist2 < min_distance)
               && (fabs (snd distance) > fabs dist2)
               && (fabs dist2 <= fabs dist1)
               && (fabs dist2 <= fabs dist3)
          then Vcenter, dist2
          else if (fabs dist3 < min_distance)
               && (fabs (snd distance) > fabs dist3)
               && (fabs dist3 <= fabs dist1)
               && (fabs dist3 <= fabs dist2)
          then Vright, dist3
          else distance
        | Vcenter ->
          let dist1 = pos.x +. pos.w /. 2.0 -. hd.x in
          let dist2 = pos.x +. pos.w /. 2.0 -. hd.x -. hd.w /. 2.0 in
          let dist3 = pos.x +. pos.w /. 2.0 -. hd.x -. hd.w in
          if (fabs dist1 < min_distance)
          && (fabs (snd distance) > fabs dist1)
          && (fabs dist1 <= fabs dist2)
          && (fabs dist1 <= fabs dist3)
          then Vleft, dist1
          else if (fabs dist2 < min_distance)
               && (fabs (snd distance) > fabs dist2)
               && (fabs dist2 <= fabs dist1)
               && (fabs dist2 <= fabs dist3)
          then Vcenter, dist2
          else if (fabs dist3 < min_distance)
               && (fabs (snd distance) > fabs dist3)
               && (fabs dist3 <= fabs dist1)
               && (fabs dist3 <= fabs dist2)
          then Vright, dist3
          else distance
        | Vright ->
          let dist1 = pos.x +. pos.w -. hd.x in
          let dist2 = pos.x +. pos.w -. hd.x -. hd.w /. 2.0 in
          let dist3 = pos.x +. pos.w -. hd.x -. hd.w in
          if (fabs dist1 < min_distance)
          && (fabs (snd distance) > fabs dist1)
          && (fabs dist1 <= fabs dist2)
          && (fabs dist1 <= fabs dist3)
          then Vleft, dist1
          else if (fabs dist2 < min_distance)
               && (fabs (snd distance) > fabs dist2)
               && (fabs dist2 <= fabs dist1)
               && (fabs dist2 <= fabs dist3)
          then Vcenter, dist2
          else if (fabs dist3 < min_distance)
               && (fabs (snd distance) > fabs dist3)
               && (fabs dist3 <= fabs dist1)
               && (fabs dist3 <= fabs dist2)
          then Vright, dist3
          else distance
        | Nill -> distance
      in
      count_aligns line_align_val distance tl
  in
  count_aligns line_align (Nill, (min_distance +. 1.0)) siblings

(* min_distance - pixels
   return: counts of align of selected type in min_distance interval *)
let line_align_count
    (pos : t)
    (siblings : t list)
    (min_distance : float)
    line_align_val =
  let rec aux line_align_val counts = function
    | [] -> counts
    | (hd : t) :: tl ->
      let counts =
        if (line_align_val = Htop
            && (fabs (pos.y -. hd.y) < min_distance
                || fabs (pos.y -. hd.y -. hd.h /. 2.0) < min_distance
                || fabs (pos.y -. hd.y -. hd.h) < min_distance))
        || (line_align_val = Hcenter
            && (fabs (pos.y +. pos.h /. 2.0 -. hd.y) < min_distance
                || fabs (pos.y +. pos.h /. 2.0 -. hd.y -. hd.h /. 2.0) < min_distance
                || fabs (pos.y +. pos.h /. 2.0 -. hd.y -. hd.h) < min_distance))
        || (line_align_val = Hbottom
            && (fabs (pos.y +. pos.h -. hd.y) < min_distance
                || fabs (pos.y +. pos.h -. hd.y -. hd.h /. 2.0) < min_distance
                || fabs (pos.y +. pos.h -. hd.y -. hd.h) < min_distance))
        || (line_align_val = Vleft
            && (fabs (pos.x -. hd.x) < min_distance
                || fabs (pos.x -. hd.x -. hd.w /. 2.0) < min_distance
                || fabs (pos.x -. hd.x -. hd.w) < min_distance))
        || (line_align_val = Vcenter
            && (fabs (pos.x +. pos.w /. 2.0 -. hd.x) < min_distance
                || fabs (pos.x +. pos.w /. 2.0 -. hd.x -. hd.w /. 2.0) < min_distance
                || fabs (pos.x +. pos.w /. 2.0 -. hd.x -. hd.w) < min_distance))
        || (line_align_val = Vright
            && (fabs (pos.x +. pos.w -. hd.x) < min_distance
                || fabs (pos.x +. pos.w -. hd.x -. hd.w /. 2.0) < min_distance
                || fabs (pos.x +. pos.w -. hd.x -. hd.w) < min_distance))
        then succ counts
        else counts
      in
      aux line_align_val counts tl
  in
  aux line_align_val 0 siblings

let make_line_properties align pos min_distance items =
  align,
  line_align_count pos items min_distance align,
  line_find_closest_align pos items min_distance align

(* return: direction, count aligns (0 = none align lines),
   closest line distance (if distance > min_distance = no find lines) *)
let hlines_for_move_action pos min_distance siblings =
  [ make_line_properties Htop pos min_distance siblings
  ; make_line_properties Hcenter pos min_distance siblings
  ; make_line_properties Hbottom pos min_distance siblings
  ]

let hlines_for_resize_action pos min_distance siblings (direction : direction) =
  let align_direction = match direction with
    | NW | NE | N -> Htop
    | SW | SE | S -> Hbottom
    | W | E -> Hcenter in
  [make_line_properties align_direction pos min_distance siblings]

let vlines_for_move_action pos min_distance siblings =
  [ make_line_properties Vleft pos min_distance siblings
  ; make_line_properties Vcenter pos min_distance siblings
  ; make_line_properties Vright pos min_distance siblings
  ]

let vlines_for_resize_action pos min_distance siblings (direction : direction) =
  let align_direction = match direction with
    | NW | SW | W -> Vleft
    | NE | SE | E -> Vright
    | N | S -> Vcenter in
  [make_line_properties align_direction pos min_distance siblings]

let get_snap (coord : float) (min_distance : float)
    (items : (line_align_direction * int * (line_align_direction * float)) list) =
  let rec aux (snap : float) (snap_min_delta : float)
      (items: (line_align_direction * int * (line_align_direction * float)) list)
    :float
    = match items with
    | [] -> snap
    | (_, aligns_count, distance__align_other) :: tl ->
      let (_ , distance) = distance__align_other in
      let snap_min_delta =
        if aligns_count > 0 && fabs distance < fabs snap_min_delta
        then distance else snap_min_delta in
      let snap =
        if fabs snap_min_delta <= min_distance
        then coord -. snap_min_delta else snap in
      aux snap snap_min_delta tl in
  aux coord (min_distance +. 1.0) items  (* FIX + 1.0; (1.0 != 1 pixel) *)

let get_item_snap_y (pos : t) min_distance siblings =
  get_snap pos.y min_distance @@ hlines_for_move_action pos min_distance siblings

let get_item_snap_x (pos : t) min_distance siblings =
  get_snap pos.x min_distance @@ vlines_for_move_action pos min_distance siblings

let snap_to_siblings_move (pos : t) min_distance siblings : t =
  { x = get_item_snap_x pos min_distance siblings
  ; y = get_item_snap_y pos min_distance siblings
  ; w = pos.w
  ; h = pos.h
  }

let snap_to_siblings_resize (pos : t) min_distance siblings =
  let make_line align = make_line_properties align pos min_distance siblings in
  function
  | NW ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Htop] in
    ({ x = get_snap pos.x min_distance snap_list_x
     ; y = get_snap pos.y min_distance snap_list_y
     ; w = pos.x -. (get_snap pos.x min_distance snap_list_x) +. pos.w
     ; h = pos.y -. (get_snap pos.y min_distance snap_list_y) +. pos.h
     } : t)
  | NE ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Htop] in
    { x = pos.x
    ; y = get_snap pos.y min_distance snap_list_y
    ; w = (get_snap pos.x min_distance snap_list_x) -. pos.x +. pos.w
    ; h = pos.y -. (get_snap pos.y min_distance snap_list_y) +. pos.h
    }
  | SW ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hbottom] in
    { x = get_snap pos.x min_distance snap_list_x
    ; y = pos.y
    ; w = pos.x -. (get_snap pos.x min_distance snap_list_x) +. pos.w
    ; h = (get_snap pos.y min_distance snap_list_y) -. pos.y +. pos.h
    }
  | SE ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hbottom] in
    { x = pos.x
    ; y = pos.y
    ; w = (get_snap pos.x min_distance snap_list_x) -. pos.x +. pos.w
    ; h = (get_snap pos.y min_distance snap_list_y) -. pos.y +. pos.h
    }
  | N ->
    let snap_list_y = [make_line Htop] in
    { x = pos.x
    ; y = get_snap pos.y min_distance snap_list_y
    ; w = pos.w
    ; h = pos.y -. (get_snap pos.y min_distance snap_list_y) +. pos.h
    }
  | S ->
    let snap_list_y = [make_line Hbottom] in
    { x = pos.x
    ; y = pos.y
    ; w = pos.w
    ; h = (get_snap pos.y min_distance snap_list_y) -. pos.y +. pos.h
    }
  | W ->
    let snap_list_x = [make_line Vleft] in
    { x = get_snap pos.x min_distance snap_list_x
    ; y = pos.y
    ; w = pos.x -. (get_snap pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.h
    }
  | E ->
    let snap_list_x = [make_line Vright] in
    { x = pos.x
    ; y = pos.y
    ; w = (get_snap pos.x min_distance snap_list_x) -. pos.x +. pos.w
    ; h = pos.h
    }

(* glue lines to its item *)
let get_snap_lines (pos : t) siblings min_distance
    (action : [`Resize of direction | `Move]) =
  let snap_list =
    match action with
    | `Move ->
      vlines_for_move_action pos 1.0 siblings
      @ hlines_for_move_action pos 1.0 siblings
    | `Resize dir ->
      vlines_for_resize_action pos 1.0 siblings dir
      @ hlines_for_resize_action pos 1.0 siblings dir
  in
  let rec create_lines action acc = function
    | [] -> acc
    | (direction, aligns_count, (align_other, _)) :: tl ->
      let acc =
        if aligns_count > 0
        then
          let is_vertical = match direction with
            | Vleft | Vright | Vcenter -> true
            | Nill | Htop | Hbottom | Hcenter -> false in
          let is_center = match align_other with
            | Vcenter | Hcenter -> true
            | _ -> false in
          let origin = match direction with
            | Vleft -> pos.x
            | Vcenter -> pos.x +. pos.w /. 2.0
            | Vright -> pos.x +. pos.w
            | Htop -> pos.y
            | Hcenter -> pos.y +. pos.h /. 2.0
            | Hbottom -> pos.y +. pos.h
            | Nill -> 0.0 in
          let line_ret =
            ({ is_vertical
             ; is_multiple = aligns_count > 1
             ; is_center
             ; origin
             } : line) in
          line_ret :: acc
        else acc in
      create_lines action acc tl in
  create_lines action [] snap_list

let clip_to_parent ({ w; h; x; y } as pos : t)
    ~parent_size:(parent_w, parent_h)
    (min_w : float)
    (min_h : float) = function
  | `Move -> fix_xy ~parent_w ~parent_h pos
  | `Resize direction ->
    let (max_x, max_y, min_x, min_y) =
      match direction with
      | NW -> Some (x +. w -. min_w), Some (y +. h -. min_h), None, None
      | NE -> None, Some (y +. h -. min_h), Some x, None
      | SW -> Some (x +. w -. min_w), None, None, Some y
      | SE -> None, None, Some x, Some y
      | N -> Some (x +. w -. min_w), Some (y +. h -. min_h), None, None
      | E -> None, None, Some (x), None
      | W -> Some (x +. w -. min_w), None, None, Some y
      | S -> Some (x +. w -. min_w), None, None, None
    in
    fix ?min_x ?max_x ?min_y ?max_y ~min_w ~min_h ~parent_w ~parent_h pos

let snap_to_grid_move (pos : t) (grid_step : float) : t =
  let x = Js.math##round (pos.x /. grid_step) *. grid_step in
  let y = Js.math##round (pos.y /. grid_step) *. grid_step in
  { pos with x; y }

let snap_to_grid_resize (direction : direction) (pos : t) (grid_step : float) : t =
  match direction with
  | NW ->
    let x = Js.math##round (pos.x /. grid_step) *. grid_step in
    let y = Js.math##round (pos.y /. grid_step) *. grid_step in
    let w = pos.w +. pos.x -. x in
    let h = pos.h +. pos.y -. y in
    { x; y; h; w }
  | NE ->
    let y = Js.math##round (pos.y /. grid_step) *. grid_step in
    let w = Js.math##round (pos.w /. grid_step) *. grid_step in
    let h = pos.h +. pos.y -. y in
    { pos with y; h; w }
  | SW ->
    let x = Js.math##round (pos.x /. grid_step) *. grid_step in
    let w = pos.w +. pos.x -. x in
    let h = Js.math##round (pos.h /. grid_step) *. grid_step in
    { pos with x; w; h }
  | SE ->
    let w = Js.math##round (pos.w /. grid_step) *. grid_step in
    let h = Js.math##round (pos.h /. grid_step) *. grid_step in
    { pos with w; h }
  | N ->
    let y = Js.math##round (pos.y /. grid_step) *. grid_step in
    let h = pos.h +. pos.y -. y in
    { pos with y; h }
  | S -> { pos with h = Js.math##round (pos.h /. grid_step) *. grid_step }
  | W ->
    let x = Js.math##round (pos.x /. grid_step) *. grid_step in
    let w = pos.w +. pos.x -. x in
    { pos with x; w }
  | E -> { pos with w = Js.math##round (pos.w /. grid_step) *. grid_step }

let move_children (rect_position : t) (children : t list) =
  let open Pipeline_types.Wm in
  let pos_left = List.hd
      (List.sort Stdlib.compare
         (List.map (fun v -> v.x ) children)) in
  let pos_top = List.hd
      (List.sort Stdlib.compare
         (List.map (fun v -> v.y ) children)) in
  List.map
    (fun v -> let x = v.x -. pos_left in
      let y = v.y -. pos_top in
      {v with x = rect_position.x +. x ;  y = rect_position.y +. y} )
    children

let get_float_aspect (aspect : int * int)  =
  let asp =
    if fst aspect = 0
    then 1.0
    else (float_of_int (snd aspect)) /. (float_of_int (fst aspect)) in
  if asp <= 0.0 then 1.0 else asp

let get_min_rect_size_for_aspect
    ~(aspect : float)
    ~(min_width : float)
    ~(min_height : float) = function
  | [] -> min_width, min_height
  | children ->
    let bound = bounding_rect children in
    if min_width > min_height
    then
      let child_min_w =
        List.hd
        @@ List.sort (fun (a : t) b -> Stdlib.compare a.w b.w) children in
      let w = bound.w *. min_width /. child_min_w.w in
      let h = w *. aspect in
      w, h
    else
      let child_min_h =
        List.hd
        @@ List.sort (fun (a : t) b -> Stdlib.compare a.h b.h) children in
      let h = bound.h *. min_height /. child_min_h.h in
      let w = h /. aspect in
      w, h

let get_min_rect_size ~(min_width : float) ~(min_height : float) = function
  | [] -> min_width, min_height
  | children ->
    let child_min_w =
      List.hd
      @@ List.sort (fun (a : t) b -> Stdlib.compare a.w b.w) children in
    let child_min_h =
      List.hd
      @@ List.sort (fun (a : t) b -> Stdlib.compare a.h b.h) children in
    let bound = bounding_rect children in
    bound.w *. min_width /. child_min_w.w,
    bound.h *. min_height /. child_min_h.h

let fix_aspect_min
    (dir : direction)
    (pos : t)
    (orig_pos : t)
    (asp : float)
    (children : t list)
    (min_width : float)
    (min_height : float) =
  let open Pipeline_types.Wm in
  let (min_w, min_h) = get_min_rect_size_for_aspect
      ~aspect:asp
      ~min_width
      ~min_height
      children in
  if pos.h < min_h || pos.w < min_w then
    match dir with
    | SE ->
      { x = orig_pos.x
      ; y = orig_pos.y
      ; w = min_w
      ; h = min_h
      }
    | NW ->
      { x = orig_pos.x +. orig_pos.w -. min_w
      ; y = orig_pos.y +. orig_pos.h -. min_h
      ; w = min_w
      ; h = min_h
      }
    | S | W | SW ->
      { x = orig_pos.x +. orig_pos.w -. min_w
      ; y = orig_pos.y
      ; w = min_w
      ; h = min_h
      }
    | N | E | NE ->
      { x = orig_pos.x
      ; y = orig_pos.y +. orig_pos.h -. min_h
      ; w = min_w
      ; h = min_h
      }
  else pos

let get_max_wh_for_aspect
    (dir : direction)
    (x, y, w, h) (* input orig_pos *)
    (max_w : float)
    (max_h : float)
    (asp : float) =
  let (w1, h1, w2, h2) = match dir with
    | NW ->
      let w1 = x +. w in
      let h1 = w1 *. asp in
      let h2 = y +. h in
      let w2 = h2 /. asp in
      (w1, h1, w2, h2)
    | NE ->
      let w1 = max_w -. x in
      let h1 = w1 *. asp in
      let h2 = y +. h in
      let w2 = h2 /. asp in
      (w1, h1, w2, h2)
    | SE ->
      let w1 = max_w -. x in
      let h1 = w1 *. asp in
      let h2 = max_h -. y in
      let w2 = h2 /. asp in
      (w1, h1, w2, h2)
    | SW ->
      let w1 = x +. w in
      let h1 = w1 *. asp in
      let h2 = max_h -. y in
      let w2 = h2 /. asp in
      (w1, h1, w2, h2)
    | N | E ->
      let w1 = max_w -. x in
      let h1 = w1 *. asp in
      let h2 = y +. h in
      let w2 = h2 /. asp in
      (w1, h1, w2, h2)
    | S | W ->
      let w1 = x +. w in
      let h1 = w1 *. asp in
      let h2 = max_h -. y in
      let w2 = h2 /. asp in
      (w1, h1, w2, h2)
  in
  if w1 < w2 then (w1, h1) else (w2, h2)

let fix_aspect_max
    (dir : direction)
    (pos : t)
    (orig_pos : t)
    (asp : float)
    (max_width : float)
    (max_height : float) =
  let open Pipeline_types.Wm in
  let (max_w, max_h) = get_max_wh_for_aspect dir
      (orig_pos.x, orig_pos.y, orig_pos.w, orig_pos.h)
      max_width max_height asp in
  if pos.h > max_h || pos.w > max_w then
    match dir with
    | NW -> { x = orig_pos.x +. orig_pos.w -. max_w
            ; y = orig_pos.y +. orig_pos.h -. max_h
            ; w = max_w
            ; h = max_h
            }
    | NE -> { x = orig_pos.x
            ; y = orig_pos.y +. orig_pos.h -. max_h
            ; w = max_w
            ; h = max_h
            }
    | SE -> { x = orig_pos.x
            ; y = orig_pos.y
            ; w = max_w
            ; h = max_h
            }
    | SW -> { x = orig_pos.x +. orig_pos.w -. max_w
            ; y = orig_pos.y
            ; w = max_w
            ; h = max_h
            }
    | N -> { x = orig_pos.x
           ; y = orig_pos.y +. orig_pos.h -. max_h
           ; w = max_w
           ; h = max_h
           }
    | S -> { x = orig_pos.x +. orig_pos.w -. max_w
           ; y = orig_pos.y
           ; w = max_w
           ; h = max_h
           }
    | W -> { x = orig_pos.x +. orig_pos.w -. max_w
           ; y = orig_pos.y
           ; w = max_w
           ; h = max_h
           }
    | E -> { x = orig_pos.x
           ; y = orig_pos.y +. orig_pos.h -. max_h
           ; w = max_w
           ; h = max_h
           }
  else pos

let fix_aspect_after_snap
    (dir : direction)
    (orig_pos : t)
    (before_pos : t)
    (after_pos : t)
    (aspect : int * int)
    (min_width : float)
    (min_height : float)
    (children : t list)
    (max_width : float)
    (max_height : float) =
  let open Pipeline_types.Wm in
  let asp = get_float_aspect aspect in
  let h = after_pos.w *. asp in
  let w = after_pos.h /. asp in
  let p1 = match dir with
    | NW ->
      if fabs (before_pos.w -. after_pos.w)
         >= fabs (before_pos.h -. after_pos.h)
      then { after_pos with y = orig_pos.y +. orig_pos.h -. h; h }
      else { after_pos with x = orig_pos.x +. orig_pos.w -. w; w }
    | NE ->
      if fabs (before_pos.w -. after_pos.w)
         >= fabs (before_pos.h -. after_pos.h)
      then { after_pos with y = orig_pos.y +. orig_pos.h -. h; h }
      else { after_pos with x = orig_pos.x; w }
    | SE ->
      if fabs (before_pos.w -. after_pos.w)
         >= fabs (before_pos.h -. after_pos.h)
      then { after_pos with h }
      else { after_pos with w }
    | SW ->
      if fabs (before_pos.w -. after_pos.w)
         >= fabs (before_pos.h -. after_pos.h)
      then { after_pos with h }
      else { after_pos with x = orig_pos.x +. orig_pos.w -. w; w }
    | N -> { after_pos with x = orig_pos.x; w }
    | S -> { after_pos with x = orig_pos.x +. orig_pos.w -. w; w }
    | W ->
      { after_pos with
        y = orig_pos.y;
        h = (after_pos.w *. asp) }
    | E -> { after_pos with
             y = orig_pos.y +. orig_pos.h -. h;
             h = (after_pos.w *. asp) }
  in
  p1

(* FIXME cannot read arguments purpose from signature *)
let fix_aspect2
    (aspect_ratio : (int * int) option)
    (action : [`Resize of direction | `Move])
    (children : t list)
    (orig_pos : t)
    (before_pos : t)
    (after_pos : t)
    (min_width : float)
    (min_height : float)
    (max_width : float)
    (max_height : float) =
  match aspect_ratio with
  | None -> after_pos
  | Some x -> match action with
    | `Move ->
      let p1 = fix_aspect_after_snap SE
          orig_pos before_pos after_pos x
          min_width min_height children max_width max_height
      in
      let p2 = fix_aspect_min
          SE
          p1
          orig_pos
          (get_float_aspect x)
          children
          min_width
          min_height in
      fix_aspect_max SE p2 orig_pos (get_float_aspect x) max_width max_height
    | `Resize resz ->
      let p1 = fix_aspect_after_snap resz
          orig_pos before_pos after_pos x
          min_width min_height children max_width max_height
      in
      let p2 = fix_aspect_min
          resz
          p1
          orig_pos
          (get_float_aspect x)
          children
          min_width
          min_height in
      fix_aspect_max resz p2 orig_pos (get_float_aspect x) max_width max_height

let resize_children
    ({ x; y; w; h } : t)
    (children : t list)
    (min_width : float)
    (min_height : float) =
  let open Pipeline_types.Wm in
  let bound = bounding_rect children in (* FIXME what is this? *)
  let scale_w = w /. (if bound.w <= 0.0 then 1.0 else bound.w) in
  let scale_h = h /. (if bound.h <= 0.0 then 1.0 else bound.h) in
  List.map
    (fun v ->
       { x = x +. (v.x -. bound.x) *. scale_w
       ; y = y +. (v.y -. bound.y) *. scale_h
       ; w = v.w *. scale_w
       ; h = v.h *. scale_h
       })
    children

let adjust ?aspect_ratio
    ?(snap_lines = true)
    ?(collisions = false) (* need if we used collides *)
    ?(min_width = 20.)
    ?(min_height = 20.)
    ?(min_distance = 12.)
    ?grid_step
    ?max_width
    ?max_height
    ~(action : [`Resize of direction | `Move])
    ~(siblings : t list) (* widget positions int coordinatrs to float [0;1.0] *)
    ~(parent_size : float * float) (* need if input positions is int pixel coordinates *)
    ~(frame_position : t)
    (positions : t list) =
  let parent_w, parent_h = parent_size in
  let position = frame_position in
  let original_position = bounding_rect positions in
  let position = match grid_step, action with
    | None, _ -> position
    | Some step, `Move -> snap_to_grid_move position step
    | Some step, `Resize dir -> snap_to_grid_resize dir position step
  in
  let position_asp = fix_aspect2
      aspect_ratio
      action
      positions
      original_position
      position
      position
      min_width
      min_height
      parent_w
      parent_h
  in
  let position_snap = match snap_lines, action with
    | false, _ -> position_asp
    | true, `Move ->
      snap_to_siblings_move position_asp min_distance siblings
    | true, `Resize resz ->
      snap_to_siblings_resize position_asp min_distance siblings resz
  in
  let position_asp =
    fix_aspect2
      aspect_ratio
      action
      positions
      original_position
      position_asp
      position_snap
      min_width
      min_height
      parent_w
      parent_h
  in
  let min_w, min_h = get_min_rect_size ~min_width ~min_height positions in
  let position_clip_parent = match aspect_ratio with
    | None -> clip_to_parent ~parent_size position_asp min_w min_h action
    | Some _ -> clip_to_parent ~parent_size position_asp 0.0 0.0 action  (* not calc min sizes *)
  in
  let snap_lines =
    if snap_lines
    then get_snap_lines position_clip_parent siblings min_distance action
    else [] in
  let children = match action with
    | `Move ->
      move_children
        position_clip_parent
        (resize_children position_clip_parent positions min_w min_h)
    | `Resize resz ->
      resize_children position_clip_parent positions min_w min_h
  in
  position_clip_parent, children, snap_lines
