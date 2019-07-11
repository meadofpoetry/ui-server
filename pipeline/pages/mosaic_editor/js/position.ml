open Js_of_ocaml
open Components

let ( % ) f g x = f (g x)

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : int
  }

type resize_direction =
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | Top
  | Bottom
  | Left
  | Right

type line_align_direction =
  | Htop
  | Hcenter
  | Hbottom
  | Vleft
  | Vcenter
  | Vright
  | Nill


type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

let empty =
  { x = 0
  ; y = 0
  ; w = 0
  ; h = 0
  }

let show { x; y; w; h } =
  Printf.sprintf "x=%d, y=%d, w=%d, h=%d"
    x y w h

let compare (a : t) (b : t) =
  let c = compare a.x b.x in
  if c <> 0 then c
  else (let c = compare a.y b.y in
        if c <> 0 then c
        else (let c = compare a.w b.w in
              if c <> 0 then c
              else compare a.h b.h))

let equal (a : t) (b : t) =
  a.x = b.x && a.y = b.y && a.w = b.w && a.h = b.h

(** Checks if two elements collide, returns [true] if so and [false] otherwise *)
let collides (pos1 : t) (pos2 : t) =
  if (pos1.x + pos1.w <= pos2.x) then false
  else if (pos1.x >= pos2.x + pos2.w) then false
  else if (pos1.y + pos1.h <= pos2.y) then false
  else if (pos1.y >= pos2.y + pos2.h) then false
  else true

(** Returns first collision *)
let collision_map
    ~(f : 'a -> t)
    (pos : 'a)
    (l : 'a list) =
  let rec aux = function
    | [] -> None
    | hd :: tl ->
      if collides (f pos) (f hd)
      then Some hd
      else aux tl
  in
  aux l

(** Returns all collisions *)
let collisions_map
    ~(f : 'a -> t)
    (item : 'a)
    (l : 'a list) =
  List.fold_left (fun acc (x : 'a) ->
      if collides (f item) (f x)
      then x :: acc
      else acc) [] l

(** Checks if element collides with other elements *)
let has_collision_map ~f x (l : 'a list) =
  match collision_map ~f x l with
  | None -> false | Some _ -> true

let collision x l = collision_map ~f:(fun x -> x) x l

let collisions x l = collisions_map ~f:(fun x -> x) x l

let has_collision x l = has_collision_map ~f:(fun x -> x) x l

(** Changes top and left coordinates to correspond parent dimentions *)
(* original
   let fix_xy par_w par_h (p : t) =
   let x = if p.x < 0 then 0 else if p.x + p.w > par_w then par_w - p.w else p.x in
   let y =
    match par_h with
    | None -> if p.y < 0 then 0 else p.y
    | Some ph -> if p.y < 0 then 0 else if p.y + p.h > ph then ph - p.h else p.y
   in
   { p with x; y }
*)

(** Changes top and left coordinates to correspond parent dimentions *)
let fix_xy ?min_x ?min_y ?max_x ?max_y par_w par_h (p : t) =
  let x = if p.x < 0 then 0 else if p.x + p.w > par_w then par_w - p.w else p.x in
  let y =
    match par_h with
    | None -> if p.y < 0 then 0 else p.y
    | Some ph -> if p.y < 0 then 0 else if p.y + p.h > ph then ph - p.h else p.y
  in
  let x = match max_x with
    | Some max -> if x > max then max else x
    | None -> x
  in
  let x = match min_x with
    | Some min -> if x > min then min else x
    | None -> x
  in
  let y = match max_y with
    | Some max -> if y > max then max else y
    | None -> y
  in
  let y = match min_y with
    | Some min -> if y > min then min else y
    | None -> y
  in
  { p with x; y }

(** Changes width to correspond provided constraints *)
let fix_w ?max_w ?(min_w = 1) par_w (p : t) =
  let w = match max_w with
    | Some max ->
      if p.w > max then max
      else if p.w < min_w then min_w
      else p.w
    | None -> if p.w < min_w then min_w else p.w
  in
  let w =
    if p.x + w > par_w
    then par_w - p.x
    else if p.x < 0
    then w + p.x
    else w in
  { p with w }

(** Changes height to correspond provided constraints *)
let fix_h ?max_h ?(min_h = 1) par_h (p : t) =
  let h = match max_h with
    | Some max -> if p.h > max then max else if p.h < min_h then min_h else p.h
    | None -> if p.h < min_h then min_h else p.h
  in
  let h = match par_h with
    | Some ph ->
      if p.y + h > ph
      then ph - p.y
      else if p.y < 0
      then h + p.y
      else h
    | None    -> h
  in
  { p with h }

(** Changes width and height to correspond provided constraints *)
let fix_wh ?max_w ?min_w ?max_h ?min_h par_w par_h =
  (fix_h ?max_h ?min_h par_h) % (fix_w ?max_w ?min_w par_w)

(** Changes width and height to correspond provided aspect *)
let fix_aspect (p : t) (aspect : int * int) =
  let w =
    if p.w mod (fst aspect) <> 0 then
      let w = (p.w / (fst aspect)) * (fst aspect) in
      if w = 0 then (fst aspect)  else w
    else p.w
  in
  let h =
    if p.h mod (snd aspect) <> 0 then
      let h = (p.h / (snd aspect)) * (snd aspect) in
      if h = 0 then (snd aspect) else h
    else p.h
  in
  let sw = w / (fst aspect) in
  let sh = h / (snd aspect) in
  let w, h =
    if sw > sh
    then (fst aspect) * sh, h
    else w, (snd aspect) * sw
  in
  { p with w; h }

let apply_to_element (pos : t) (elt : #Dom_html.element Js.t) =
  elt##.style##.width := Utils.px_js pos.w;
  elt##.style##.left := Utils.px_js pos.x;
  elt##.style##.height := Utils.px_js pos.h;
  elt##.style##.top := Utils.px_js pos.y

let of_element (elt : #Dom_html.element Js.t) =
  { x = elt##.offsetLeft
  ; y = elt##.offsetTop
  ; w = elt##.offsetWidth
  ; h = elt##.offsetHeight
  }

let to_client_rect (t : t) : Dom_html.clientRect Js.t =
  object%js
    val top = float_of_int t.y
    val left = float_of_int t.x
    val right = float_of_int @@ t.x + t.w
    val bottom = float_of_int @@ t.y + t.h
    val width = Js.def @@ float_of_int t.w
    val height = Js.def @@ float_of_int t.h
  end

let of_client_rect (rect : Dom_html.clientRect Js.t) : t =
  { x = int_of_float rect##.left
  ; y = int_of_float rect##.top
  ; w = Js.Optdef.case rect##.width (fun () -> 0) int_of_float
  ; h = Js.Optdef.case rect##.height (fun () -> 0) int_of_float
  }

let default_aspect_ratio = 1.

let string_of_float = Printf.sprintf "%g"

(* min_distance - pixels
   return: (other element align as line_align_direction *
            minimum distance of several lines of one align as int) 
*)
let line_find_closest_align
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance
    line_align_val =
  let rec count_aligns line_align_val (distance: (line_align_direction * int)) = function
    | [] -> distance
    | hd :: tl ->
      let icompare = of_element hd in
      let distance =
        if Element.equal item hd
        then count_aligns line_align_val distance tl (*distance*) else
          match line_align_val with
          | Htop ->
            let dist1 = pos.y - icompare.y in
            let dist2 = pos.y - icompare.y - icompare.h / 2 in
            let dist3 = pos.y - icompare.y - icompare.h in
            if (abs dist1 < min_distance)
            && (abs (snd distance) > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then Htop, dist1
            else if (abs dist2 < min_distance)
                 && (abs (snd distance) > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then Hcenter, dist2
            else if (abs dist3 < min_distance)
                 && (abs (snd distance) > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then Hbottom, dist3
            else distance
          | Hcenter ->
            let dist1 = pos.y + pos.h / 2 - icompare.y in
            let dist2 = pos.y + pos.h / 2 - icompare.y - icompare.h / 2 in
            let dist3 = pos.y + pos.h / 2 - icompare.y - icompare.h in
            if (abs dist1 < min_distance)
            && (abs (snd distance) > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then Htop, dist1
            else if (abs dist2 < min_distance)
                 && (abs (snd distance) > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then Hcenter, dist2
            else if (abs dist3 < min_distance)
                 && (abs (snd distance) > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then Hbottom, dist3
            else distance
          | Hbottom ->
            let dist1 = pos.y + pos.h - icompare.y in
            let dist2 = pos.y + pos.h - icompare.y - icompare.h / 2 in
            let dist3 = pos.y + pos.h - icompare.y - icompare.h in
            if (abs dist1 < min_distance)
            && (abs (snd distance) > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then Htop, dist1
            else if (abs dist2 < min_distance)
                 && (abs (snd distance) > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then Hcenter, dist2
            else if (abs dist3 < min_distance)
                 && (abs (snd distance) > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then Hbottom, dist3
            else distance
          | Vleft ->
            let dist1 = pos.x - icompare.x in
            let dist2 = pos.x - icompare.x - icompare.w / 2 in
            let dist3 = pos.x - icompare.x - icompare.w in
            if (abs dist1 < min_distance)
            && (abs (snd distance) > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then Vleft, dist1
            else if (abs dist2 < min_distance)
                 && (abs (snd distance) > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then Vcenter, dist2
            else if (abs dist3 < min_distance)
                 && (abs (snd distance) > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then Vright, dist3
            else distance
          | Vcenter ->
            let dist1 = pos.x + pos.w / 2 - icompare.x in
            let dist2 = pos.x + pos.w / 2 - icompare.x - icompare.w / 2 in
            let dist3 = pos.x + pos.w / 2 - icompare.x - icompare.w in
            if (abs dist1 < min_distance)
            && (abs (snd distance) > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then Vleft, dist1
            else if (abs dist2 < min_distance)
                 && (abs (snd distance) > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then Vcenter, dist2
            else if (abs dist3 < min_distance)
                 && (abs (snd distance) > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then Vright, dist3
            else distance
          | Vright ->
            let dist1 = pos.x + pos.w - icompare.x in
            let dist2 = pos.x + pos.w - icompare.x - icompare.w / 2 in
            let dist3 = pos.x + pos.w - icompare.x - icompare.w in
            if (abs dist1 < min_distance)
            && (abs (snd distance) > abs dist1)
            && (abs dist1 <= abs dist2)
            && (abs dist1 <= abs dist3)
            then Vleft, dist1
            else if (abs dist2 < min_distance)
                 && (abs (snd distance) > abs dist2)
                 && (abs dist2 <= abs dist1)
                 && (abs dist2 <= abs dist3)
            then Vcenter, dist2
            else if (abs dist3 < min_distance)
                 && (abs (snd distance) > abs dist3)
                 && (abs dist3 <= abs dist1)
                 && (abs dist3 <= abs dist2)
            then Vright, dist3
            else distance
          | Nill -> distance
      in
      count_aligns line_align_val distance tl
  in
  count_aligns line_align_val (Nill, (min_distance + 1)) items

(* min_distance - pixels
   return: counts of align of selected type in min_distance interval *)
let line_align_count
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance
    line_align_val =
  let rec aux line_align_val counts = function
    | [] -> counts
    | hd :: tl ->
      let icompare = of_element hd in
      if Element.equal item hd
      then aux line_align_val counts tl (*counts*)
      else
        let counts =
          if (line_align_val = Htop
              && (abs (pos.y - icompare.y) < min_distance
                  || abs (pos.y - icompare.y - icompare.h / 2) < min_distance
                  || abs (pos.y - icompare.y - icompare.h) < min_distance))
          || (line_align_val = Hcenter
              && (abs (pos.y + pos.h / 2 - icompare.y) < min_distance
                  || abs (pos.y + pos.h / 2 - icompare.y - icompare.h / 2) < min_distance
                  || abs (pos.y + pos.h / 2 - icompare.y - icompare.h) < min_distance))
          || (line_align_val = Hbottom
              && (abs (pos.y + pos.h - icompare.y) < min_distance
                  || abs (pos.y + pos.h - icompare.y - icompare.h / 2) < min_distance
                  || abs (pos.y + pos.h - icompare.y - icompare.h) < min_distance))
          || (line_align_val = Vleft
              && (abs (pos.x - icompare.x) < min_distance
                  || abs (pos.x - icompare.x - icompare.w / 2) < min_distance
                  || abs (pos.x - icompare.x - icompare.w) < min_distance))
          || (line_align_val = Vcenter
              && (abs (pos.x + pos.w / 2 - icompare.x) < min_distance
                  || abs (pos.x + pos.w / 2 - icompare.x - icompare.w / 2) < min_distance
                  || abs (pos.x + pos.w / 2 - icompare.x - icompare.w) < min_distance))
          || (line_align_val = Vright
              && (abs (pos.x + pos.w - icompare.x) < min_distance
                  || abs (pos.x + pos.w - icompare.x - icompare.w / 2) < min_distance
                  || abs (pos.x + pos.w - icompare.x - icompare.w) < min_distance))
          then succ counts
          else counts
        in
        aux line_align_val counts tl
  in
  aux line_align_val 0 items

let make_line_properties align item pos min_distance items =
  align,
  line_align_count item pos items min_distance align,
  line_find_closest_align item pos items min_distance align

(* return: direction, count aligns (0 = none align lines),
   closest line distance (if distance > min_distance = no find lines) *)
let hlines_for_move_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  [ make_line_properties Htop item pos min_distance items
  ; make_line_properties Hcenter item pos min_distance items
  ; make_line_properties Hbottom item pos min_distance items
  ]

let hlines_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  let align_direction = match direction with
    | Top_left | Top_right | Top -> Htop
    | Bottom_left | Bottom_right | Bottom -> Hbottom
    | Left | Right -> Hcenter in
  [make_line_properties align_direction item pos min_distance items]

let vlines_for_move_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  [ make_line_properties Vleft item pos min_distance items
  ; make_line_properties Vcenter item pos min_distance items
  ; make_line_properties Vright item pos min_distance items
  ]

let vlines_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  let align_direction = match direction with
    | Top_left | Bottom_left | Left -> Vleft
    | Top_right | Bottom_right | Right -> Vright
    | Top | Bottom -> Vcenter in
  [make_line_properties align_direction item pos min_distance items]

let get_snap (item : Dom_html.element Js.t) coord min_distance items =
  let rec aux snap snap_min_delta = function
    | [] -> snap
    | (_, aligns_count, distance__align_other) :: tl ->
      let (_ , distance) = distance__align_other in
      let snap_min_delta =
        if aligns_count > 0 && abs distance < abs snap_min_delta
        then distance else snap_min_delta in
      let snap =
        if abs snap_min_delta <= min_distance
        then coord - snap_min_delta else snap in
      aux snap snap_min_delta tl in
  aux coord (min_distance + 1) items

let get_item_snap_y (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  get_snap item pos.y min_distance
  @@ hlines_for_move_action item pos min_distance items

let get_item_snap_x (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  get_snap item pos.x min_distance
  @@ vlines_for_move_action item pos min_distance items

let get_item_snap_position_for_move_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list) =
  { x = get_item_snap_x item pos min_distance items
  ; y = get_item_snap_y item pos min_distance items
  ; w = pos.w
  ; h = pos.h
  }

let get_item_snap_position_for_resize_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  let make_line align = make_line_properties align item pos min_distance items in
  match direction with
  | Top_left ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Htop] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Top_right ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Htop] in
    { x = pos.x (*get_snap item pos.x min_distance snap_list_x *)
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = (get_snap item pos.x min_distance snap_list_x) - pos.x + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Bottom_left ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hbottom] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = pos.y (*get_snap item pos.y min_distance snap_list_y *)
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = (get_snap item pos.y min_distance snap_list_y) - pos.y + pos.h
    }
  | Bottom_right ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hbottom] in
    { x = pos.x (*get_snap item pos.x min_distance snap_list_x *)
    ; y = pos.y (*get_snap item pos.y min_distance snap_list_y *)
    ; w = (get_snap item pos.x min_distance snap_list_x) - pos.x + pos.w
    ; h = (get_snap item pos.y min_distance snap_list_y) - pos.y + pos.h
    }
  | Top ->
    (* not tested *)
    let snap_list_x = [make_line Vcenter] in
    let snap_list_y = [make_line Htop] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Bottom ->
    (* not tested *)
    let snap_list_x = [make_line Vcenter] in
    let snap_list_y = [make_line Hbottom] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Left ->
    (* not tested *)
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hcenter] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }
  | Right ->
    (* not tested *)
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hcenter] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x - (get_snap item pos.x min_distance snap_list_x) + pos.w
    ; h = pos.y - (get_snap item pos.y min_distance snap_list_y) + pos.h
    }

(* glue lines to its item *)
let get_snap_lines
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance
    (action : [`Resize of resize_direction | `Move]) =
  let snap_list =
    match action with
    | `Move ->
      vlines_for_move_action item pos min_distance items
      @ hlines_for_move_action item pos min_distance items
    | `Resize dir ->
      vlines_for_resize_action item pos min_distance items dir
      @ hlines_for_resize_action item pos min_distance items dir
  in
  let rec create_lines action acc = function
    (* (inplist: (line_align_direction * int * (line_align_direction * int)) list )  (*= function *)
       = match inplist with*)
    | [] -> acc
    | (direction, aligns_count, (align_other, _)) :: tl ->
      let acc =
        if aligns_count > 0
        then
          let is_vertical = match direction with
            | Vleft | Vright | Vcenter -> true
            | Nill | Htop | Hbottom | Hcenter -> false in
          (*let is_center = match direction with
            | Vcenter | Hcenter -> true
            | _ -> false in*)
          let is_center = match align_other with
            | Vcenter | Hcenter -> true
            | _ -> false in
          let origin = match direction with
            | Vleft -> pos.x
            | Vcenter -> pos.x + pos.w / 2
            | Vright -> pos.x + pos.w
            | Htop -> pos.y
            | Hcenter -> pos.y + pos.h / 2
            | Hbottom -> pos.y + pos.h
            | Nill -> 0 in
          let line_ret =
            { is_vertical
            ; is_multiple = aligns_count > 1
            ; is_center
            ; origin
            } in
          line_ret :: acc
        else acc in
      create_lines action acc tl in
  create_lines action [] snap_list

let position_clipped_parent
    ({ w; h; x; y } as pos : t)
    (parent_w, parent_h : int * int)
    (min_w : int)
    (min_h : int) = function
  | `Move -> fix_xy parent_w (Some parent_h) pos
  | `Resize direction ->
    let (max_x, max_y, min_x, min_y) =
      match direction with
      | Top_left -> Some (x + w - min_w), Some (y + h - min_h), None, None
      | Top_right -> None, Some (y + h - min_h), Some x, None
      | Bottom_left -> Some (x + w - min_w), None, None, Some y
      | Bottom_right -> None, None, Some x, Some y
      (* not tested *)
      | Top -> Some (x + w - min_w), None, None, None
      (* not tested *)
      | Bottom -> None, None, None, Some y
      (* not tested *)
      | Left -> Some (x + w - min_h), None, None, None
      (* not tested *)
      | Right -> None, None, Some x, None
    in
    fix_xy ?min_x ?max_x ?min_y ?max_y
      parent_w (Some parent_h)
      (fix_wh ~min_w ~min_h parent_w (Some parent_h) pos)

(* alternative position_not_collide_others_glide *)
let fix_collisions
    (original_position : t)
    (position : t)
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list) =
  let rec positions_at_items acc = function
    | [] -> acc
    | hd :: tl ->
      let acc =
        if Element.equal item hd
        then acc
        else (of_element hd) :: acc in
      positions_at_items acc tl in
  if has_collision position (positions_at_items [] items)
  then original_position
  else position

(* return position of item, closest and not to other
        (checks all others) at move direction (top bottom left right)
         or resize direction
       find_position init value = original_position
       recursion ends, if max_counter <= 0
       set max_counter as max width height parent
       _
       resize_direction used in this function NOT as resize_direction *)
let closest_non_intersecting
    (vector : resize_direction)
    (action : [`Resize of resize_direction | `Move])
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list)
    (max_counter : int)
    (position : t) =
  let rec aux acc = function
    | n when n <= 0 -> position (* FIXME maybe acc?! *)
    | n ->
      let pos = match action with
        | `Move -> (* move *)
          let x = match vector with
            | Left -> acc.x - 1
            | Right -> acc.x + 1
            | Top | Bottom | Top_left | Top_right
            | Bottom_left | Bottom_right -> acc.x in
          let y = match vector with
            | Top -> acc.y - 1
            | Bottom -> acc.y + 1
            | Left | Right | Top_left | Top_right
            | Bottom_left | Bottom_right -> acc.y in
          { acc with x; y }
        | `Resize resz ->
          (* direction to find closest position item witch other *)
          let x = match vector with
            | Left ->
              begin match resz with
                | Top_left -> acc.x - 1
                | Top_right -> acc.x
                | Bottom_left -> acc.x - 1
                | Bottom_right -> acc.x
                | Top | Bottom | Left | Right -> acc.x (* not tested *)
              end
            | _ -> acc.x in
          let y = match vector with
            | Top ->
              begin match resz with
                | Top_left -> acc.y - 1
                | Top_right -> acc.y - 1
                | Bottom_left -> acc.y
                | Bottom_right -> acc.y
                | Top | Bottom | Left | Right -> acc.y
              end (* not tested *)
            | _ -> acc.y in
          let w = match vector with
            | Right ->
              begin match resz with
                | Top_left -> acc.w
                | Top_right -> acc.w + 1
                | Bottom_left -> acc.w
                | Bottom_right -> acc.w + 1
                | Top | Bottom | Left | Right -> acc.w
              end (* not tested *)
            | _ -> acc.w in
          let h = match vector with
            | Bottom ->
              begin match resz with
                | Top_left -> acc.h
                | Top_right -> acc.h
                | Bottom_left -> acc.h + 1
                | Bottom_right -> acc.h + 1
                | Top | Bottom | Left | Right -> acc.h
              end (* not tested *)
            | _ -> acc.h in
          { x; y; w; h } in
      if equal (fix_collisions position pos item items) position
      then acc else aux pos (pred n) in
  aux position max_counter

let fix_collisions_glide
    ~(origin : t)
    ({ x; y; w; h } as pos : t)
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list)
    (action : [`Resize of resize_direction | `Move])
    (parent_size : int * int) =
  if equal pos origin then origin
  else
    (* for four directions (to top, to bottom, to right, to left)
       find closest not clip position at item to other.
       It used for remove wrong addition distance at item to other
       if mouse big jump in other object presents -
       remove big space between item and other *)
    let closest =
      match action with
      | `Move ->
        if abs (x - origin.x) > abs (y - origin.y)
        then
          if x - origin.x < 0
          then closest_non_intersecting Left
              action item items
              (max (fst parent_size) (snd parent_size))
              origin
          else closest_non_intersecting Right
              action item items
              (max (fst parent_size) (snd parent_size))
              origin
        else
        if y - origin.y < 0
        then closest_non_intersecting Top
            action item items
            (max (fst parent_size) (snd parent_size))
            origin
        else closest_non_intersecting Bottom
            action item items
            (max (fst parent_size) (snd parent_size))
            origin
      | `Resize resz ->
        if match resz with
          | Top_left -> abs (x - origin.x) > abs (y - origin.y)
          | Top_right -> abs (w - origin.w) > abs (h - origin.h)
          | Bottom_left -> abs (x - origin.x) > abs (y - origin.y)
          | Bottom_right -> abs (w - origin.w) > abs (h - origin.h)
          | Top | Bottom | Left | Right -> true (* not tested *)
        then if match resz with
          | Top_left -> x - origin.x < 0
          | Top_right -> w - origin.w < 0
          | Bottom_left  -> x - origin.x < 0
          | Bottom_right -> w - origin.w < 0
          | Top | Bottom | Left | Right -> true (* not tested *)
          then
            closest_non_intersecting Left
              action item items
              (max (fst parent_size) (snd parent_size))
              origin
          else
            closest_non_intersecting Right
              action item items
              (max (fst parent_size) (snd parent_size))
              origin
        else if match resz with
          | Top_left -> y - origin.y < 0
          | Top_right -> y - origin.y < 0
          | Bottom_left  -> h - origin.h < 0
          | Bottom_right -> h - origin.h < 0
          | Top | Bottom | Left | Right -> true (* not tested *)
        then
          closest_non_intersecting Top
            action item items
            (max (fst parent_size) (snd parent_size))
            origin
        else
          closest_non_intersecting Bottom
            action item items
            (max (fst parent_size) (snd parent_size))
            origin
    in
    let pos_y = match action with
      | `Move -> { pos with x = closest.x; w = closest.w }
      | `Resize sz -> match sz with
        | Top_left | Bottom_left -> { pos with x = closest.x; w = w + x - closest.x }
        | Top_right | Bottom_right -> { pos with x = closest.x; w = closest.w }
        | Top | Bottom | Left | Right -> origin (* not tested *)
    in
    let pos_x = match action with
      | `Move -> { pos with y = closest.y; h = closest.h }
      | `Resize sz -> match sz with
        | Top_left | Top_right -> { pos with y = closest.y; h = h + y - closest.y }
        | Bottom_left | Bottom_right -> { pos with y = closest.y; h = closest.h }
        | Top | Bottom | Left | Right -> origin (* not tested *)
    in
    let pos = fix_collisions origin pos item items in
    if equal pos origin
    then
      let pos = fix_collisions origin pos_y item items in
      if equal pos origin
      then fix_collisions origin pos_x item items
      else pos
    else pos

let snap_to_grid position grid_step =
  { x = position.x - position.x mod grid_step
  ; y = position.y - position.y mod grid_step
  ; w = position.w - position.w mod grid_step
  ; h = position.h - position.h mod grid_step
  }

(* FIXME remove*)
let global_saved_original_position = ref {x=0;y=0;h=0;w=0}
let global_saved_position_previous = ref {x=0;y=0;h=0;w=0}

let adjust ?aspect_ratio
    ?(snap_lines = true)
    ?(collisions = false)
    ?(min_width = 20) (* XXX Do we need the default value at all? *)
    ?(min_height = 20)
    (*?(grid_step = 15)*)
    ?max_width
    ?max_height
    ~(action : [`Resize of resize_direction | `Move])
    ~(original_position : t)
    ~(position : t)
    ~(siblings : Dom_html.element Js.t list)
    ~(parent_size : int * int)
    (item : Dom_html.element Js.t) : t * line list =
  let min_distance = 12 in
  (* FIXME values to function declaration? *)
  let grid_step = 15 in
  let grid_enable = false in
  (* ok
     FIXME replace with `fix` functions (keep DRY) *)
  let position_to_grid =
    if grid_enable
    then snap_to_grid position grid_step
    else position
  in
  let position_snaped = match snap_lines, action with
    | false, _ -> position_to_grid
    | true, `Move ->
      get_item_snap_position_for_move_action item position_to_grid
        min_distance siblings
    | true, `Resize resz ->
      get_item_snap_position_for_resize_action item position_to_grid
        min_distance siblings resz
  in
  let position_clipped_parent =
    position_clipped_parent position_snaped parent_size min_width min_height action
  in
  (* FIXME remove *)
  let last_pos =
    if !global_saved_original_position <> original_position
    then begin
      global_saved_original_position:=original_position;
      global_saved_position_previous:=original_position;
      original_position
    end
    else !global_saved_position_previous in
  let position_not_collide_others =
    if collisions
    then
      fix_collisions_glide
        ~origin:last_pos
        position_clipped_parent
        item siblings action parent_size
    else position_clipped_parent in (* pos snapped*)
  let snap_lines =
    if snap_lines
    then get_snap_lines item position_not_collide_others siblings min_distance action
    else [] in
  (* FIXME remove *)
  global_saved_position_previous := position_not_collide_others;
  position_not_collide_others, snap_lines

let of_wm_position ?aspect ~parent pos =
  empty

let to_wm_position ?aspect ~parent t =
  Pipeline_types.Wm.{ top = 0; left = 0; right = 0; bottom = 0 }
