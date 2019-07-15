open Js_of_ocaml
open Components

let ( % ) f g x = f (g x)

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : float
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
  { x : float
  ; y : float
  ; w : float
  ; h : float
  }

let empty =
  { x = 0.0
  ; y = 0.0
  ; w = 0.0
  ; h = 0.0
  }

let fmod v1 v2 =
  if v2 = 0.0
  then 0.0
  else let val1 = floor (v1 /. v2) in
    let ost = v1 -. val1 in
    ost

let fabs (v1 : float) = if v1 >= 0.0
  then v1
  else (-. v1)

let show { x; y; w; h } =
  Printf.sprintf "x=%g, y=%g, w=%g, h=%g" x y w h

let compare (a : t) (b : t) =
  let c = compare a.x b.x in
  if c <> 0 then c
  else (let c = compare a.y b.y in
        if c <> 0 then c
        else (let c = compare a.w b.w in
              if c <> 0 then c
              else compare a.h b.h))

let equal (a : t) (b : t) =
  fabs( a.x -. b.x ) < 0.00001 &&
  fabs( a.y -. b.y ) < 0.00001 &&
  fabs( a.w -. b.w ) < 0.00001 &&
  fabs( a.h -. b.h ) < 0.00001

(** Checks if two elements collide, returns [true] if so and [false] otherwise *)
let collides (pos1 : t) (pos2 : t) =
  if (pos1.x +. pos1.w <= pos2.x) then false
  else if (pos1.x >= pos2.x +. pos2.w) then false
  else if (pos1.y +. pos1.h <= pos2.y) then false
  else if (pos1.y >= pos2.y +. pos2.h) then false
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
    else w in
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
  { p with h }

(** Changes width and height to correspond provided constraints *)
let fix_wh ?max_w ?min_w ?max_h ?min_h ?parent_w ?parent_h =
  (fix_h ?max_h ?min_h ?parent_h) % (fix_w ?max_w ?min_w ?parent_w)

let fix ?min_x ?min_y ?max_x ?max_y
    ?max_w ?min_w ?max_h ?min_h ?parent_w ?parent_h =
  fix_xy ?min_x ?min_y ?max_x ?max_y ?parent_w ?parent_h
  % fix_wh ?max_w ?min_w ?max_h ?min_h ?parent_w ?parent_h

let apply_to_element (pos : t) (elt : #Dom_html.element Js.t) =
  elt##.style##.width := Utils.px_js (int_of_float pos.w);
  elt##.style##.left := Utils.px_js (int_of_float pos.x);
  elt##.style##.height := Utils.px_js (int_of_float pos.h);
  elt##.style##.top := Utils.px_js (int_of_float pos.y)

let of_element (elt : #Dom_html.element Js.t) =
  { x = float_of_int elt##.offsetLeft
  ; y = float_of_int elt##.offsetTop
  ; w = float_of_int elt##.offsetWidth
  ; h = float_of_int elt##.offsetHeight
  }

let to_client_rect (p : t) : Dom_html.clientRect Js.t =
  object%js
    val top = p.y
    val left = p.x
    val right = p.x +. p.w
    val bottom = p.y +. p.h
    val width = Js.def p.w
    val height = Js.def p.h
  end

let of_client_rect (r : Dom_html.clientRect Js.t) : t =
  { x = r##.left
  ; y = r##.top
  ; w = Js.Optdef.get r##.width (fun () -> r##.right -. r##.left)
  ; h = Js.Optdef.get r##.height (fun () -> r##.bottom -. r##.top)
  }

let default_aspect_ratio = 1.

let string_of_float = Printf.sprintf "%g"

let get_int_attribute (elt : #Dom_html.element Js.t) attr : int =
  match Element.get_attribute elt attr with
  | None -> 0
  | Some x ->
    match int_of_string_opt x with
    | None -> 0
    | Some x -> x

(* min_distance - pixels
   return: (other element align as line_align_direction *
            minimum distance of several lines of one align as int) 
*)
let line_find_closest_align
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    min_distance
    line_align_val =
  let rec count_aligns line_align_val (distance: (line_align_direction * float)) = function
    | [] -> distance
    | hd :: tl ->
      let icompare = 
        { x = (of_element (* ~parent_f:(parent_w_f, parent_h_f) *) hd ).x
        ; y = (of_element (* ~parent_f:(parent_w_f, parent_h_f) *) hd ).y
        ; w = (of_element (* ~parent_f:(parent_w_f, parent_h_f) *) hd ).w
        ; h = (of_element (* ~parent_f:(parent_w_f, parent_h_f) *) hd ).h
        }
      in
      let (distance : line_align_direction * float) =
        if Element.equal item hd
        then count_aligns line_align_val distance tl (*distance*) else
          match line_align_val with
          | Htop ->
            let dist1 = pos.y -. icompare.y in
            let dist2 = pos.y -. icompare.y -. icompare.h /. 2.0 in
            let dist3 = pos.y -. icompare.y -. icompare.h in
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
            let dist1 = pos.y +. pos.h /. 2.0 -. icompare.y in
            let dist2 = pos.y +. pos.h /. 2.0 -. icompare.y -. icompare.h /. 2.0 in
            let dist3 = pos.y +. pos.h /. 2.0 -. icompare.y -. icompare.h in
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
            let dist1 = pos.y +. pos.h -. icompare.y in
            let dist2 = pos.y +. pos.h -. icompare.y -. icompare.h /. 2.0 in
            let dist3 = pos.y +. pos.h -. icompare.y -. icompare.h in
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
            let dist1 = pos.x -. icompare.x in
            let dist2 = pos.x -. icompare.x -. icompare.w /. 2.0 in
            let dist3 = pos.x -. icompare.x -. icompare.w in
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
            let dist1 = pos.x +. pos.w /. 2.0 -. icompare.x in
            let dist2 = pos.x +. pos.w /. 2.0 -. icompare.x -. icompare.w /. 2.0 in
            let dist3 = pos.x +. pos.w /. 2.0 -. icompare.x -. icompare.w in
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
            let dist1 = pos.x +. pos.w -. icompare.x in
            let dist2 = pos.x +. pos.w -. icompare.x -. icompare.w /. 2.0 in
            let dist3 = pos.x +. pos.w -. icompare.x -. icompare.w in
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
  count_aligns line_align_val (Nill, (min_distance +. 1.0)) items  (* FIX + 1.0*)

(* min_distance - pixels
   return: counts of align of selected type in min_distance interval *)
let line_align_count
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    (min_distance : float)
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
              && (fabs (pos.y -. icompare.y) < min_distance
                  || fabs (pos.y -. icompare.y -. icompare.h /. 2.0) < min_distance
                  || fabs (pos.y -. icompare.y -. icompare.h) < min_distance))
          || (line_align_val = Hcenter
              && (fabs (pos.y +. pos.h /. 2.0 -. icompare.y) < min_distance
                  || fabs (pos.y +. pos.h /. 2.0 -. icompare.y -. icompare.h /. 2.0) < min_distance
                  || fabs (pos.y +. pos.h /. 2.0 -. icompare.y -. icompare.h) < min_distance))
          || (line_align_val = Hbottom
              && (fabs (pos.y +. pos.h -. icompare.y) < min_distance
                  || fabs (pos.y +. pos.h -. icompare.y -. icompare.h /. 2.0) < min_distance
                  || fabs (pos.y +. pos.h -. icompare.y -. icompare.h) < min_distance))
          || (line_align_val = Vleft
              && (fabs (pos.x -. icompare.x) < min_distance
                  || fabs (pos.x -. icompare.x -. icompare.w /. 2.0) < min_distance
                  || fabs (pos.x -. icompare.x -. icompare.w) < min_distance))
          || (line_align_val = Vcenter
              && (fabs (pos.x +. pos.w /. 2.0 -. icompare.x) < min_distance
                  || fabs (pos.x +. pos.w /. 2.0 -. icompare.x -. icompare.w /. 2.0) < min_distance
                  || fabs (pos.x +. pos.w /. 2.0 -. icompare.x -. icompare.w) < min_distance))
          || (line_align_val = Vright
              && (fabs (pos.x +. pos.w -. icompare.x) < min_distance
                  || fabs (pos.x +. pos.w -. icompare.x -. icompare.w /. 2.0) < min_distance
                  || fabs (pos.x +. pos.w -. icompare.x -. icompare.w) < min_distance))
          then succ counts
          else counts
        in
        aux line_align_val counts tl
  in
  aux line_align_val 0 items

let make_line_properties align item pos min_distance items (parent_w_f, parent_h_f : float * float) =
  align,
  line_align_count item pos items (parent_w_f, parent_h_f) min_distance align,
  line_find_closest_align item pos items (parent_w_f, parent_h_f) min_distance align

(* return: direction, count aligns (0 = none align lines),
   closest line distance (if distance > min_distance = no find lines) *)
let hlines_for_move_action (item : Dom_html.element Js.t)
    pos 
    min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  [ make_line_properties Htop item pos min_distance items (parent_w_f, parent_h_f)
  ; make_line_properties Hcenter item pos min_distance items (parent_w_f, parent_h_f)
  ; make_line_properties Hbottom item pos min_distance items (parent_w_f, parent_h_f)
  ]

let hlines_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    (direction : resize_direction) =
  let align_direction = match direction with
    | Top_left | Top_right | Top -> Htop
    | Bottom_left | Bottom_right | Bottom -> Hbottom
    | Left | Right -> Hcenter in
  [make_line_properties align_direction item pos min_distance items (parent_w_f, parent_h_f)]

let vlines_for_move_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  [ make_line_properties Vleft item pos min_distance items (parent_w_f, parent_h_f)
  ; make_line_properties Vcenter item pos min_distance items (parent_w_f, parent_h_f)
  ; make_line_properties Vright item pos min_distance items (parent_w_f, parent_h_f)
  ]

let vlines_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    (direction : resize_direction) =
  let align_direction = match direction with
    | Top_left | Bottom_left | Left -> Vleft
    | Top_right | Bottom_right | Right -> Vright
    | Top | Bottom -> Vcenter in
  [make_line_properties align_direction item pos min_distance items (parent_w_f, parent_h_f)]

let get_snap 
    (item : Dom_html.element Js.t) 
    (coord: float)
    (min_distance:float) 
    (items : (line_align_direction * int * (line_align_direction * float)) list) =
  let rec aux (snap:float) (snap_min_delta:float) 
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

let get_item_snap_y (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  get_snap item pos.y min_distance
  @@ hlines_for_move_action item pos min_distance items (parent_w_f, parent_h_f)

let get_item_snap_x (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  get_snap item pos.x min_distance
  @@ vlines_for_move_action item pos min_distance items (parent_w_f, parent_h_f)

let get_item_snap_position_for_move_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  { x = get_item_snap_x item pos min_distance items (parent_w_f, parent_h_f)
  ; y = get_item_snap_y item pos min_distance items (parent_w_f, parent_h_f)
  ; w = pos.w
  ; h = pos.h
  }

let get_item_snap_position_for_resize_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    (direction : resize_direction) =
  let make_line align = make_line_properties align item pos min_distance items (parent_w_f, parent_h_f) in
  match direction with
  | Top_left ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Htop] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Top_right ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Htop] in
    { x = pos.x (*get_snap item pos.x min_distance snap_list_x *)
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = (get_snap item pos.x min_distance snap_list_x) -. pos.x +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Bottom_left ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hbottom] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = pos.y (*get_snap item pos.y min_distance snap_list_y *)
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = (get_snap item pos.y min_distance snap_list_y) -. pos.y +. pos.h
    }
  | Bottom_right ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hbottom] in
    { x = pos.x (*get_snap item pos.x min_distance snap_list_x *)
    ; y = pos.y (*get_snap item pos.y min_distance snap_list_y *)
    ; w = (get_snap item pos.x min_distance snap_list_x) -. pos.x +. pos.w
    ; h = (get_snap item pos.y min_distance snap_list_y) -. pos.y +. pos.h
    }
  | Top ->
    (* not tested *)
    let snap_list_x = [make_line Vcenter] in
    let snap_list_y = [make_line Htop] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Bottom ->
    (* not tested *)
    let snap_list_x = [make_line Vcenter] in
    let snap_list_y = [make_line Hbottom] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Left ->
    (* not tested *)
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hcenter] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Right ->
    (* not tested *)
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hcenter] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }

(* glue lines to its item *)
let get_snap_lines
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    min_distance
    (action : [`Resize of resize_direction | `Move]) =
  let snap_list =
    match action with
    | `Move ->
      vlines_for_move_action item pos min_distance items (parent_w_f, parent_h_f)
      @ hlines_for_move_action item pos min_distance items (parent_w_f, parent_h_f)
    | `Resize dir ->
      vlines_for_resize_action item pos min_distance items (parent_w_f, parent_h_f) dir
      @ hlines_for_resize_action item pos min_distance items (parent_w_f, parent_h_f) dir
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

let position_clipped_parent
    ({ w; h; x; y } as pos : t)
    ?parent_w
    ?parent_h
    (min_w : float)
    (min_h : float) = function
  | `Move -> fix_xy ?parent_w ?parent_h pos
  | `Resize direction ->
    let (max_x, max_y, min_x, min_y) =
      match direction with
      | Top_left -> Some (x +. w -. min_w), Some (y +. h -. min_h), None, None
      | Top_right -> None, Some (y +. h -. min_h), Some x, None
      | Bottom_left -> Some (x +. w -. min_w), None, None, Some y
      | Bottom_right -> None, None, Some x, Some y
      (* not tested *)
      | Top -> Some (x +. w -. min_w), None, None, None
      (* not tested *)
      | Bottom -> None, None, None, Some y
      (* not tested *)
      | Left -> Some (x +. w -. min_h), None, None, None
      (* not tested *)
      | Right -> None, None, Some x, None
    in
    fix ?min_x ?max_x ?min_y ?max_y ~min_w ~min_h ?parent_w ?parent_h pos

(* alternative position_not_collide_others_glide *)
let fix_collisions
    (original_position : t)
    (position : t)
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
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

let snap_to_grid position (grid_step : float) =
  { x = position.x -. (fmod position.x grid_step)
  ; y = position.y -. (fmod position.y grid_step)
  ; w = position.w -. (fmod position.w grid_step)
  ; h = position.h -. (fmod position.h grid_step)
  }

let adjust ?aspect_ratio
    ?(snap_lines = true)
    ?(collisions = false) (* need if we used collides *)
    ?(min_width = 20) (* XXX Do we need the default value at all? *) (* to float [0;1.0] *)
    ?(min_height = 20) (* to float [0;1.0] *)
    (*?(grid_step = 15)*) (* is needed if we used grid *)
    ?max_width (* not need *)
    ?max_height (* not need *)
    ~(action : [`Resize of resize_direction | `Move])
    ~(original_position : t) (* need if we use collides *) (* int coordinatrs to float [0;1.0] *)
    ~(position : t) (* int coordinatrs to float [0;1.0] *)
    ~(siblings : Dom_html.element Js.t list) (* widget positions int coordinatrs to float [0;1.0] *)
    ~(parent_size : float * float) (* need if input positions is int pixel coordinates *)
    (item : Dom_html.element Js.t) : t * line list
    (*
    add:
    (input_container_aspect : float)    = width/height in float
    (input_table_cell_aspect : float)   = width/height in float
    (align : align)  - container align, if we use align container in cell
    *)
  =
  let min_distance = 12 in
  (* FIXME values to function declaration? *)
  let grid_step = 15 in
  let parent_w =
    if (fst parent_size) > 0.
    then (fst parent_size)
    else 100.0
  in
  let parent_h =
    if (snd parent_size) > 0.
    then (snd parent_size)
    else 100.0
  in
  let max_of_parent_size =  max parent_w parent_h in
  let min_distance = (float_of_int min_distance) /. max_of_parent_size in
  let grid_step = (float_of_int grid_step) /. max_of_parent_size in
  let min_width = (float_of_int min_width) /. max_of_parent_size in
  let min_height = (float_of_int min_height) /. max_of_parent_size in
  let grid_enable = false in
  let position_to_grid =
    if grid_enable
    then snap_to_grid position grid_step
    else position
  in
  let position_snaped = match snap_lines, action with
    | false, _ -> position_to_grid
    | true, `Move ->
      get_item_snap_position_for_move_action item position_to_grid
        min_distance siblings (parent_w, parent_h)
    | true, `Resize resz ->
      get_item_snap_position_for_resize_action item position_to_grid
        min_distance siblings (parent_w, parent_h) resz
  in
  let position_clipped_parent =
    position_clipped_parent position_snaped
      ~parent_w
      ~parent_h
      min_width
      min_height
      action
  in
  let position_not_collide_others = position_clipped_parent in
  let snap_lines =
    if snap_lines
    then get_snap_lines item position_not_collide_others
        siblings
        (parent_w, parent_h)
        min_distance
        action
    else [] in
  { x = position_not_collide_others.x
  ; y = position_not_collide_others.y
  ; w = position_not_collide_others.w
  ; h = position_not_collide_others.h
  }, snap_lines

let to_wm_position (t : t) : Pipeline_types.Wm.position =
  { left = int_of_float t.x
  ; top = int_of_float t.y
  ; right = int_of_float @@ t.x +. t.w
  ; bottom = int_of_float @@ t.y +. t.h
  }

let of_wm_position (t : Pipeline_types.Wm.position) : t =
  { x = float_of_int t.left
  ; y = float_of_int t.top
  ; w = float_of_int (t.right - t.left)
  ; h = float_of_int (t.bottom - t.top)
  }

let to_relative ~(parent_size : float * float) (pos : t) =
  let parent_width, parent_height = parent_size in
  let w, h =
    if parent_width > pos.w
    then (pos.w /. parent_width,
          pos.h /. parent_height)
    else (parent_width /. pos.w,
          parent_height /. pos.h) in
  let x = (pos.x *. w) /. pos.w in
  let y = (pos.y *. h) /. pos.h in
  { x; y; w; h }

let of_relative ~(parent_size : float * float) (pos : t) =
  let w = pos.w *. (fst parent_size) in
  let h = pos.h *. (snd parent_size) in
  let x = pos.x *. w /. pos.w in
  let y = pos.y *. h /. pos.h in
  { x; y; w; h }
