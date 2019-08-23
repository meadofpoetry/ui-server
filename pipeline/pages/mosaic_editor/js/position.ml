open Js_of_ocaml

include Position_intf

let ( % ) f g x = f (g x)

type absolute =
  { x : float
  ; y : float
  ; w : float
  ; h : float
  }

module Make(Pos : S) : Position with type t = Pos.t = struct

  include Pos

  let equal (a : t) (b : t) =
    let (=) a b = abs_float (a -. b) < epsilon_float in
    Pos.top a = Pos.top b
    && Pos.left a = Pos.left b
    && Pos.height a = Pos.height b
    && Pos.width a = Pos.width b

  let show (x : t)=
    Printf.sprintf "left=%g, top=%g, width=%g, height=%g"
      (left x) (top x) (width x) (height x)

  let right (t : t) =
    Pos.left t +. Pos.width t

  let bottom (t : t) =
    Pos.top t +. Pos.height t

  let empty : t = Pos.make ~left:0. ~top:0. ~width:0. ~height:0.

  (** Checks if two elements collide, returns [true] if so and [false] otherwise *)
  let collides (a : t) (b : t) =
    (right a) > (Pos.left b)
    && (Pos.left a) <= (right b)
    && (bottom a) >= (Pos.top b)
    && (Pos.top a) <= (bottom b)

  (** Changes width to correspond provided constraints *)
  let validate_width ?max_width ?(min_width = 0.) ?parent_width (p : t) =
    let w = Pos.width p in
    let w = match max_width with
      | Some max ->
        if w > max then max
        else if w < min_width then min_width
        else w
      | None -> if w < min_width then min_width else w
    in
    let left = Pos.left p in
    let w = match parent_width with
      | None -> w
      | Some parent_width ->
        if left +. w > parent_width
        then parent_width -. left
        else if left < 0.0
        then w +. left
        else w
    in
    let w = if w < min_width then min_width else w in
    Pos.set_width p w

  (** Changes height to correspond provided constraints *)
  let validate_height ?max_height ?(min_height = 0.) ?parent_height (p : t) =
    let h = Pos.height p in
    let h = match max_height with
      | Some max ->
        if h > max then max
        else if h < min_height then min_height
        else h
      | None -> if h < min_height then min_height else h
    in
    let top = Pos.top p in
    let h = match parent_height with
      | None -> h
      | Some parent_height ->
        if top +. h > parent_height
        then parent_height -. top
        else if top < 0.0
        then h +. top
        else h
    in
    let h =
      if h < min_height
      then min_height
      else h
    in
    Pos.set_height p h

  (** Changes width and height to correspond provided constraints *)
  let validate_size ?max_width ?min_width ?max_height ?min_height
      ?parent_width ?parent_height =
    validate_height ?max_height ?min_height ?parent_height
    % validate_width ?max_width ?min_width ?parent_width

  (** Changes top and left coordinates to correspond parent dimentions *)
  let validate_left_top ?min_left ?min_top ?max_left ?max_top
      ?(parent_width = 1.) ?(parent_height = 1.) (p : t) =
    let x, y, w, h = Pos.left p, Pos.top p, Pos.width p, Pos.height p in
    let x =
      if x < 0. then 0.
      else if x +. w > parent_width then parent_width -. w
      else x in
    let y =
      if y < 0. then 0.
      else if y +. h > parent_height then parent_height -. h
      else y
    in
    let x = match max_left with
      | Some max -> if x > max then max else x
      | None -> x
    in
    let x = match min_left with
      | Some min -> if x < min then min else x
      | None -> x
    in
    let y = match max_top with
      | Some max -> if y > max then max else y
      | None -> y
    in
    let y = match min_top with
      | Some min -> if y < min then min else y
      | None -> y
    in
    Pos.set_top (Pos.set_left p x) y

  let validate ?min_left ?min_top ?max_left ?max_top
      ?max_width ?min_width ?max_height ?min_height
      ?parent_width ?parent_height =
    validate_left_top ?min_left ?min_top ?max_left ?max_top
      ?parent_width ?parent_height
    % validate_size ?max_width ?min_width ?max_height ?min_height
      ?parent_width ?parent_height

  let bounding_rect : t list -> t = function
    | [] -> empty
    | [x] -> x
    | hd :: tl ->
      let acc = Pos.left hd, Pos.top hd, right hd, bottom hd in
      let (left, top, right, bottom) =
        List.fold_left (fun (x, y, r, b) (pos : t) ->
            let pos_right = right pos in
            let pos_bottom = bottom pos in
            min x (Pos.left pos),
            min y (Pos.top pos),
            max r pos_right,
            max b pos_bottom)
          acc tl in
      Pos.make ~left ~top ~width:(right -. left) ~height:(bottom -. top)

end

module Abs = struct
  type t = absolute

  let width t = t.w
  let height t = t.h
  let top t = t.y
  let left t = t.x

  let set_width (t : t) w = { t with w }
  let set_height (t : t) h = { t with h }
  let set_top (t : t) y = { t with y }
  let set_left (t : t) x = { t with x }

  let make ~left ~top ~width ~height =
    { x = left
    ; y = top
    ; w = width
    ; h = height
    }
end

module Norm = struct
  type t = Pipeline_types.Wm.position

  let width (t : t) = t.w
  let height (t : t) = t.h
  let top (t : t) = t.y
  let left (t : t) = t.x

  let set_width (t : t) w = { t with w }
  let set_height (t : t) h = { t with h }
  let set_top (t : t) y = { t with y }
  let set_left (t : t) x = { t with x }

  let make ~left ~top ~width ~height : t =
    { x = left
    ; y = top
    ; w = width
    ; h = height
    }
end

module Normalized = struct
  include Make(Norm)

  let validate (t : t) : t =
    let x = max 0. t.x in
    let y = max 0. t.y in
    let w = if x +. t.w > 1. then 1. -. x else t.w in
    let h = if y +. t.h > 1. then 1. -. y else t.h in
    { x; y; w; h }

  let compare (a : t) (b : t) =
    let c = compare a.x b.x in
    if c <> 0 then c
    else (let c = compare a.y b.y in
          if c <> 0 then c
          else (let c = compare a.w b.w in
                if c <> 0 then c
                else compare a.h b.h))

  (* FIXME this function relies on percentage styles of an element,
     which is not always satisfied *)
  let of_element (elt : #Dom_html.element Js.t) : t =
    { x = (Js.parseFloat elt##.style##.left) /. 100.
    ; y = (Js.parseFloat elt##.style##.top) /. 100.
    ; w = (Js.parseFloat elt##.style##.width) /. 100.
    ; h = (Js.parseFloat elt##.style##.height) /. 100.
    }

  let apply_to_element (pos : t) (elt : #Dom_html.element Js.t) =
    let fn = Printf.sprintf "%g%%" in
    let (pos : t) =
      { x = pos.x *. 100.
      ; y = pos.y *. 100.
      ; w = pos.w *. 100.
      ; h = pos.h *. 100.
      } in
    elt##.style##.width := Js.string @@ fn pos.w;
    elt##.style##.left := Js.string @@ fn pos.x;
    elt##.style##.height := Js.string @@ fn pos.h;
    elt##.style##.top := Js.string @@ fn pos.y

end

module Absolute = struct
  include Make(Abs)

  open Components_lab_tyxml.Transform

  let apply_to_element (pos : t) (elt : Dom_html.element Js.t) =
    let fn = Printf.sprintf "%gpx" in
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

  (* min_distance - pixels
     return: (other element align as line_align_direction *
              minimum distance of several lines of one align as int)
  *)
  let line_find_closest_align
      (pos : t)
      (siblings : t list)
      (min_distance : float)
      (line_align : Snap_line.direction) =
    let rec count_aligns line_align_val distance = function
      | [] -> distance
      | (hd : t) :: tl ->
        let distance = match line_align with
          | Htop ->
            let dist1 = pos.y -. hd.y in
            let dist2 = pos.y -. hd.y -. hd.h /. 2.0 in
            let dist3 = pos.y -. hd.y -. hd.h in
            if (abs_float dist1 < min_distance)
            && (abs_float (snd distance) > abs_float dist1)
            && (abs_float dist1 <= abs_float dist2)
            && (abs_float dist1 <= abs_float dist3)
            then Snap_line.Htop, dist1
            else if (abs_float dist2 < min_distance)
                 && (abs_float (snd distance) > abs_float dist2)
                 && (abs_float dist2 <= abs_float dist1)
                 && (abs_float dist2 <= abs_float dist3)
            then Hcenter, dist2
            else if (abs_float dist3 < min_distance)
                 && (abs_float (snd distance) > abs_float dist3)
                 && (abs_float dist3 <= abs_float dist1)
                 && (abs_float dist3 <= abs_float dist2)
            then Hbottom, dist3
            else distance
          | Hcenter ->
            let dist1 = pos.y +. pos.h /. 2.0 -. hd.y in
            let dist2 = pos.y +. pos.h /. 2.0 -. hd.y -. hd.h /. 2.0 in
            let dist3 = pos.y +. pos.h /. 2.0 -. hd.y -. hd.h in
            if (abs_float dist1 < min_distance)
            && (abs_float (snd distance) > abs_float dist1)
            && (abs_float dist1 <= abs_float dist2)
            && (abs_float dist1 <= abs_float dist3)
            then Htop, dist1
            else if (abs_float dist2 < min_distance)
                 && (abs_float (snd distance) > abs_float dist2)
                 && (abs_float dist2 <= abs_float dist1)
                 && (abs_float dist2 <= abs_float dist3)
            then Hcenter, dist2
            else if (abs_float dist3 < min_distance)
                 && (abs_float (snd distance) > abs_float dist3)
                 && (abs_float dist3 <= abs_float dist1)
                 && (abs_float dist3 <= abs_float dist2)
            then Hbottom, dist3
            else distance
          | Hbottom ->
            let dist1 = pos.y +. pos.h -. hd.y in
            let dist2 = pos.y +. pos.h -. hd.y -. hd.h /. 2.0 in
            let dist3 = pos.y +. pos.h -. hd.y -. hd.h in
            if (abs_float dist1 < min_distance)
            && (abs_float (snd distance) > abs_float dist1)
            && (abs_float dist1 <= abs_float dist2)
            && (abs_float dist1 <= abs_float dist3)
            then Htop, dist1
            else if (abs_float dist2 < min_distance)
                 && (abs_float (snd distance) > abs_float dist2)
                 && (abs_float dist2 <= abs_float dist1)
                 && (abs_float dist2 <= abs_float dist3)
            then Hcenter, dist2
            else if (abs_float dist3 < min_distance)
                 && (abs_float (snd distance) > abs_float dist3)
                 && (abs_float dist3 <= abs_float dist1)
                 && (abs_float dist3 <= abs_float dist2)
            then Hbottom, dist3
            else distance
          | Vleft ->
            let dist1 = pos.x -. hd.x in
            let dist2 = pos.x -. hd.x -. hd.w /. 2.0 in
            let dist3 = pos.x -. hd.x -. hd.w in
            if (abs_float dist1 < min_distance)
            && (abs_float (snd distance) > abs_float dist1)
            && (abs_float dist1 <= abs_float dist2)
            && (abs_float dist1 <= abs_float dist3)
            then Vleft, dist1
            else if (abs_float dist2 < min_distance)
                 && (abs_float (snd distance) > abs_float dist2)
                 && (abs_float dist2 <= abs_float dist1)
                 && (abs_float dist2 <= abs_float dist3)
            then Vcenter, dist2
            else if (abs_float dist3 < min_distance)
                 && (abs_float (snd distance) > abs_float dist3)
                 && (abs_float dist3 <= abs_float dist1)
                 && (abs_float dist3 <= abs_float dist2)
            then Vright, dist3
            else distance
          | Vcenter ->
            let dist1 = pos.x +. pos.w /. 2.0 -. hd.x in
            let dist2 = pos.x +. pos.w /. 2.0 -. hd.x -. hd.w /. 2.0 in
            let dist3 = pos.x +. pos.w /. 2.0 -. hd.x -. hd.w in
            if (abs_float dist1 < min_distance)
            && (abs_float (snd distance) > abs_float dist1)
            && (abs_float dist1 <= abs_float dist2)
            && (abs_float dist1 <= abs_float dist3)
            then Vleft, dist1
            else if (abs_float dist2 < min_distance)
                 && (abs_float (snd distance) > abs_float dist2)
                 && (abs_float dist2 <= abs_float dist1)
                 && (abs_float dist2 <= abs_float dist3)
            then Vcenter, dist2
            else if (abs_float dist3 < min_distance)
                 && (abs_float (snd distance) > abs_float dist3)
                 && (abs_float dist3 <= abs_float dist1)
                 && (abs_float dist3 <= abs_float dist2)
            then Vright, dist3
            else distance
          | Vright ->
            let dist1 = pos.x +. pos.w -. hd.x in
            let dist2 = pos.x +. pos.w -. hd.x -. hd.w /. 2.0 in
            let dist3 = pos.x +. pos.w -. hd.x -. hd.w in
            if (abs_float dist1 < min_distance)
            && (abs_float (snd distance) > abs_float dist1)
            && (abs_float dist1 <= abs_float dist2)
            && (abs_float dist1 <= abs_float dist3)
            then Vleft, dist1
            else if (abs_float dist2 < min_distance)
                 && (abs_float (snd distance) > abs_float dist2)
                 && (abs_float dist2 <= abs_float dist1)
                 && (abs_float dist2 <= abs_float dist3)
            then Vcenter, dist2
            else if (abs_float dist3 < min_distance)
                 && (abs_float (snd distance) > abs_float dist3)
                 && (abs_float dist3 <= abs_float dist1)
                 && (abs_float dist3 <= abs_float dist2)
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
          if (line_align_val = Snap_line.Htop
              && (abs_float (pos.y -. hd.y) < min_distance
                  || abs_float (pos.y -. hd.y -. hd.h /. 2.0) < min_distance
                  || abs_float (pos.y -. hd.y -. hd.h) < min_distance))
          || (line_align_val = Hcenter
              && (abs_float (pos.y +. pos.h /. 2.0 -. hd.y) < min_distance
                  || abs_float (pos.y +. pos.h /. 2.0 -. hd.y -. hd.h /. 2.0) < min_distance
                  || abs_float (pos.y +. pos.h /. 2.0 -. hd.y -. hd.h) < min_distance))
          || (line_align_val = Hbottom
              && (abs_float (pos.y +. pos.h -. hd.y) < min_distance
                  || abs_float (pos.y +. pos.h -. hd.y -. hd.h /. 2.0) < min_distance
                  || abs_float (pos.y +. pos.h -. hd.y -. hd.h) < min_distance))
          || (line_align_val = Vleft
              && (abs_float (pos.x -. hd.x) < min_distance
                  || abs_float (pos.x -. hd.x -. hd.w /. 2.0) < min_distance
                  || abs_float (pos.x -. hd.x -. hd.w) < min_distance))
          || (line_align_val = Vcenter
              && (abs_float (pos.x +. pos.w /. 2.0 -. hd.x) < min_distance
                  || abs_float (pos.x +. pos.w /. 2.0 -. hd.x -. hd.w /. 2.0) < min_distance
                  || abs_float (pos.x +. pos.w /. 2.0 -. hd.x -. hd.w) < min_distance))
          || (line_align_val = Vright
              && (abs_float (pos.x +. pos.w -. hd.x) < min_distance
                  || abs_float (pos.x +. pos.w -. hd.x -. hd.w /. 2.0) < min_distance
                  || abs_float (pos.x +. pos.w -. hd.x -. hd.w) < min_distance))
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

  let hlines_for_resize_action pos min_distance siblings
      (direction : direction) =
    let align_direction = match direction with
      | NW | NE | N -> Snap_line.Htop
      | SW | SE | S -> Hbottom
      | W | E -> Hcenter in
    [make_line_properties align_direction pos min_distance siblings]

  let vlines_for_move_action pos min_distance siblings =
    [ make_line_properties Vleft pos min_distance siblings
    ; make_line_properties Vcenter pos min_distance siblings
    ; make_line_properties Vright pos min_distance siblings
    ]

  let vlines_for_resize_action pos min_distance siblings
      (direction : direction) =
    let align_direction = match direction with
      | NW | SW | W -> Snap_line.Vleft
      | NE | SE | E -> Vright
      | N | S -> Vcenter in
    [make_line_properties align_direction pos min_distance siblings]

  let get_snap (coord : float) (min_distance : float) items =
    let rec aux (snap : float) (snap_min_delta : float) = function
      | [] -> snap
      | (_, aligns_count, distance__align_other) :: tl ->
        let (_ , distance) = distance__align_other in
        let snap_min_delta =
          if aligns_count > 0 && abs_float distance < abs_float snap_min_delta
          then distance else snap_min_delta in
        let snap =
          if abs_float snap_min_delta <= min_distance
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
  let get_snap_lines (pos : t) siblings (action : [`Resize of direction | `Move]) =
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
              | Snap_line.Vleft | Vright | Vcenter -> true
              | Nill | Htop | Hbottom | Hcenter -> false in
            let is_center = match align_other with
              | Snap_line.Vcenter | Hcenter -> true
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
               } : Snap_line.t) in
            line_ret :: acc
          else acc in
        create_lines action acc tl in
    create_lines action [] snap_list

  let clip_to_parent ~parent_size:(parent_width, parent_height)
      ~(min_width : float)
      ~(min_height : float)
      ({ w; h; x; y } as pos : t) = function
    | `Move ->
      validate_left_top
        ~min_left:0.
        ~min_top:0.
        ~parent_width
        ~parent_height
        pos
    | `Resize direction ->
      let (max_left, max_top, min_left, min_top) =
        match direction with
        | NW ->
          Some (x +. w -. min_width),
          Some (y +. h -. min_height),
          None, None
        | NE -> None, Some (y +. h -. min_height), Some x, None
        | SW -> Some (x +. w -. min_width), None, None, Some y
        | SE -> None, None, Some x, Some y
        | N -> Some (x +. w -. min_width), Some (y +. h -. min_height), None, None
        | E -> None, None, Some (x), None
        | W -> Some (x +. w -. min_width), None, None, Some y
        | S -> Some (x +. w -. min_width), None, None, None
      in
      validate ?min_left ?max_left ?min_top ?max_top
        ~min_width ~min_height
        ~parent_width ~parent_height
        pos

  let snap_to_grid_move (pos : t) (grid_step : float) : t =
    let x = Js.math##round (pos.x /. grid_step) *. grid_step in
    let y = Js.math##round (pos.y /. grid_step) *. grid_step in
    { pos with x; y }

  let snap_to_grid_resize (direction : direction)
      (pos : t)
      (grid_step : float) : t =
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

  let move_children (rect_position : t) (children : t list) : t list =
    let pos_left = List.hd
        (List.sort Stdlib.compare
           (List.map (fun (v : t) -> v.x) children)) in
    let pos_top = List.hd
        (List.sort Stdlib.compare
           (List.map (fun (v : t) -> v.y) children)) in
    List.map
      (fun (v : t) ->
         let x = v.x -. pos_left in
         let y = v.y -. pos_top in
         { v with x = rect_position.x +. x
                ; y = rect_position.y +. y
         })
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
      (min_height : float) : t =
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
      (max_height : float) : t =
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
      (aspect : int * int) =
    let asp = get_float_aspect aspect in
    let h = after_pos.w *. asp in
    let w = after_pos.h /. asp in
    let p1 = match dir with
      | NW ->
        if abs_float (before_pos.w -. after_pos.w)
           >= abs_float (before_pos.h -. after_pos.h)
        then { after_pos with y = orig_pos.y +. orig_pos.h -. h; h }
        else { after_pos with x = orig_pos.x +. orig_pos.w -. w; w }
      | NE ->
        if abs_float (before_pos.w -. after_pos.w)
           >= abs_float (before_pos.h -. after_pos.h)
        then { after_pos with y = orig_pos.y +. orig_pos.h -. h; h }
        else { after_pos with x = orig_pos.x; w }
      | SE ->
        if abs_float (before_pos.w -. after_pos.w)
           >= abs_float (before_pos.h -. after_pos.h)
        then { after_pos with h }
        else { after_pos with w }
      | SW ->
        if abs_float (before_pos.w -. after_pos.w)
           >= abs_float (before_pos.h -. after_pos.h)
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
      (children : t list) : t list =
    let bound = bounding_rect children in (* FIXME what is this? *)
    let scale_w = w /. (if bound.w <= 0.0 then 1.0 else bound.w) in
    let scale_h = h /. (if bound.h <= 0.0 then 1.0 else bound.h) in
    List.map
      (fun (v : t) : t ->
         { x = x +. (v.x -. bound.x) *. scale_w
         ; y = y +. (v.y -. bound.y) *. scale_h
         ; w = v.w *. scale_w
         ; h = v.h *. scale_h
         })
      children

  let adjust ?aspect_ratio
      ?(snap_lines = true)
      ?(min_width = 20.)
      ?(min_height = 20.)
      ?(min_distance = 12.)
      ?grid_step
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
    let min_width, min_height = get_min_rect_size ~min_width ~min_height positions in
    let position_clip_parent = match aspect_ratio with
      | None ->
        clip_to_parent ~parent_size
          ~min_width
          ~min_height
          position_asp
          action
      | Some _ ->
        clip_to_parent ~parent_size
          ~min_width:0.0
          ~min_height:0.0
          position_asp
          action  (* not calc min sizes *)
    in
    let snap_lines =
      if snap_lines
      then get_snap_lines position_clip_parent siblings action
      else [] in
    let children = match action with
      | `Move ->
        move_children
          position_clip_parent
          (resize_children position_clip_parent positions)
      | `Resize _ -> resize_children position_clip_parent positions
    in
    position_clip_parent, children, snap_lines

end

let absolute_to_normalized ~(parent_size : float * float)
    (pos : Absolute.t) : Normalized.t =
  let w, h =
    pos.w /. (fst parent_size),
    pos.h /. (snd parent_size) in
  let x = (pos.x *. w) /. pos.w in
  let y = (pos.y *. h) /. pos.h in
  Normalized.validate { x; y; w; h }
