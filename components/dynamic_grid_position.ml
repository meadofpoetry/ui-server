open Containers

type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

let empty = { x = 0; y = 0; w = 0; h = 0 }

let equal a b = a.x = b.x && a.y = b.y && a.w = b.w && a.h = b.h

let to_string (pos:t) = Printf.sprintf "x=%d, y=%d, w=%d, h=%d" pos.x pos.y pos.w pos.h

(* checks if two elements collide, returns true if do and false otherwise *)
let collides (pos1:t) (pos2:t) =
  if (pos1.x + pos1.w <= pos2.x)      then false
  else if (pos1.x >= pos2.x + pos2.w) then false
  else if (pos1.y + pos1.h <= pos2.y) then false
  else if (pos1.y >= pos2.y + pos2.h) then false
  else true

(** get first element that collides with position **)
let get_first_collision ~(f:'a -> t) pos (l:'a list) =
  List.fold_while (fun acc (x:'a) -> if collides pos (f x) then (Some x,`Stop) else (acc,`Continue)) None l

(** get all elements that collides with position **)
let get_all_collisions ~(f:'a -> t) pos (l:'a list) =
  List.fold_left (fun acc (x:'a) -> if collides pos (f x) then x::acc else acc) [] l

(** check if element collides with other elements **)
let has_collision ~(f:'a -> t) x (l:'a list) =
  Option.is_some @@ get_first_collision ~f x l

(** compacts the position vertically **)
let compact ~(f:'a -> t) (pos:t) (l:'a list) : t =
  let rec up (pos:t) =
    let y = pos.y - 1 in
    if y >= 0 && (not @@ has_collision ~f { pos with y } l)
    then up { pos with y }
    else pos in
  up pos

(** Sorts positions by top **)
let sort_by_y ~(f:'a -> t) (l:'a list) = List.sort (fun p1 p2 -> compare (f p1).y (f p2).y) l

(** Given a list of collisions and overall items in grid, resolve these collisions by moving down **)
let rec move_down ?rows ~f ~(eq:'a -> 'a -> bool) ~(collisions:'a list) (pos : t) (l:'a list) =
  let y            = pos.y + pos.h in
  let move x acc   =
    let new_pos    = {(f x) with y} in
    let new_list   = (new_pos, x)::acc in
    let filtered   = List.filter (fun i -> not @@ eq x i) l in
    let collisions = List.filter (fun x -> collides new_pos (f x)) filtered in
    let further    = move_down ?rows ~f ~eq ~collisions new_pos filtered in
    let result     = List.append new_list further in
    match collisions, further with
    | _::_, [] -> false, []
    | _,_      -> true, result
  in
  match rows with
  | None      -> List.fold_left (fun acc x -> snd @@ move x acc) [] collisions
  | Some rows -> List.fold_while (fun acc x ->
                     if y + (f x).h <= rows
                     then let result = move x acc in
                          if fst result
                          then snd result,`Continue
                          else [], `Stop
                     else [], `Stop) [] collisions

(** Given a list of collisions and overall items in grid, resolve these collisions by moving up **)
let move_top ~f ~(eq:'a -> 'a -> bool) ~(collisions:'a list) (pos:t) (l:'a list) =
  List.fold_while (fun acc x ->
      let lst     = List.filter (fun i -> not @@ eq x i) l in
      let new_pos = compact ~f (f x) lst in
      if new_pos.y + new_pos.h <= pos.y
      then ((new_pos, x) :: acc), `Continue
      else [], `Stop) [] collisions

(** Given a list of collisions, overall items in grid and ghost current position,**)
(** resolves these collisions by swapping elements if its possible **)
let swap ~cols ~f ~(eq:'a -> 'a -> bool) ~(collisions:'a list) ~(ghost_pos:t) (pos:t) (l:'a list) =
  List.fold_left (fun acc x ->
      let lst               = List.filter (fun i -> not @@ eq x i) l in
      let x_pos             = f x in
      let coll_pos, new_pos =
        if ghost_pos.x < x_pos.x
        then {x_pos with x  = ghost_pos.x}, {ghost_pos with x = x_pos.x + x_pos.w - ghost_pos.w}
        else {x_pos with x  = ghost_pos.x + ghost_pos.w - x_pos.w}, {ghost_pos with x = x_pos.x}
      in
      List.append acc @@
        if ghost_pos.y = x_pos.y
           && (ghost_pos.x = x_pos.x + x_pos.w || ghost_pos.x + ghost_pos.w = x_pos.x)
           && not @@ has_collision ~f coll_pos lst
           && not @@ has_collision ~f new_pos lst
           && not @@ has_collision ~f pos lst
           && not @@ collides coll_pos pos
           && coll_pos.x + coll_pos.w <= cols
           && coll_pos.x >= 0
        then [coll_pos, x]
        else []) [] collisions

let get_free_rect ?(cmp:(t -> t -> int) option) ~(f:'a -> t) (pos:t) (items:'a list) w h () =
  if has_collision ~f:(fun x -> x) pos items
  then None
  else
    let area pos = pos.w * pos.h in
    let cmp = match cmp with
      | Some f -> f
      | None   ->
         (fun new_pos old_pos ->
           let new_area = area new_pos in
           let old_area = area old_pos in
           compare new_area old_area)
    in
    let items      = List.map f items in
    (* FIXME obviously not optimized algorithm *)
    (* get only elements that are on the way to cursor proection to the left/right side *)
    let x_filtered = List.filter (fun i -> pos.y > i.y && pos.y < i.y + i.h) items in
    (* get cursor proection to the left side *)
    let l = List.filter (fun i -> i.x < pos.x) x_filtered
            |> List.fold_left (fun acc i -> if i.x + i.w > acc.x + acc.w then i else acc) empty
            |> (fun x -> x.x + x.w) in
    (* get cursor proection to the right side *)
    let r =
      List.filter (fun i -> i.x > pos.x) x_filtered
      |> List.fold_left (fun acc i -> if i.x < acc.x then i else acc)
                        { x=w; y=0; w=0; h=0 }
      |> (fun x -> x.x) in
    (* get only elements that are on the way to cursor proection to the top/bottom side *)
    let y_filtered = List.filter
                       (fun i -> pos.x > i.x && pos.x < i.x + i.w && i.x + i.w > l && i.x < r)
                       items in
    (* get cursor proection to the top side *)
    let t = List.filter (fun i -> i.y < pos.y) y_filtered
            |> List.fold_left (fun acc i -> if i.y + i.h > acc.y + acc.h then i else acc) empty
            |> (fun x -> x.y + x.h) in
    (* get cursor proection to the bottom side *)
    let b =
      List.filter (fun i -> i.y > pos.y) y_filtered
      |> List.fold_left (fun acc i -> if i.y < acc.y then i else acc)
                        { x=0; y=h; w=0; h=0}
      |> (fun x -> x.y) in
    (* get available x points, FIXME obviously we don't need to iterate over all items *)
    let xs = List.fold_left (fun acc i ->
                 let join = fun x lst -> if x >= l && x <= r then x :: lst else lst in
                 let acc  = join i.x acc |> join (i.x + i.w) in
                 acc) [] items
             |> (fun x -> l :: x @ [r])
             |> List.sort_uniq ~cmp:(Pervasives.compare)
    in
    (* get available y points, FIXME obviously we don't need to iterate over all items *)
    let ys = List.fold_left (fun acc i ->
                 let join = fun y lst -> if y >= t && y <= b then y :: lst else lst in
                 let acc  = join i.y acc |> join (i.y + i.h) in
                 acc) [] items
             |> (fun x -> t :: x @ [b])
             |> List.sort_uniq ~cmp:(Pervasives.compare)
    in
    (* get biggest non-overlapping rectangle under the cursor *)
    (* FIXME obviously not optimized at all *)
    let a = List.fold_left (fun acc x0 ->
                let xs = List.filter (fun i -> i > x0) xs in
                List.fold_left (fun acc x1 ->
                    List.fold_left (fun acc y0 ->
                        let ys = List.filter (fun i -> i > y0) ys in
                        List.fold_left (fun acc y1 ->
                            let (new_pos:t) = { x = x0; y = y0; w = x1 - x0; h = y1 - y0 } in
                            (*
                             * new rect must be the biggest one available,
                             * it must not overlap with other rects,
                             * it must be under the mouse cursor
                             *)
                            match (cmp new_pos acc),
                                  get_first_collision ~f:(fun x -> x) new_pos items,
                                  collides pos new_pos with
                            | 1, None, true -> new_pos
                            | _             -> acc) acc ys) acc ys) acc xs)
                           empty xs
    in
    if equal a empty then None else Some a

let correct_aspect (p:t) (aspect:int*int) =
  let w = if p.w mod (fst aspect) <> 0
          then let w = (p.w / (fst aspect)) * (fst aspect) in
               if w = 0 then (fst aspect) else w
          else p.w
  in
  let h = if p.h mod (snd aspect) <> 0
          then let h = (p.h / (snd aspect)) * (snd aspect) in
               if h = 0 then (snd aspect) else h
          else p.h
  in
  let sw  = w / (fst aspect) in
  let sh  = h / (snd aspect) in
  let w,h = if sw > sh then (fst aspect) * sh,h
            else w, (snd aspect) * sw
  in
  { p with w; h }

let correct_xy (p:t) par_w par_h =
  let x = if p.x < 0 then 0 else if p.x + p.w > par_w then par_w - p.w else p.x in
  let y = match par_h with
    | Some ph -> if p.y < 0 then 0 else if p.y + p.h > ph then ph - p.h else p.y
    | None    -> if p.y < 0 then 0 else p.y
  in
  { p with x;y }

let correct_w ?max_w ?(min_w=1) (p:t) par_w =
  let w = match max_w with
    | Some max -> if p.w > max then max else if p.w < min_w then min_w else p.w
    | None     -> if p.w < min_w then min_w else p.w
  in
  let w = if p.x + w > par_w then par_w - p.x else w in
  { p with w }

let correct_h ?max_h ?(min_h=1) (p:t) par_h =
  let h = match max_h with
    | Some max -> if p.h > max then max else if p.h < min_h then min_h else p.h
    | None     -> if p.h < min_h then min_h else p.h
  in
  let h = match par_h with
    | Some ph -> if p.y + h > ph then ph - p.y else h
    | None    -> h
  in
  { p with h }

let correct_wh ?max_w ?min_w ?max_h ?min_h p par_w par_h =
  correct_w ?max_w ?min_w p par_w |> (fun p -> correct_h ?max_h ?min_h p par_h)
