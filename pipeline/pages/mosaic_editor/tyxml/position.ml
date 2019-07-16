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

let compare (a : t) (b : t) =
  let c = compare a.x b.x in
  if c <> 0 then c
  else (let c = compare a.y b.y in
        if c <> 0 then c
        else (let c = compare a.w b.w in
              if c <> 0 then c
              else compare a.h b.h))

let equal (a : t) (b : t) =
  compare a b = 0

let show { x; y; w; h } =
  Printf.sprintf "x=%g, y=%g, w=%g, h=%g" x y w h

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
    then (pos.w /. parent_width *. 100.,
          pos.h /. parent_height *. 100.)
    else (parent_width /. pos.w *. 100.,
          parent_height /. pos.h *. 100.) in
  let x = (pos.x *. w) /. pos.w in
  let y = (pos.y *. h) /. pos.h in
  { x; y; w; h }

let of_relative ~(parent_size : float * float) (pos : t) =
  let w = pos.w *. (fst parent_size) /. 100. in
  let h = pos.h *. (snd parent_size) /. 100. in
  let x = pos.x *. w /. pos.w in
  let y = pos.y *. h /. pos.h in
  { x; y; w; h }
