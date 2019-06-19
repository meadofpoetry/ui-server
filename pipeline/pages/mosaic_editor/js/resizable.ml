open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

(* TODO
   1. Take aspect ratio into account while resizing (if any)
   2. Respect parent boundaries when resizing/moving
   3. Check if element collides with its siblings
   4. Show helper alignment lines
   5. Stick to neighbour elements
   6. Extend `resize` dir to handle Top, Left, Right, Bottom dirs *)

type resize_dir =
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right

module Sig : sig
  type line =
    { is_vertical : bool (* Is line vertical *)
    ; is_multiple : bool (* Multiple intersection detected *)
    ; is_center : bool
    ; x : int
    ; y : int
    }

  val show_line : line -> string

  val adjust_position :
    ?aspect_ratio:float (* Aspect ratio of active item, if any *)
    -> Dom_html.element Js.t (* Active item *)
    -> Position.t (* Active item position *)
    -> Dom_html.element Js.t list (* Active item neighbours (with active item) *)
    -> int * int (* Parent width & height *)
    -> Position.t * (line list) (* Adjusted position & lines properties *)
end = struct
  open Position

  type line =
    { is_vertical : bool (* Is line vertical *)
    ; is_multiple : bool (* Multiple intersection detected *)
    ; is_center : bool
    ; x : int
    ; y : int
    } [@@deriving show]

  type line_align_direction =
    | Horizontal_Top
    | Horizontal_Center
    | Horizontal_Bottom
    | Vertical_Left
    | Vertical_Center
    | Vertical_Right
    | None

  (* min_distance - pixels
     return: minimum distance of several lines of one align *)
  let line_find_closest_align_value
      (pos : Position.t)
      (items : Dom_html.element Js.t list)
      min_distance
      line_align_val =
    let rec count_aligns line_align_val distance = function
      | [] -> distance
      | h1 :: h2 ->
        let icompare = Position.of_element h1 in
        let distance =
          match line_align_val with
          | Horizontal_Top ->
            let dist = pos.y - icompare.y in
            if abs dist < min_distance && abs distance > abs dist
            then dist else distance
          | Horizontal_Center ->
            let dist = pos.y + pos.h / 2 - icompare.y - icompare.h / 2 in
            if abs dist < min_distance && abs distance > abs dist
            then dist else distance
          | Horizontal_Bottom ->
            let dist = pos.y + pos.h - icompare.y - icompare.h in
            if abs dist < min_distance && abs distance > abs dist
            then dist else distance
          | Vertical_Left ->
            let dist = pos.x - icompare.x in
            if abs dist < min_distance && abs distance > abs dist
            then dist else distance
          | Vertical_Center ->
            let dist = pos.x + pos.w / 2 - icompare.x - icompare.w / 2 in
            if abs dist < min_distance && abs distance > abs dist
            then dist else distance
          | Vertical_Right ->
            let dist = pos.x + pos.w - icompare.x - icompare.w in
            if abs dist < min_distance && abs distance > abs dist
            then dist else distance
          | None -> distance
        in
        count_aligns line_align_val distance h2
    in
    count_aligns line_align_val (min_distance + 1) items

  (* min_distance - pixels
     return: counts of align of selected type in min_distance interval *)
  let line_align_count
      (pos : Position.t)
      (items : Dom_html.element Js.t list)
      min_distance
      line_align_val =
    let rec count_aligns line_align_val counts = function
      | [] -> counts
      | h1 :: h2 ->
        let icompare = Position.of_element h1 in
        let counts =
          if (line_align_val = Horizontal_Top
              && abs pos.y - icompare.y < min_distance)
          || (line_align_val = Horizontal_Center
              && abs pos.y + pos.h / 2 - icompare.y - icompare.h / 2 < min_distance)
          || (line_align_val = Horizontal_Bottom
              && abs pos.y + pos.h - icompare.y - icompare.h < min_distance)
          || (line_align_val = Vertical_Left
              && abs pos.x - icompare.x < min_distance)
          || (line_align_val = Vertical_Center
              && abs pos.x + pos.w / 2 - icompare.x - icompare.w / 2 < min_distance)
          || (line_align_val = Vertical_Right
              && abs pos.x + pos.w - icompare.x - icompare.w < min_distance)
          then succ counts
          else counts
        in
        count_aligns line_align_val counts h2
    in
    count_aligns line_align_val 0 items

  (* return: direction, count aligns (0 = none align lines),
     closest line distance (if distance > min_distance = no find lines) *)
  let lines_aligns_H_list pos (items : Dom_html.element Js.t list) min_distance =
    let ret_list =
      [ Horizontal_Top,
        line_align_count pos items min_distance Horizontal_Top,
        line_find_closest_align_value pos items min_distance Horizontal_Top
      ; Horizontal_Center,
        line_align_count pos items min_distance Horizontal_Center,
        line_find_closest_align_value pos items min_distance Horizontal_Center
      ; Horizontal_Bottom,
        line_align_count pos items min_distance Horizontal_Bottom,
        line_find_closest_align_value pos items min_distance Horizontal_Bottom
      ] in
    ret_list

  let lines_aligns_V_list pos (items:Dom_html.element Js.t list) min_distance =
    let ret_list =
      [ Vertical_Left,
        line_align_count pos items min_distance Vertical_Left,
        line_find_closest_align_value pos items min_distance Vertical_Left
      ; Vertical_Center,
        line_align_count pos items min_distance Vertical_Center,
        line_find_closest_align_value pos items min_distance Vertical_Center
      ; Vertical_Right,
        line_align_count pos items min_distance Vertical_Right,
        line_find_closest_align_value pos items min_distance Vertical_Right
      ] in
    ret_list

  let get_snap coord min_distance items =
    let rec aux snap snap_min_delta = function
      | [] -> snap
      | (_, aligns_count, distance) :: tl ->
        let snap_min_delta =
          if aligns_count > 0 && abs distance < abs snap_min_delta
          then distance else snap_min_delta in
        let snap =
          if abs snap_min_delta <= min_distance
          then coord + snap_min_delta else snap in
        aux snap snap_min_delta tl in
    aux coord (min_distance + 1) items

  let get_item_snap_y pos min_distance (items : Dom_html.element Js.t list) =
    let snap_list = lines_aligns_V_list pos items min_distance in
    get_snap pos.y min_distance snap_list

  let get_item_snap_x pos min_distance (items : Dom_html.element Js.t list) =
    let snap_list = lines_aligns_H_list pos items min_distance in
    get_snap pos.x min_distance snap_list

  let get_snap_lines
      (pos : Position.t)
      (items : Dom_html.element Js.t list)
      min_distance =
    let snap_list_v = lines_aligns_V_list pos items min_distance in
    let snap_list_h = lines_aligns_H_list pos items min_distance in
    let rec create_lines_v_list acc = function
      | [] -> acc
      | (direction, aligns_count, distance) :: tl ->
        let acc =
          if aligns_count > 0
          then
            let line_ret =
              { is_vertical = true
              ; is_multiple = aligns_count > 1
              ; is_center = direction = Vertical_Center
              ; x =
                  if direction = Vertical_Left
                  then pos.x + distance
                  else if direction = Vertical_Center
                  then pos.x + pos.w / 2 + distance
                  else if direction = Vertical_Right
                  then pos.x + pos.w + distance
                  else 0
              ; y = 0
              } in
            line_ret :: acc
          else acc in
        create_lines_v_list acc tl in
    let rec create_lines_h_list acc = function
      | [] -> acc
      | (direction, aligns_count, distance) :: tl ->
        let acc =
          if aligns_count > 0
          then
            let line_ret =
              { is_vertical = false
              ; is_multiple = aligns_count > 1
              ; is_center = direction = Horizontal_Center
              ; x = 0
              ; y =
                  if direction = Horizontal_Top
                  then pos.y + distance
                  else if direction = Horizontal_Center
                  then pos.y + pos.h / 2 + distance
                  else if direction = Horizontal_Bottom
                  then pos.y + pos.h + distance
                  else 0
              } in
            line_ret :: acc
          else acc in
        create_lines_h_list acc tl in
    let list_lines_ret =
      create_lines_v_list [] snap_list_v
      @ create_lines_h_list [] snap_list_h in
    list_lines_ret

  let adjust_position ?aspect_ratio
      (item : Dom_html.element Js.t)
      (pos : Position.t)
      (items : Dom_html.element Js.t list)
      parent_wh =
    let min_distance = 20 in
    (* FIXME this is broken because these conditions can occur simultaneously *)
    let pos =
      if pos.x < 0
      then { pos with x = 0 }
      else if pos.y < 0
      then { pos with y = 0 }
      else if pos.x + pos.w >= fst parent_wh
      then { pos with x = (fst parent_wh) - pos.w }
      else if pos.y + pos.h >= (snd parent_wh)
      then { pos with y = (snd parent_wh) - pos.h }
      else pos
    in
    (* let pos =
     *   { pos with x = get_item_snap_x pos min_distance items
     *            ; y = get_item_snap_y pos min_distance items
     *   }
     * in *)
    pos, get_snap_lines pos items min_distance

end

module Event = struct
  class type event =
    object
      inherit [Dom_html.clientRect Js.t] Widget.custom_event
    end

  let input : event Js.t Events.Typ.t =
    Events.Typ.make "mosaic-resizable:input"
  let change : event Js.t Events.Typ.t =
    Events.Typ.make "mosaic-resizable:change"
  let selected : event Js.t Events.Typ.t =
    Events.Typ.make "mosaic-resizable:selected"
end

let unwrap x = Js.Optdef.get x (fun () -> assert false)

let resize_dir_of_event (e : #Dom_html.event Js.t) : resize_dir option =
  let target = Dom_html.eventTarget e in
  if Element.has_class target Markup.CSS.resizer_top_left
  then Some Top_left
  else if Element.has_class target Markup.CSS.resizer_top_right
  then Some Top_right
  else if Element.has_class target Markup.CSS.resizer_bottom_left
  then Some Bottom_left
  else if Element.has_class target Markup.CSS.resizer_bottom_right
  then Some Bottom_right
  else None

let get_touch_by_id (touches : Dom_html.touchList Js.t)
    (id : int) : Dom_html.touch Js.t option =
  let rec aux acc i =
    if i >= touches##.length then acc else
      let touch = unwrap (touches##item i) in
      if touch##.identifier = id
      then Some touch else aux acc (succ i) in
  aux None 0

let get_cursor_position ?touch_id (event : #Dom_html.event Js.t) =
  match Js.to_string event##._type with
  | "mousemove" | "mousedown" ->
    let (e : Dom_html.mouseEvent Js.t) = Js.Unsafe.coerce event in
    begin match Js.Optdef.(to_option e##.pageX,
                           to_option e##.pageY) with
    | Some page_x, Some page_y -> page_x, page_y
    | _ -> failwith "no page coordinates in mouse event"
    end
  | "touchmove" | "touchstart"->
    let (e : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
    let touches = e##.changedTouches in
    let rec aux acc i =
      if i >= touches##.length then acc else
        let touch = unwrap (touches##item i) in
        match touch_id with
        | None -> Some touch
        | Some id ->
          if touch##.identifier = id then Some touch else
            aux acc (succ i) in
    (match aux None 0 with
     | None -> failwith "no touch event found"
     | Some t -> t##.pageX, t##.pageY)
  | _ -> 0, 0

class t ?aspect ?(min_size = 20) (elt : Dom_html.element Js.t) () =
  object(self)
    val mutable _min_size = min_size
    val mutable _aspect = aspect

    val mutable _touchstart_listener = None
    val mutable _mousedown_listener = None
    val mutable _move_listener = None
    val mutable _stop_listener = None

    val mutable _dragging = false

    (* Initial position and size of element relative to parent *)
    val mutable _position = Position.empty
    (* Initial position of mouse cursor (or touch) relative to page *)
    val mutable _coordinate = 0, 0

    val mutable _touch_id = None
    inherit Widget.t elt () as super

    method! init () : unit =
      super#add_class @@ Elevation.CSS.elevation 2;
      super#init ()

    method! initial_sync_with_dom () : unit =
      _mousedown_listener <- Some (
          Events.mousedowns super#root self#handle_mouse_down);
      _touchstart_listener <- Some (
          Events.touchstarts super#root self#handle_touch_start);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      super#layout ();
      if super#has_class Markup.CSS.resizable_active
      then self#draw_background ()

    method! destroy () : unit =
      Utils.Option.iter Lwt.cancel _touchstart_listener;
      Utils.Option.iter Lwt.cancel _mousedown_listener;
      _touchstart_listener <- None;
      _mousedown_listener <- None;
      self#handle_drag_end ();
      super#destroy ()

    method set_min_size (x : int) : unit =
      _min_size <- x

    (* Private methods *)

    method private notify_input position : unit =
      let detail = Position.to_client_rect position in
      super#emit ~should_bubble:true ~detail Event.input

    method private notify_change () : unit =
      let detail = Position.to_client_rect _position in
      super#emit ~should_bubble:true ~detail Event.change

    method private notify_selected () : unit =
      let detail = Position.to_client_rect _position in
      super#emit ~should_bubble:true ~detail Event.selected

    method private handle_touch_start (e : Dom_html.touchEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      Dom.preventDefault e;
      self#stop_move_listeners ();
      _dragging <- false;
      begin match Js.Optdef.to_option (e##.changedTouches##item 0) with
        | None -> ()
        | Some touch -> _touch_id <- Some touch##.identifier
      end;
      let target = Dom_html.eventTarget e in
      _position <- Position.of_element super#root;
      _coordinate <- get_cursor_position ?touch_id:_touch_id e;
      let action =
        if Element.has_class target Markup.CSS.resizer
        then `Resize (resize_dir_of_event e)
        else `Move in
      _move_listener <- Some (
          Events.touchmoves Dom_html.window (self#handle_touch_move action));
      _stop_listener <- Some (
          Events.touchends Dom_html.window self#handle_touch_end);
      super#add_class Markup.CSS.resizable_active;
      Lwt.return_unit

    method private handle_touch_move action (e : Dom_html.touchEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      Dom.preventDefault e;
      match _touch_id with
      | None -> self#move e
      | Some id ->
        let touches = e##.changedTouches in
        match get_touch_by_id touches id with
        | None -> Lwt.return_unit
        | Some _ ->
          match action with
          | `Move -> self#move e
          | `Resize dir -> self#resize dir e

    method private handle_touch_end (e : Dom_html.touchEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      begin match _touch_id with
        | None -> self#handle_drag_end ()
        | Some id ->
          let touches = e##.changedTouches in
          match get_touch_by_id touches id with
          | None -> ()
          | Some _ -> self#handle_drag_end ()
      end;
      Lwt.return_unit

    method private handle_mouse_down (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      Dom.preventDefault e;
      self#stop_move_listeners ();
      _dragging <- false;
      let target = Dom_html.eventTarget e in
      (* Refresh element position and size *)
      _position <- Position.of_element super#root;
      (* Refresh mouse cursor position *)
      _coordinate <- get_cursor_position e;
      let action =
        if Element.has_class target Markup.CSS.resizer
        then match e##.button with
          | 0 -> `Resize (resize_dir_of_event e)
          | _ -> `None
        else match e##.button with
          | 0 -> `Move
          | _ -> `None in
      _move_listener <- Some (
          Events.mousemoves Dom_html.window (self#handle_mouse_move action));
      _stop_listener <- Some (
          Events.mouseups Dom_html.window self#handle_mouse_up);
      super#add_class Markup.CSS.resizable_active;
      Lwt.return_unit

    method private handle_mouse_move action (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      match action with
      | `Resize dir -> self#resize dir e
      | `Move -> self#move e
      | `None -> Lwt.return_unit

    method private handle_mouse_up (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      match e##.button with
      | 0 -> self#handle_drag_end (); Lwt.return_unit
      | _ -> Lwt.return_unit

    method private stop_move_listeners () : unit =
      Utils.Option.iter Lwt.cancel _move_listener;
      Utils.Option.iter Lwt.cancel _stop_listener;
      _move_listener <- None;
      _stop_listener <- None;

    method private handle_drag_end () : unit =
      self#stop_move_listeners ();
      super#root##.style##.backgroundImage := Js.string "";
      super#remove_class Markup.CSS.resizable_active;
      if _dragging
      then self#notify_change ()
      else self#notify_selected ()

    (* Moves an element *)
    method private move : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t =
      fun e ->
      let page_x, page_y = get_cursor_position ?touch_id:_touch_id e in
      if page_x <> (fst _coordinate) || page_y <> (snd _coordinate)
      then _dragging <- true;
      let position =
        { _position with x = _position.x + page_x - (fst _coordinate)
                       ; y = _position.y + page_y - (snd _coordinate)
        } in
      self#notify_input position;
      self#draw_background ();
      Lwt.return_unit

    (* Resizes an element *)
    method private resize : 'a. resize_dir option
      -> (#Dom_html.event as 'a) Js.t
      -> unit Lwt.t =
      fun dir e ->
      _dragging <- true;
      match dir with
      | None -> Lwt.return_unit
      | Some pos ->
        let page_x, page_y = get_cursor_position ?touch_id:_touch_id e in
        let position = match pos with
          | Top_left ->
            { Position.
              w = _position.w - (page_x - (fst _coordinate))
            ; h = _position.h - (page_y - (snd _coordinate))
            ; x = _position.x + (page_x - (fst _coordinate))
            ; y = _position.y + (page_y - (snd _coordinate))
            }
          | Top_right ->
            { _position with
              w = _position.w + (page_x - (fst _coordinate))
            ; h = _position.h - (page_y - (snd _coordinate))
            ; y = _position.y + (page_y - (snd _coordinate))
            }
          | Bottom_left ->
            { _position with
              w = _position.w - (page_x - (fst _coordinate))
            ; h = _position.h + (page_y - (snd _coordinate))
            ; x = _position.x + (page_x - (fst _coordinate))
            }
          | Bottom_right ->
            { _position with
              w = _position.w + (page_x - (fst _coordinate))
            ; h = _position.h + (page_y - (snd _coordinate))
            } in
        (* self#set_position position; *)
        self#notify_input position;
        Lwt.return_unit

    (* Paints 3x3 grid inside an element *)
    method private draw_background () : unit =
      let w = int_of_float @@ (float_of_int elt##.offsetWidth) /. 3. in
      let h = int_of_float @@ (float_of_int elt##.offsetHeight) /. 3. in
      let style = Printf.sprintf
          "repeating-linear-gradient(0deg,transparent,transparent %dpx,#CCC %dpx,#CCC %dpx),\
           repeating-linear-gradient(-90deg,transparent,transparent %dpx,#CCC %dpx,#CCC %dpx)"
          h h (succ h)
          w w (succ w) in
      let size = Printf.sprintf "%dpx %dpx" (succ w) (succ h) in
      elt##.style##.backgroundImage := Js.string style;
      (Js.Unsafe.coerce elt##.style)##.backgroundSize := Js.string size
  end

let make () : t =
  let element =
    Tyxml_js.To_dom.of_element
    @@ Markup.create_resizable () in
  new t element ()
