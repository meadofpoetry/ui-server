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

let drag_type_prefix = "application/grid-item"

module Event = struct
  type action =
    | Move
    | Resize

  class type detail =
    object
      method originalRect : Dom_html.clientRect Js.t Js.readonly_prop
      method rect : Dom_html.clientRect Js.t Js.readonly_prop
      method action : action Js.readonly_prop
      method direction : Position.resize_direction Js.readonly_prop
    end

  class type input =
    object
      inherit [detail Js.t] Widget.custom_event
    end

  class type event =
    object
      inherit [Dom_html.clientRect Js.t] Widget.custom_event
    end

  let input : input Js.t Events.Typ.t =
    Events.Typ.make "mosaic-resizable:resize"
  let change : event Js.t Events.Typ.t =
    Events.Typ.make "mosaic-resizable:change"
  let selected : event Js.t Events.Typ.t =
    Events.Typ.make "mosaic-resizable:selected"
end

let unwrap x = Js.Optdef.get x (fun () -> assert false)

let resize_dir_of_event (e : #Dom_html.event Js.t) : Position.resize_direction option =
  let target = Dom_html.eventTarget e in
  if Element.has_class target Markup.CSS.resizer_top_left
  then Some Top_left
  else if Element.has_class target Markup.CSS.resizer_top_right
  then Some Top_right
  else if Element.has_class target Markup.CSS.resizer_bottom_left
  then Some Bottom_left
  else if Element.has_class target Markup.CSS.resizer_bottom_right
  then Some Bottom_right
  else if Element.has_class target Markup.CSS.resizer_top
  then Some Top
  else if Element.has_class target Markup.CSS.resizer_bottom
  then Some Bottom
  else if Element.has_class target Markup.CSS.resizer_left
  then Some Left
  else if Element.has_class target Markup.CSS.resizer_right
  then Some Right
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
  | "mousemove" | "mousedown" | "dragover" ->
    let (e : Dom_html.mouseEvent Js.t) = Js.Unsafe.coerce event in
    begin match Js.Optdef.(to_option e##.pageX,
                           to_option e##.pageY) with
    | Some page_x, Some page_y -> page_x, page_y
    | _ -> failwith "no page coordinates in mouse event"
    end
  | "touchmove" | "touchstart" ->
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
      super#init ()

    method! initial_sync_with_dom () : unit =
      _mousedown_listener <- Some (
          Events.mousedowns super#root self#handle_mouse_down);
      _touchstart_listener <- Some (
          Events.touchstarts super#root self#handle_touch_start);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      super#layout ()

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

    method private notify_input ?(direction = Position.Top_left) action position : unit =
      let (detail : Event.detail Js.t) =
        object%js
          val rect = Position.to_client_rect position
          val originalRect = Position.to_client_rect _position
          val action = action
          val direction = direction
        end in
      super#emit ~should_bubble:true ~detail Event.input

    method private notify_change () : unit =
      let detail = Position.to_client_rect @@ Position.of_element super#root in
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
      _stop_listener <- None

    method private handle_drag_end () : unit =
      self#stop_move_listeners ();
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
      self#notify_input Move position;
      Lwt.return_unit

    (* Resizes an element *)
    method private resize : 'a. Position.resize_direction option
      -> (#Dom_html.event as 'a) Js.t
      -> unit Lwt.t =
      fun dir e ->
      _dragging <- true;
      match dir with
      | None -> Lwt.return_unit
      | Some direction ->
        let page_x, page_y = get_cursor_position ?touch_id:_touch_id e in
        let position = match direction with
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
            }
          | Top ->
            { _position with
              h = _position.h - (page_y - (snd _coordinate))
            ; y = _position.y + (page_y - (snd _coordinate))
            }
          | Bottom ->
            { _position with
              h = _position.h + (page_y - (snd _coordinate))
            ; y = _position.y + (page_y - (snd _coordinate))
            }
          | Left ->
            { _position with
              w = _position.w - (page_x - (fst _coordinate))
            ; x = _position.x + (page_x - (fst _coordinate))
            }
          | Right ->
            { _position with
              w = _position.w - (page_x - (fst _coordinate))
            ; x = _position.x + (page_x - (fst _coordinate))
            } in
        self#notify_input ~direction Resize position;
        Lwt.return_unit

  end

let make ?classes () : t =
  let element =
    Tyxml_js.To_dom.of_element
    @@ Markup.create_resizable ?classes () in
  new t element ()
