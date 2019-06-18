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

module Position = struct
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
      val right = float_of_int @@ t.w - t.x
      val bottom = float_of_int @@ t.h - t.y
      val width = Js.def @@ float_of_int t.w
      val height = Js.def @@ float_of_int t.h
    end

  let of_client_rect (rect : Dom_html.clientRect Js.t) : t =
    { x = int_of_float rect##.left
    ; y = int_of_float rect##.top
    ; w = Js.Optdef.case rect##.width (fun () -> 0) int_of_float
    ; h = Js.Optdef.case rect##.height (fun () -> 0) int_of_float
    }
end

module Sig : sig
  type line =
    { is_vertical : bool (* Is line vertical *)
    ; is_multiple : bool (* Multiple intersection detected *)
    ; is_center : bool
    ; x : int
    ; y : int
    }

  val adjust_position :
    ?aspect_ratio:int * int (* Aspect ratio of active item, if any *)
    -> Dom_html.element Js.t (* Active item *)
    -> Position.t (* Active item position *)
    -> Dom_html.element Js.t list (* Active item neighbours (with active item) *)
    -> int * int (* Parent width & height *)
    -> Position.t * (line list) (* Adjusted position & lines properties *)
end = struct
  type line =
    { is_vertical : bool (* Is line vertical *)
    ; is_multiple : bool (* Multiple intersection detected *)
    ; is_center : bool
    ; x : int
    ; y : int
    }

  let adjust_position ?aspect_ratio item pos items parent =
    failwith "Implement"
end

module Event = struct
  class type detail =
    object
      method item : Dom_html.element Js.t Js.readonly_prop
      method rect : Dom_html.clientRect Js.t Js.readonly_prop
    end
  class type event =
    object
      inherit [detail Js.t] Widget.custom_event
    end

  let input : event Js.t Events.Typ.t =
    Events.Typ.make "mosaic-resizable:input"
  let change : event Js.t Events.Typ.t =
    Events.Typ.make "mosaic-resizable:change"
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

    method private notify_input () : unit =
      let (detail : Event.detail Js.t) =
        object%js
          val item = super#root
          val rect = Position.to_client_rect _position
        end in
      super#emit ~should_bubble:true ~detail Event.input

    method private notify_change () : unit =
      let (detail : Event.detail Js.t) =
        object%js
          val item = super#root
          val rect = Position.to_client_rect _position
        end in
      super#emit ~should_bubble:true ~detail Event.change

    method private handle_touch_start (e : Dom_html.touchEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      Dom.preventDefault e;
      self#handle_drag_end ();
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
      self#handle_drag_end ();
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

    method private handle_drag_end () : unit =
      Utils.Option.iter Lwt.cancel _move_listener;
      Utils.Option.iter Lwt.cancel _stop_listener;
      _move_listener <- None;
      _stop_listener <- None;
      super#root##.style##.backgroundImage := Js.string "";
      super#remove_class Markup.CSS.resizable_active;
      self#notify_change ()

    (* Moves an element *)
    method private move : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t =
      fun e ->
      let page_x, page_y = get_cursor_position ?touch_id:_touch_id e in
      let x = _position.x + page_x - (fst _coordinate) in
      let y = _position.y + page_y - (snd _coordinate) in
      _position <- { _position with x; y};
      super#root##.style##.left := Utils.px_js x;
      super#root##.style##.top := Utils.px_js y;
      self#notify_input ();
      self#draw_background ();
      Lwt.return_unit

    (* Resizes an element *)
    method private resize : 'a. resize_dir option
      -> (#Dom_html.event as 'a) Js.t
      -> unit Lwt.t =
      fun dir e ->
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
        _position <- position;
        self#set_position position;
        self#layout ();
        self#notify_input ();
        self#draw_background ();
        Lwt.return_unit

    method private set_position (x : Position.t) =
      if x.w > _min_size
      then (
        super#root##.style##.width := Utils.px_js x.w;
        super#root##.style##.left := Utils.px_js x.x);
      if x.h > _min_size
      then (
        super#root##.style##.height := Utils.px_js x.h;
        super#root##.style##.top := Utils.px_js x.y)

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
