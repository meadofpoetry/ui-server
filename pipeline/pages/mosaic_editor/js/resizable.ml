open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

module Selector = struct
  let resizer = "." ^ Markup.CSS.resizer
end

type event =
  | Mouse of Dom_html.mouseEvent Js.t
  | Touch of Dom_html.touchEvent Js.t

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
    } [@@deriving show]

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
  | "mousemove" ->
    let (e : Dom_html.mouseEvent Js.t) = Js.Unsafe.coerce event in
    begin match Js.Optdef.(to_option e##.pageX,
                           to_option e##.pageY) with
    | Some page_x, Some page_y -> page_x, page_y
    | _ -> failwith "no page coordinates in mouse event"
    end
  | "touchmove" ->
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

class t (elt : Dom_html.element Js.t) () =
  object(self)
    val resizers = Element.query_selector_all elt Selector.resizer
    val mutable _min_size = 20

    val mutable _touchstart_listener = None
    val mutable _down_listener = None
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
      (* _down_listener <- Some (
       *     Events.mousedowns super#root self#handle_mouse_down); *)
      _touchstart_listener <- Some (
          Events.touchstarts super#root self#handle_touch_start);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      self#draw_background ();
      super#layout ()

    method! destroy () : unit =
      super#destroy ()

    (* Private methods *)

    method private handle_touch_start (e : Dom_html.touchEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      let target = Dom_html.eventTarget e in
      Dom.preventDefault e;
      _position <- Position.of_element super#root;
      _coordinate <- ( (Js.Unsafe.coerce e)##.pageX
                     , (Js.Unsafe.coerce e)##.pageY);
      begin match Js.Optdef.to_option (e##.changedTouches##item 0) with
        | None -> ()
        | Some touch -> _touch_id <- Some touch##.identifier
      end;
      _stop_listener <- Some (Events.touchends Dom_html.window self#handle_touch_end);
      let action =
        if Element.has_class target Markup.CSS.resizer
        then (
          let dir = resize_dir_of_event e in
          `Resize dir)
        else `Move in
      _move_listener <- Some (
          Events.touchmoves Dom_html.window (self#handle_touch_move action));
      Lwt.return_unit

    method private handle_touch_move action (e : Dom_html.touchEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
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

    (* method private handle_mouse_down (e : Dom_html.mouseEvent Js.t)
     *     (_ : unit Lwt.t) : unit Lwt.t =
     *   let target = Dom_html.eventTarget e in
     *   Dom.preventDefault e;
     *   (\* Refresh element position and size *\)
     *   _position <- Position.of_element super#root;
     *   (\* Refresh mouse cursor position *\)
     *   _coordinate <- ( (Js.Unsafe.coerce e)##.pageX
     *                  , (Js.Unsafe.coerce e)##.pageY);
     *   _stop_listener <- Some (
     *       Events.mouseups Dom_html.window self#handle_mouse_up);
     *   let action =
     *     if Element.has_class target Markup.CSS.resizer
     *     then begin match e##.button with
     *       | 0 ->
     *         let dir = resize_dir_of_event e in
     *         `Resize dir
     *       | _ -> `None
     *     end
     *     else begin match e##.button with
     *       | 0 -> `Resize 
     *       | _ -> Lwt.return_unit
     *   end *)

    method private handle_mouse_move (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      self#move e

    method private handle_mouse_up (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      match e##.button with
      | 0 -> self#handle_drag_end (); Lwt.return_unit
      | _ -> Lwt.return_unit

    method private handle_drag_end () : unit =
      Utils.Option.iter Lwt.cancel _move_listener;
      Utils.Option.iter Lwt.cancel _stop_listener;

    (* Moves an element *)
    method private move : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t =
      fun e ->
      let page_x, page_y = get_cursor_position e in
      let x = _position.x + page_x - (fst _coordinate) in
      let y = _position.y + page_y - (snd _coordinate) in
      super#root##.style##.left := Utils.px_js x;
      super#root##.style##.top := Utils.px_js y;
      Lwt.return_unit

    (* Resizes an element *)
    method private resize : 'a. resize_dir option
      -> (#Dom_html.event as 'a) Js.t
      -> unit Lwt.t =
      fun dir e ->
      let page_x, page_y =
        (Js.Unsafe.coerce e)##.pageX,
        (Js.Unsafe.coerce e)##.pageY in
      (match dir with
       | None -> ()
       | Some Top_left ->
         let w = _position.w - (page_x - (fst _coordinate)) in
         let h = _position.h - (page_y - (snd _coordinate)) in
         let x = _position.x + (page_x - (fst _coordinate)) in
         let y = _position.y + (page_y - (snd _coordinate)) in
         if w > _min_size
         then (
           super#root##.style##.width := Utils.px_js w;
           super#root##.style##.left := Utils.px_js x);
         if h > _min_size
         then (
           super#root##.style##.height := Utils.px_js h;
           super#root##.style##.top := Utils.px_js y)
       | Some Top_right ->
         let w = _position.w + (page_x - (fst _coordinate)) in
         let h = _position.h - (page_y - (snd _coordinate)) in
         let y = _position.y + (page_y - (snd _coordinate)) in
         if w > _min_size
         then super#root##.style##.width := Utils.px_js w;
         if h > _min_size
         then (
           super#root##.style##.height := Utils.px_js h;
           super#root##.style##.top := Utils.px_js y)
       | Some Bottom_left ->
         let w = _position.w - (page_x - (fst _coordinate)) in
         let h = _position.h + (page_y - (snd _coordinate)) in
         let x = _position.x + (page_x - (fst _coordinate)) in
         if w > _min_size
         then (
           super#root##.style##.width := Utils.px_js w;
           super#root##.style##.left := Utils.px_js x);
         if h > _min_size
         then super#root##.style##.height := Utils.px_js h
       | Some Bottom_right ->
         let w = _position.w + (page_x - (fst _coordinate)) in
         let h = _position.h + (page_y - (snd _coordinate)) in
         if w > _min_size
         then super#root##.style##.width := Utils.px_js w;
         if h > _min_size
         then super#root##.style##.height := Utils.px_js h);
      self#layout ();
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
