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

let get_mouse_position : int option -> event -> float * float =
  fun touch_id -> function
    | Mouse e ->
      begin match Js.Optdef.(to_option e##.pageX,
                             to_option e##.pageY) with
      | Some page_x, Some page_y ->
        float_of_int page_x, float_of_int page_y
      | _ -> failwith "no page coordinates in mouse event"
      end
    | Touch e ->
      let touches = e##.changedTouches in
      let rec aux acc i =
        if i >= touches##.length then acc else
          let touch = unwrap (touches##item i) in
          match touch_id with
          | None -> Some touch
          | Some id ->
            if touch##.identifier = id then Some touch else
              aux acc (succ i) in
      match aux None 0 with
      | None -> failwith "no touch event found"
      | Some t -> float_of_int t##.pageX, float_of_int t##.pageY

class t (elt : Dom_html.element Js.t) () =
  object(self)
    val resizers = Element.query_selector_all elt Selector.resizer
    val mutable _min_size = 20

    val mutable _down_listener = None
    val mutable _move_listener = None
    val mutable _stop_listener = None

    (* Initial position and size of element relative to parent *)
    val mutable _position = Position.empty
    (* Initial position of mouse cursor (or touch) relative to page *)
    val mutable _coordinate = 0, 0
    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ()

    method! initial_sync_with_dom () : unit =
      _down_listener <- Some (
        Events.mousedowns super#root self#on_mousedown);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      self#draw_background ();
      super#layout ()

    method! destroy () : unit =
      super#destroy ()

    (* Private methods *)

    method private on_mousedown e _ =
      let target = Dom_html.eventTarget e in
      if Element.has_class target Markup.CSS.resizer
      then begin match e##.button with
        | 0 ->
          Dom.preventDefault e;
          let dir = resize_dir_of_event e in
          (* Refresh element position and size *)
          _position <- Position.of_element super#root;
          (* Refresh mouse cursor position *)
          _coordinate <- ( (Js.Unsafe.coerce e)##.pageX
                         , (Js.Unsafe.coerce e)##.pageY);
          (* Attach event listeners *)
          _move_listener <- Some (Events.mousemoves Dom_html.window (self#resize dir));
          _stop_listener <- Some (Events.mouseups Dom_html.window self#stop_move);
          Lwt.return_unit
        | _ -> Lwt.return_unit
      end
      else begin match e##.button with
        | 0 ->
          Dom.preventDefault e;
          _position <- Position.of_element super#root;
          _coordinate <- ( (Js.Unsafe.coerce e)##.pageX
                         , (Js.Unsafe.coerce e)##.pageY);
          _move_listener <- Some (Events.mousemoves Dom_html.window self#move);
          _stop_listener <- Some (Events.mouseups Dom_html.window self#stop_move);
          Lwt.return_unit
        | _ -> Lwt.return_unit
      end

    method private stop_move e _ =
      match e##.button with
      | 0 ->
        Utils.Option.iter Lwt.cancel _move_listener;
        Utils.Option.iter Lwt.cancel _stop_listener;
        Lwt.return_unit
      | _ -> Lwt.return_unit

    (* Moves an element *)
    method private move (e : Dom_html.mouseEvent Js.t) _ : unit Lwt.t =
      let page_x, page_y =
        (Js.Unsafe.coerce e)##.pageX,
        (Js.Unsafe.coerce e)##.pageY in
      let x = _position.x + page_x - (fst _coordinate) in
      let y = _position.y + page_y - (snd _coordinate) in
      super#root##.style##.left := Utils.px_js x;
      super#root##.style##.top := Utils.px_js y;
      Lwt.return_unit

    (* Resizes an element *)
    method private resize dir (e : Dom_html.mouseEvent Js.t) _ : unit Lwt.t =
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
