open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components

include Page_mosaic_editor_tyxml.Resizable
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

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

  module Typ = struct
    let input : input Js.t Dom_html.Event.typ =
      Dom_html.Event.make "mosaic-resizable:resize"

    let change : event Js.t Dom_html.Event.typ =
      Dom_html.Event.make "mosaic-resizable:change"

    let select : event Js.t Dom_html.Event.typ =
      Dom_html.Event.make "mosaic-resizable:selected"
  end

  let input ?use_capture h =
    Lwt_js_events.make_event ?use_capture Typ.input h

  let inputs ?cancel_handler ?use_capture h =
    Lwt_js_events.seq_loop ?cancel_handler ?use_capture input h

  let change ?use_capture h =
    Lwt_js_events.make_event ?use_capture Typ.change h

  let changes ?cancel_handler ?use_capture h =
    Lwt_js_events.seq_loop ?cancel_handler ?use_capture change h

  let select ?use_capture h =
    Lwt_js_events.make_event Typ.select h

  let selects ?cancel_handler ?use_capture h =
    Lwt_js_events.seq_loop ?cancel_handler ?use_capture select h
end

let unwrap x = Js.Optdef.get x (fun () -> assert false)

let resize_dir_of_event (e : #Dom_html.event Js.t) : Position.resize_direction option =
  let target = Dom.eventTarget e in
  if Element.has_class target CSS.resizer_top_left
  then Some Top_left
  else if Element.has_class target CSS.resizer_top_right
  then Some Top_right
  else if Element.has_class target CSS.resizer_bottom_left
  then Some Bottom_left
  else if Element.has_class target CSS.resizer_bottom_right
  then Some Bottom_right
  else if Element.has_class target CSS.resizer_top
  then Some Top
  else if Element.has_class target CSS.resizer_bottom
  then Some Bottom
  else if Element.has_class target CSS.resizer_left
  then Some Left
  else if Element.has_class target CSS.resizer_right
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

(* FIXME remove *)
let get_cursor_position ?touch_id (event : #Dom_html.event Js.t) =
  Js.Opt.case (Dom_html.CoerceTo.mouseEvent event)
    (fun () ->
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
        | Some t -> float_of_int t##.pageX, float_of_int t##.pageY))
    (fun (e : Dom_html.mouseEvent Js.t) ->
       begin match Js.Optdef.(to_option e##.pageX,
                              to_option e##.pageY) with
       | Some page_x, Some page_y -> float_of_int page_x, float_of_int page_y
       | _ -> failwith "no page coordinates in mouse event"
       end)

class t ?aspect ?(min_size = 20) (elt : Dom_html.element Js.t) () =
  object(self)
    val mutable _min_size = min_size
    val mutable _aspect = aspect

    val mutable _listeners = []
    val mutable _temp_listeners = []

    val mutable _dragging = false

    (* Initial position and size of element relative to parent *)
    val mutable _position : Position.t = { x = 0.; y = 0.; w = 0.; h = 0. }
    (* Initial position of mouse cursor (or touch) relative to page *)
    val mutable _coordinate = 0., 0.

    val mutable _touch_id = None
    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <- Lwt_js_events.(
          [ mousedowns super#root self#handle_drag_start
          ; touchstarts super#root self#handle_drag_start
          ]);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel _listeners;
      List.iter Lwt.cancel _temp_listeners;
      _listeners <- [];
      _temp_listeners <- [];
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
      super#emit ~should_bubble:true ~detail Event.Typ.input

    method private notify_change () : unit =
      let detail = Position.(to_client_rect @@ of_element super#root) in
      super#emit ~should_bubble:true ~detail Event.Typ.change

    method private notify_selected () : unit =
      let detail = Position.to_client_rect _position in
      super#emit ~should_bubble:true ~detail Event.Typ.select

    method private handle_drag_start
      : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t -> unit Lwt.t =
      fun (event : #Dom_html.event Js.t) _ ->
      Dom.preventDefault event;
      Dom_html.stopPropagation event;
      _dragging <- false;
      let target = Dom.eventTarget event in
      _dragging <- false;
      let button =
        Js.Opt.case
          (Dom_html.CoerceTo.mouseEvent event)
          (fun () ->
             let (event : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
             Js.Optdef.iter (event##.changedTouches##item 0)
               (fun touch -> _touch_id <- Some touch##.identifier);
             None)
          (fun (event : Dom_html.mouseEvent Js.t) ->
             Some event##.button) in
      let action = match button with
        | None | Some 0 ->
          if Element.has_class target CSS.resizer
          then `Resize (resize_dir_of_event event)
          else `Move
        | _ -> `None in
      (* Refresh element position and size *)
      _position <- Position.of_element super#root;
      (* Refresh mouse cursor position *)
      _coordinate <- get_cursor_position event;
      let doc = Dom_html.document in
      _temp_listeners <- Lwt_js_events.(
          [ mouseups doc self#handle_drag_end
          ; touchcancels doc self#handle_drag_end
          ; touchends doc self#handle_drag_end
          ; mousemoves doc (self#handle_drag_move action)
          ; touchmoves doc (fun e t ->
                match _touch_id with
                | None -> self#move e
                | Some id ->
                  let touches = e##.changedTouches in
                  match get_touch_by_id touches id with
                  | None -> Lwt.return_unit
                  | Some _ -> self#handle_drag_move action e t)
          ]);
      super#add_class CSS.active;
      Lwt.return_unit

    method private handle_drag_move
      : 'a. [`Resize of Position.resize_direction option | `Move | `None]
        -> (#Dom_html.event as 'a) Js.t
        -> unit Lwt.t
        -> unit Lwt.t =
      fun action e _ ->
      match action with
      | `Resize dir -> self#resize dir e
      | `Move -> self#move e
      | `None -> Lwt.return_unit

    method private handle_drag_end
      : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t -> unit Lwt.t =
      fun event _ ->
      let f () =
        List.iter Lwt.cancel _temp_listeners;
        _temp_listeners <- [];
        super#remove_class CSS.active;
        if _dragging
        then self#notify_change ()
        else self#notify_selected ();
        Lwt.return_unit in
      Js.Opt.case
        (Dom_html.CoerceTo.mouseEvent event)
        (fun () ->
           let (e : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
           match _touch_id with
           | None -> f ()
           | Some id ->
             let touches = e##.changedTouches in
             match get_touch_by_id touches id with
             | None -> Lwt.return_unit
             | Some _ -> f ())
        (fun (e : Dom_html.mouseEvent Js.t) ->
           match e##.button with
           | 0 -> f ()
           | _ -> Lwt.return_unit)

    (* Moves an element *)
    method private move : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t =
      fun e ->
      let page_x, page_y = get_cursor_position ?touch_id:_touch_id e in
      if page_x <> (fst _coordinate) || page_y <> (snd _coordinate)
      then _dragging <- true;
      let position =
        { _position with x = _position.x +. page_x -. (fst _coordinate)
                       ; y = _position.y +. page_y -. (snd _coordinate)
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
              w = _position.w -. (page_x -. (fst _coordinate))
            ; h = _position.h -. (page_y -. (snd _coordinate))
            ; x = _position.x +. (page_x -. (fst _coordinate))
            ; y = _position.y +. (page_y -. (snd _coordinate))
            }
          | Top_right ->
            { _position with
              w = _position.w +. (page_x -. (fst _coordinate))
            ; h = _position.h -. (page_y -. (snd _coordinate))
            ; y = _position.y +. (page_y -. (snd _coordinate))
            }
          | Bottom_left ->
            { _position with
              w = _position.w -. (page_x -. (fst _coordinate))
            ; h = _position.h +. (page_y -. (snd _coordinate))
            ; x = _position.x +. (page_x -. (fst _coordinate))
            }
          | Bottom_right ->
            { _position with
              w = _position.w +. (page_x -. (fst _coordinate))
            ; h = _position.h +. (page_y -. (snd _coordinate))
            }
          | Top ->
            { _position with
              h = _position.h -. (page_y -. (snd _coordinate))
            ; y = _position.y +. (page_y -. (snd _coordinate))
            }
          | Bottom ->
            { _position with
              h = _position.h +. (page_y -. (snd _coordinate))
            ; y = _position.y +. (page_y -. (snd _coordinate))
            }
          | Left ->
            { _position with
              w = _position.w -. (page_x -. (fst _coordinate))
            ; x = _position.x +. (page_x -. (fst _coordinate))
            }
          | Right ->
            { _position with
              w = _position.w -. (page_x -. (fst _coordinate))
            ; x = _position.x +. (page_x -. (fst _coordinate))
            } in
        self#notify_input ~direction Resize position;
        Lwt.return_unit

  end

let make ?classes () : t =
  let element =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?classes () in
  new t element ()
