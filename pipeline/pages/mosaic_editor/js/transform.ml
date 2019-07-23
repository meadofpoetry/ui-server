open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Page_mosaic_editor_tyxml
open Components

include Page_mosaic_editor_tyxml.Transform
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let name = "transform"

type elt =
  | Query of string
  | Node of Dom_html.element Js.t

let select_all (elts : elt list) =
  List.fold_left (fun acc -> function
      | Query q ->
        let nodes = Dom_html.document##querySelectorAll (Js.string q) in
        Dom.list_of_nodeList nodes @ acc
      | Node e -> e :: acc) [] elts

module Attr = struct
  let direction = "data-direction"
end

let get_z_index (x : Dom_html.element Js.t) : int option =
  int_of_string_opt @@ Js.to_string (Dom_html.window##getComputedStyle x)##.zIndex

let get_topmost_item (items : Dom_html.element Js.t list) =
  match List.rev items with
  | [] -> None
  | [x] -> Some x
  | x :: tl ->
    Some (snd @@ List.fold_left (fun ((z, _) as acc) x ->
        match z, get_z_index x with
        | None, None | Some _, None -> acc
        | None, (Some _ as z) -> (z, x)
        | Some z, Some z' -> if z >= z' then acc else (Some z', x))
        (get_z_index x, x) tl)

type position =
  { left : float
  ; top : float
  ; width : float
  ; height : float
  }

let position_of_element (elt : #Dom_html.element Js.t) : position =
  { left = float_of_int elt##.offsetLeft
  ; top = float_of_int elt##.offsetTop
  ; width = float_of_int elt##.offsetWidth
  ; height = float_of_int elt##.offsetHeight
  }

let position_to_client_rect (p : position) : Dom_html.clientRect Js.t =
  object%js
    val top = p.top
    val left = p.left
    val right = p.left +. p.width
    val bottom = p.top +. p.height
    val width = Js.def p.width
    val height = Js.def p.height
  end

module Event = struct
  type action =
    | Move
    | Resize

  class type detail =
    object
      method originalEvent : Dom_html.event Js.t Js.readonly_prop
      method originalRect : Dom_html.clientRect Js.t Js.readonly_prop
      method rect : Dom_html.clientRect Js.t Js.readonly_prop
      method action : action Js.readonly_prop
      method direction : Direction.t Js.readonly_prop
    end

  class type select = [Dom_html.element Js.t] Widget.custom_event

  class type input =
    object
      inherit [detail Js.t] Widget.custom_event
    end

  class type event =
    object
      inherit [Dom_html.clientRect Js.t] Widget.custom_event
    end

  module Typ = struct
    let select : select Js.t Dom_html.Event.typ =
      Dom_html.Event.make @@ Printf.sprintf "%s:select" name

    let input : input Js.t Dom_html.Event.typ =
      Dom_html.Event.make @@ Printf.sprintf "%s:resize" name

    let change : event Js.t Dom_html.Event.typ =
      Dom_html.Event.make @@ Printf.sprintf "%s:change" name
  end

  let select ?use_capture h =
    Lwt_js_events.make_event ?use_capture Typ.select h

  let select ?cancel_handler ?use_capture h =
    Lwt_js_events.seq_loop ?cancel_handler ?use_capture select h

  let input ?use_capture h =
    Lwt_js_events.make_event ?use_capture Typ.input h

  let inputs ?cancel_handler ?use_capture h =
    Lwt_js_events.seq_loop ?cancel_handler ?use_capture input h

  let change ?use_capture h =
    Lwt_js_events.make_event ?use_capture Typ.change h

  let changes ?cancel_handler ?use_capture h =
    Lwt_js_events.seq_loop ?cancel_handler ?use_capture change h
end

let unwrap x = Js.Optdef.get x (fun () -> assert false)

let direction_of_event (e : #Dom_html.event Js.t) : Direction.t option =
  let target = Dom.eventTarget e in
  match Element.get_attribute target Attr.direction with
  | None -> None
  | Some x -> Direction.of_string x

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

let check_touch f touch_id (e : Dom_html.touchEvent Js.t) t =
  match touch_id with
  | None -> f e t
  | Some id ->
    let touches = e##.changedTouches in
    match get_touch_by_id touches id with
    | None -> Lwt.return_unit
    | Some _ -> f e t

type state =
  { (* Initial position and size of element relative to parent *)
    position : position
  (* Initial position of mouse cursor (or touch) relative to page *)
  ; point : (float * float)
  ; action : [`Resize of Direction.t | `Move]
  ; touch_id : int option
  ; transformables : Dom_html.element Js.t list
  ; mutable single_click : bool
  }

class t
    ?(transformables = [])
    ?(move_threshold = 10.)
    (elt : Dom_html.element Js.t) () =
  object(self)

    val mutable _listeners = []
    val mutable _delayed_listeners = []
    val mutable _temp_listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <- Events.(
          [ mousedowns super#root self#handle_drag_start
          ; touchstarts ~passive:false super#root self#handle_drag_start
          ]);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel _listeners;
      List.iter Lwt.cancel _temp_listeners;
      _listeners <- [];
      _temp_listeners <- [];
      super#destroy ()

    (* Private methods *)

    method private notify_select item : unit =
      super#emit ~should_bubble:true ?detail:item Event.Typ.select

    method private notify_input (state : state) position event : unit =
      let action, direction = match state.action with
        | `Resize dir -> Event.Resize, dir
        | `Move -> Move, N in
      let (detail : Event.detail Js.t) =
        object%js
          val rect = position_to_client_rect position
          val originalEvent = event
          val originalRect = position_to_client_rect state.position
          val action = action
          val direction = direction
        end in
      super#emit ~should_bubble:true ~detail Event.Typ.input

    method private notify_change () : unit =
      let detail = position_to_client_rect @@ position_of_element super#root in
      super#emit ~should_bubble:true ~detail Event.Typ.change

    method private handle_drag_start
      : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t -> unit Lwt.t =
      fun (event : #Dom_html.event Js.t) _ ->
      Dom.preventDefault event;
      Dom_html.stopPropagation event;
      let target = Dom.eventTarget event in
      let button, touch_id =
        Js.Opt.case
          (Dom_html.CoerceTo.mouseEvent event)
          (fun () ->
             let (event : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
             let touch =
               Js.Optdef.to_option
               @@ Js.Optdef.map (event##.changedTouches##item 0)
                 (fun touch -> touch##.identifier) in
             None, touch)
          (fun e -> Some e##.button, None) in
      let action = match button with
        | None | Some 0 ->
          if Element.has_class target CSS.resizer
          then begin match direction_of_event event with
            | None -> None
            | Some dir -> Some (`Resize dir)
          end
          else Some `Move
        | _ -> None in
      match action with
      | None -> Lwt.return_unit
      | Some action ->
        let state =
          { action
          ; point = get_cursor_position event
          ; position = position_of_element elt
          ; touch_id
          ; transformables = select_all transformables
          ; single_click = true
          } in
        let doc = Dom_html.document in
        _temp_listeners <- Lwt_js_events.(
            [ mouseups doc (self#handle_drag_end state)
            ; touchcancels doc (self#handle_drag_end state)
            ; touchends doc (self#handle_drag_end state)
            ]);
        _delayed_listeners <- Events.(
            [ mousemoves doc (self#handle_delayed_drag_move state)
            ; touchmoves ~passive:false doc
                (check_touch (self#handle_delayed_drag_move state) touch_id)
            ]);
        Lwt.return_unit

    method private handle_delayed_drag_move
      : 'a. state
        -> (#Dom_html.event as 'a) Js.t
        -> unit Lwt.t
        -> unit Lwt.t =
      fun ({ point; action; touch_id; _ } as state) event _ ->
      let (x, y) = get_cursor_position ?touch_id event in
      (* Check pixel threshold *)
      if Float.abs ((x +. y) -. ((fst point) +. (snd point))) >= move_threshold
      then (
        List.iter Lwt.cancel _delayed_listeners;
        _delayed_listeners <- [];
        state.single_click <- false;
        let doc = Dom_html.document in
        _temp_listeners <- Events.(
            [ mousemoves doc (self#handle_drag_move state)
            ; touchmoves ~passive:false doc
                (check_touch (self#handle_drag_move state) touch_id)
            ] @ _temp_listeners);
        Lwt.return_unit)
      else Lwt.return_unit

    method private handle_drag_move
      : 'a. state
        -> (#Dom_html.event as 'a) Js.t
        -> unit Lwt.t
        -> unit Lwt.t =
      fun state e _ -> match state.action with
        | `Resize dir -> self#resize dir state e
        | `Move -> self#move state e

    method private handle_drag_end
      : 'a. state -> (#Dom_html.event as 'a) Js.t -> unit Lwt.t -> unit Lwt.t =
      fun state event _ ->
      let f () =
        List.iter Lwt.cancel _delayed_listeners;
        _delayed_listeners <- [];
        List.iter Lwt.cancel _temp_listeners;
        _temp_listeners <- [];
        if state.single_click
        then self#handle_single_click state event
        else (self#notify_change ();
              Lwt.return_unit) in
      Js.Opt.case
        (Dom_html.CoerceTo.mouseEvent event)
        (fun () ->
           let (e : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
           match state.touch_id with
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

    method private handle_single_click
      : 'a. state -> (#Dom_html.event as 'a) Js.t -> unit Lwt.t =
      fun { transformables; touch_id; _ } event ->
      let (x, y) = get_cursor_position ?touch_id event in
      let item =
        get_topmost_item
        @@ List.find_all (fun (elt : Dom_html.element Js.t) ->
            let rect = elt##getBoundingClientRect in
            x >= rect##.left
            && y >= rect##.top
            && x <= rect##.right
            && y <= rect##.bottom) transformables in
      self#notify_select item;
      Lwt.return_unit

    (* Moves an element *)
    method private move : 'a. state -> (#Dom_html.event as 'a) Js.t -> unit Lwt.t =
      fun ({ touch_id
           ; point = (x, y)
           ; position = ({ left; top; _ } as position)
           ; _ } as state) e ->
      let page_x, page_y = get_cursor_position ?touch_id e in
      let dx, dy = page_x -. x, page_y -. y in
      let position = { position with left = left +. dx; top = top +. dy } in
      self#notify_input state position (e :> Dom_html.event Js.t);
      Lwt.return_unit

    (* Resizes an element *)
    method private resize :
      'a. Direction.t
      -> state
      -> (#Dom_html.event as 'a) Js.t
      -> unit Lwt.t =
      fun direction ({ touch_id
                     ; point = (x, y)
                     ; position = ({ left; top; width; height } as position)
                     ; _ } as state) e ->
      let page_x, page_y = get_cursor_position ?touch_id e in
      let dx, dy = page_x -. x, page_y -. y in
      let position = match direction with
        | NW ->
          { width = width -. dx
          ; height = height -. dy
          ; left = left +. dx
          ; top = top +. dy
          }
        | NE ->
          { position with width = width +. dx
                        ; height = height -. dy
                        ; top = top +. dy
          }
        | SW ->
          { position with width = width -. dx
                        ; height = height +. dy
                        ; left = left +. dx
          }
        | SE -> { position with width = width +. dx; height = height +. dy }
        | N -> { position with height = height -. dy; top = top +. dy }
        | W -> { position with width = width -. dx; left = left +. dx }
        | S -> { position with height = position.height +. dy }
        | E -> { position with width = position.width +. dx }
      in
      self#layout ();
      self#notify_input state position (e :> Dom_html.event Js.t);
      Lwt.return_unit

  end

let make ?transformables ?classes () : t =
  let element =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?classes () in
  new t ?transformables element ()
