open Js_of_ocaml

let contains (elt : #Dom.node Js.t) (child : #Dom.node Js.t) =
  Js.to_bool @@ (Js.Unsafe.coerce elt)##contains child

let delay f = ignore @@ Dom_html.setTimeout f 0.

let is_escape_event (e : Dom_html.keyboardEvent Js.t) : bool =
  let key =
    match Js.Optdef.to_option e##.key with
    | None -> None
    | Some x -> Some (Js.to_string x)
  in
  match key, e##.keyCode with
  | Some "Escape", _ | Some "Esc", _ | _, 27 -> true
  | _ -> false

let is_tab_event (e : Dom_html.keyboardEvent Js.t) : bool =
  let key =
    match Js.Optdef.to_option e##.key with
    | None -> None
    | Some x -> Some (Js.to_string x)
  in
  match key, e##.keyCode with
  | Some "Tab", _ | _, 9 -> true
  | _ -> false

let select_if_available (node : Dom_html.element Js.t) : unit =
  match Js.to_string node##.tagName with
  | "input" ->
      let (i : Dom_html.inputElement Js.t) = Js.Unsafe.coerce node in
      let i' = Js.Unsafe.coerce i in
      if Js.Optdef.test i'##.select
         && (String.equal "function" @@ Js.to_string @@ Js.typeof i'##.select)
      then i##select
  | _ -> ()

let remove ~eq x l =
  let rec remove' eq x acc l =
    match l with
    | [] -> List.rev acc
    | y :: tl when eq x y -> remove' eq x acc tl
    | y :: tl -> remove' eq x (y :: acc) tl
  in
  remove' eq x [] l

let move_to_head ~eq (x : 'a) (l : 'a list) : 'a list =
  let rec find prev = function
    | [] -> x :: l
    | y :: tl when eq x y -> y :: (List.rev prev @ tl)
    | y :: tl -> find (y :: prev) tl
  in
  find [] l

type t =
  { state : state
  ; container : Dom_html.element Js.t
  ; pause : unit -> unit
  ; unpause : unit -> unit
  ; options : options }

and state =
  { mutable first_tabbable : Dom_html.element Js.t option
  ; mutable last_tabbable : Dom_html.element Js.t option
  ; mutable focused_before : Dom_html.element Js.t option
  ; mutable recently_focused : Dom_html.element Js.t option
  ; mutable active : bool
  ; mutable paused : bool
  ; mutable listeners : Dom_events.listener list }

and options =
  { on_activate : (unit -> unit) option
  ; on_deactivate : (unit -> unit) option
  ; initial_focus : Dom_html.element Js.t option
  ; fallback_focus : Dom_html.element Js.t option
  ; escape_deactivates : bool
  ; click_outside_deactivates : bool
  ; return_focus_on_deactivate : bool }

let equal (a : t) (b : t) : bool = a == b

type queue = t list ref

let queue : queue = ref []

let remove_listeners (t : t) : unit =
  if t.state.active
  then (
    List.iter Dom_events.stop_listen t.state.listeners;
    t.state.listeners <- [])

let get_initial_focus_node (t : t) : Dom_html.element Js.t =
  match t.options.initial_focus with
  | Some x -> x
  | None -> (
      let active =
        Js.Opt.bind Dom_html.document##.activeElement (fun active ->
            if contains t.container active then Js.some active else Js.null)
      in
      match Js.Opt.to_option active with
      | Some x -> x
      | None -> (
        match t.state.first_tabbable with
        | Some x -> x
        | None -> (
          match t.options.fallback_focus with
          | Some x -> x
          | None ->
              failwith
                "You can't have a focus-trap without at least one focusable element")))

let update_tabbable_nodes (t : t) : unit =
  let tabbable = Tabbable.get_tabbable t.container in
  let first_tabbable =
    match tabbable with
    | [] -> get_initial_focus_node t
    | x :: _ -> x
  in
  let last_tabbable =
    match List.rev tabbable with
    | [] -> get_initial_focus_node t
    | x :: _ -> x
  in
  t.state.first_tabbable <- Some first_tabbable;
  t.state.last_tabbable <- Some last_tabbable

let pause (t : t) : unit =
  if (not t.state.paused) && t.state.active
  then (
    t.state.paused <- true;
    remove_listeners t)

let rec try_focus (t : t) (node : Dom_html.element Js.t option) : unit =
  match node, Js.Opt.to_option Dom_html.document##.activeElement with
  | None, None -> ()
  | Some x, Some a when x == a -> ()
  | None, Some _ -> try_focus t (Some (get_initial_focus_node t))
  | Some x, _ ->
      x##focus;
      t.state.recently_focused <- node;
      select_if_available x

let deactivate_trap (x : t) : unit =
  queue := remove ~eq:equal x !queue;
  match !queue with
  | [] -> ()
  | y :: _ -> y.unpause ()

let deactivate ?return_focus (t : t) : unit =
  if t.state.active
  then (
    let return_focus =
      match return_focus with
      | None -> t.options.return_focus_on_deactivate
      | Some x -> x
    in
    remove_listeners t;
    t.state.active <- false;
    t.state.paused <- false;
    deactivate_trap t;
    (match t.options.on_deactivate with
    | None -> ()
    | Some f -> f ());
    if return_focus then delay (fun () -> try_focus t t.state.focused_before))

(** This needs to be done on mousedown and touchstart instead of click
    so that it precedes the focus event. *)
let check_pointer_down (t : t) (e : #Dom_html.event Js.t) =
  match Js.Opt.to_option e##.target with
  | Some target when contains t.container target -> ()
  | target ->
      let return_focus =
        match target with
        | None -> false
        | Some x -> Tabbable.is_focusable x
      in
      if t.options.click_outside_deactivates
      then deactivate ~return_focus t
      else Dom.preventDefault e

(** In case focus escapes the trap for some strange reason, pull it back in. *)
let check_focus_in (t : t) (e : Dom_html.event Js.t) =
  let target = Dom.eventTarget e in
  (* In Firefox when you Tab out of an iframe the Document is briefly focused. *)
  if (not @@ contains t.container target)
     && (not @@ Js.instanceof target Js.Unsafe.global ##. Document)
  then (
    let node =
      match t.state.recently_focused with
      | Some x -> Some x
      | None -> Some (get_initial_focus_node t)
    in
    ignore @@ (Js.Unsafe.coerce e)##stopImmediatePropagation;
    try_focus t node)

(** Hijack Tab events on the first and last focusable nodes of the trap,
    in order to prevent focus from escaping. If it escapes for even a
    moment it can end up scrolling the page and causing confusion so we
    kind of need to capture the action at the keydown phase. *)
let check_tab (t : t) (e : Dom_html.keyboardEvent Js.t) =
  update_tabbable_nodes t;
  match
    ( Js.to_bool e##.shiftKey
    , Js.Opt.to_option e##.target
    , t.state.first_tabbable
    , t.state.last_tabbable )
  with
  | true, Some target, Some first, _ when target == first ->
      Dom.preventDefault e;
      try_focus t t.state.last_tabbable
  | false, Some target, _, Some last when target == last ->
      Dom.preventDefault e;
      try_focus t t.state.first_tabbable
  | _ -> ()

let check_key (t : t) (e : Dom_html.keyboardEvent Js.t) =
  if t.options.escape_deactivates && is_escape_event e
  then (
    Dom.preventDefault e;
    deactivate t)
  else if is_tab_event e
  then check_tab t e

let check_click (t : t) (e : Dom_html.mouseEvent Js.t) =
  match t.options.click_outside_deactivates, Js.Opt.to_option e##.target with
  | true, _ -> ()
  | _, Some target when contains t.container target -> ()
  | _ ->
      Dom.preventDefault e;
      (Js.Unsafe.coerce e)##stopImmediatePropagation

let activate_trap (x : t) : unit =
  (match !queue with
  | [] -> ()
  | y :: _ -> if not @@ equal x y then y.pause ());
  queue := move_to_head ~eq:equal x !queue

let add_listeners (t : t) : unit =
  if t.state.active
  then (
    (* There can be only one listening focus trap at a time. *)
    activate_trap t;
    (* Delay ensures that the focused element doesn't capture the event
       that caused the focus trap activation. *)
    delay (fun () -> try_focus t @@ Some (get_initial_focus_node t));
    let listen e f =
      let cb _ e =
        f e;
        true
      in
      Dom_events.listen ~capture:true Dom_html.document e cb
    in
    (* XXX do we need `passive` here? *)
    let listeners =
      Dom_events.Typ.
        [ listen (make "focusin") (check_focus_in t)
        ; listen mousedown (check_pointer_down t)
        ; listen touchstart (check_pointer_down t)
        ; listen click (check_click t)
        ; listen keydown (check_key t) ]
    in
    t.state.listeners <- listeners)

let unpause (t : t) : unit =
  if t.state.paused && t.state.active
  then (
    t.state.paused <- false;
    update_tabbable_nodes t;
    add_listeners t)

let activate (t : t) : unit =
  if not t.state.active
  then (
    update_tabbable_nodes t;
    let focused_before = Js.Opt.to_option Dom_html.document##.activeElement in
    t.state.active <- true;
    t.state.paused <- true;
    t.state.focused_before <- focused_before;
    (match t.options.on_activate with
    | None -> ()
    | Some f -> f ());
    add_listeners t)

let make
    ?(on_activate : (unit -> unit) option)
    ?(on_deactivate : (unit -> unit) option)
    ?(initial_focus : Dom_html.element Js.t option)
    ?(fallback_focus : Dom_html.element Js.t option)
    ?(escape_deactivates = true)
    ?(click_outside_deactivates = false)
    ?(return_focus_on_deactivate = true)
    (container : #Dom_html.element Js.t) : t =
  let options =
    { on_activate
    ; on_deactivate
    ; initial_focus
    ; fallback_focus
    ; escape_deactivates
    ; click_outside_deactivates
    ; return_focus_on_deactivate }
  in
  let state =
    { first_tabbable = None
    ; last_tabbable = None
    ; focused_before = None
    ; recently_focused = None
    ; active = false
    ; paused = false
    ; listeners = [] }
  in
  let t =
    { state
    ; container = (container :> Dom_html.element Js.t)
    ; unpause = (fun () -> ())
    ; pause = (fun () -> ())
    ; options }
  in
  {t with unpause = (fun () -> unpause t); pause = (fun () -> pause t)}
