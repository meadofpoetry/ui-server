open Js_of_ocaml

type args =
  { load_count : int
  ; page_index : int }

type 'a options =
  { element : Dom_html.element Js.t
  ; scroll_target : Dom_html.element Js.t option
  ; scroll_threshold : int
  ; get : args -> ('a, string) Lwt_result.t
  ; is_last_page : 'a -> bool
  ; append : 'a -> unit
  ; prefill : bool
  ; on_error : (string -> unit) option }

type 'a t =
  { options : 'a options
  ; mutable response : unit Lwt.t
  ; mutable prefilling : bool
  ; mutable can_load : bool
  ; mutable page_index : int
  ; mutable load_count : int
  ; mutable window_height : int
  ; mutable top : int
  ; mutable scroll_listener : unit Lwt.t option }

let measure (t : 'a t) =
  let w = Dom_html.window in
  t.window_height <- Js.Optdef.get w##.innerHeight (fun () -> 0);
  let rect = (t.options.element)##getBoundingClientRect in
  t.top <- int_of_float rect##.top + (Js.Unsafe.coerce w)##.pageYOffset

let on_error (t : 'a t) (s : string) : unit =
  t.can_load <- false;
  match t.options.on_error with
  | None -> ()
  | Some f -> f s

let on_load (t : 'a t) (response : 'a) : unit =
  t.page_index <- t.page_index + 1;
  t.load_count <- t.load_count + 1;
  if t.options.is_last_page response then t.can_load <- false;
  t.options.append response

let load_next_page (t : 'a t) : unit =
  if (not @@ Lwt.is_sleeping t.response) && t.can_load
  then
    let args = {page_index = t.page_index; load_count = t.load_count} in
    let thread =
      Lwt.try_bind
        (fun () -> t.options.get args)
        (function
          | Ok x -> Lwt.return @@ on_load t x
          | Error e -> Lwt.return @@ on_error t e)
        (fun exn -> Lwt.return @@ on_error t @@ Printexc.to_string exn)
    in
    t.response <- thread

let get_bottom_distance (t : 'a t) : int =
  match t.options.scroll_target with
  | None ->
      let wnd = Dom_html.window in
      let bottom = t.top + (t.options.element)##.clientHeight in
      let scroll_y = (Js.Unsafe.coerce wnd)##.pageYOffset + t.window_height in
      bottom - scroll_y
  | Some x ->
      let bottom = x##.scrollHeight in
      let scroll_y = x##.scrollTop + x##.clientHeight in
      bottom - scroll_y

let get_prefill_distance (t : 'a t) : int =
  match t.options.scroll_target with
  | None -> t.window_height - (t.options.element)##.clientHeight
  | Some target -> target##.clientHeight - target##.scrollHeight

let do_prefill (t : 'a t) : unit =
  let distance = get_prefill_distance t in
  t.prefilling <- distance >= 0;
  if t.prefilling then load_next_page t

let on_scroll (t : 'a t) : unit =
  let distance = get_bottom_distance t in
  if distance <= t.options.scroll_threshold then load_next_page t

let destroy (t : 'a t) : unit =
  match t.scroll_listener with
  | None -> ()
  | Some x ->
      Lwt.cancel x;
      t.scroll_listener <- None

let make
    ~(element : Dom_html.element Js.t)
    ?(scroll_target : Dom_html.element Js.t option)
    ?(scroll_threshold = 400)
    ~(get : args -> ('a, string) Lwt_result.t)
    ~(is_last_page : 'a -> bool)
    ~(append : 'a -> unit)
    ~(prefill : bool)
    ?(on_error : (string -> unit) option)
    () =
  let options =
    { element
    ; scroll_target
    ; scroll_threshold
    ; get
    ; is_last_page
    ; append
    ; prefill
    ; on_error }
  in
  let t =
    { options
    ; response = Lwt.return ()
    ; prefilling = false
    ; can_load = true
    ; load_count = 0
    ; page_index = 1
    ; window_height = 0
    ; top = 0
    ; scroll_listener = None }
  in
  let target =
    match t.options.scroll_target with
    | None -> (Dom_html.window :> Dom_html.eventTarget Js.t)
    | Some x -> (x :> Dom_html.eventTarget Js.t)
  in
  let listener =
    Js_of_ocaml_lwt.Lwt_js_events.(
      seq_loop (make_event Dom_html.Event.scroll) target (fun _ _ ->
          on_scroll t;
          Lwt.return_unit))
  in
  t.scroll_listener <- Some listener;
  measure t;
  if prefill then do_prefill t;
  t
