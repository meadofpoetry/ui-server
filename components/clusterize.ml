(** Based on Clusterize.js *)

open Containers
open Utils

type row = Dom_html.element Js.t

type options =
  { rows_in_block : int
  ; blocks_in_cluster : int
  ; scroll_element : Dom_html.element Js.t
  ; content_element : Dom_html.element Js.t
  ; make_empty_row : (unit -> row) option
  ; make_extra_row : (unit -> row)
  ; keep_parity : bool
  }

type cache =
  { mutable top : int
  ; mutable bottom : int
  ; mutable data : row list
  }

type t =
  { mutable rows_in_cluster : int
  ; mutable item_height : int
  ; mutable cluster_height : int
  ; mutable block_height : int
  ; mutable last_cluster : int
  ; mutable scroll_top : int
  ; mutable rows : row list
  ; mutable resize_debounce : Dom_html.timeout_id option
  ; mutable scroll_listener : Dom_events.listener option
  ; mutable resize_listener : Dom_events.listener option
  ; cache : cache
  ; options : options
  }

type cluster =
  { top_offset : int
  ; bottom_offset : int
  ; rows_above : int
  ; rows : row list
  }

let base_class = "mdc-clusterize"

let get_child_nodes (t : t) : row list =
  let nodes = t.options.content_element##.childNodes in
  let rec aux acc = function
    | 0 -> acc
    | i ->
       let (node : row) =
         Js.Opt.get (nodes##item i) (fun () -> raise Not_found)
         |> Js.Unsafe.coerce in
       aux (node :: acc) (pred i) in
  aux [] nodes##.length

let fetch_markup (t : t) : unit =
  let nodes = get_child_nodes t in
  t.rows <- nodes

let get_rows_height (t : t) : bool =
  let prev_item_height = t.item_height in
  let nodes = t.options.content_element##.childNodes in
  match t.rows, nodes##.length with
  | [], _ | _, 0 ->
     t.cluster_height <- 0;
     false
  | _, nodes_length ->
     let index = nodes_length / 2 in
     let (_ : Dom_html.element Js.t) =
       Js.Unsafe.coerce
       @@ Js.Opt.get (nodes##item index) (fun () -> raise Not_found) in
     (* t.item_height <- node##.offsetHeight; *)
     (** Consider margins and border spacing *)
     t.block_height <- t.item_height * t.options.rows_in_block;
     t.rows_in_cluster <- t.options.blocks_in_cluster * t.options.rows_in_block;
     t.cluster_height <- t.options.blocks_in_cluster * t.block_height;
     prev_item_height <> t.item_height

let explore_environment (t : t) : unit =
  match t.rows with
  | [] -> ()
  | _ -> ignore @@ get_rows_height t

(** Get current cluster number *)
let get_cluster_num (t : t) : int =
  t.scroll_top <- t.options.scroll_element##.scrollTop;
  let diff = t.cluster_height - t.block_height in
  floor ((float_of_int t.scroll_top) /. (float_of_int diff))
  |> int_of_float
  |> max 0

(** Generate empty row if no data provided *)
let generate_empty_row (t : t) : row list =
  match t.options.make_empty_row with
  | None -> []
  | Some f -> [f ()]

(** Generate cluster for current scroll position *)
let generate (t : t) (cluster_num : int) : cluster =
  let opts = t.options in
  let rows_len = List.length t.rows in
  if rows_len < opts.rows_in_block
  then
    let rows = match t.rows with
      | [] -> generate_empty_row t
      | l -> l in
    { top_offset = 0
    ; bottom_offset = 0
    ; rows_above = 0
    ; rows
    }
  else
    let items_start =
      (t.rows_in_cluster - opts.rows_in_block) * cluster_num
      |> max 0 in
    let items_end = items_start + t.rows_in_cluster in
    let items_end = min rows_len items_end in
    let top_offset = max 0 (items_start * t.item_height) in
    let bottom_offset = max 0 ((rows_len - items_end) * t.item_height) in
    let rows_above = if top_offset < 1 then items_start + 1 else items_start in
    let len = items_end - items_start in
    let rows =
      Array.of_list t.rows
      |> (fun a -> Array.sub a items_start len)
      |> Array.to_list in
    { top_offset
    ; bottom_offset
    ; rows_above
    ; rows
    }

let check_changes typ (cache : cache) : bool =
  let changed = match typ with
    | `Top x -> cache.top = x
    | `Bottom x -> cache.bottom = x
    | `Data x -> List.equal Equal.physical x cache.data in
  let set = function
    | `Top x -> cache.top <- x
    | `Bottom x -> cache.bottom <- x
    | `Data x -> cache.data <- x in
  if changed then set typ;
  changed

let render_extra_row ?height  (t : t) (modifier_class : string) : row =
  let classes =
    Components_markup.CSS.(
      [ add_element base_class "extra-row"
      ; add_modifier base_class modifier_class
      ]
    ) in
  let row = t.options.make_extra_row () in
  List.iter (fun c -> row##.classList##add (Js.string c)) classes;
  Option.iter (fun height ->
      let s = Printf.sprintf "%dpx" height in
      row##.style##.height := Js.string s) height;
  row

let html (t : t) (rows : row list) : unit =
  let elt = t.options.content_element in
  while Js.Opt.test elt##.firstChild do
    Js.Opt.iter elt##.firstChild (Dom.removeChild elt);
  done;
  List.iter (Dom.appendChild elt) rows

let insert_to_dom (t : t) : unit =
  if t.cluster_height = 0 then explore_environment t;
  let data = generate t (get_cluster_num t) in
  let this_cluster_content_changed =
    check_changes (`Data data.rows) t.cache in
  let top_offset_changed =
    check_changes (`Top data.top_offset) t.cache in
  let only_bottom_offset_changed =
    check_changes (`Bottom data.bottom_offset) t.cache in
  if true || this_cluster_content_changed || top_offset_changed
  then
    let parity =
      if not t.options.keep_parity then None else
        Some (render_extra_row t "keep-parity") in
    let top = render_extra_row ~height:data.top_offset t "top-space" in
    let layout = List.cons_maybe parity (top :: data.rows) in
    let layout = match data.bottom_offset with
      | x when x <= 0 -> layout
      | height ->
         let extra = render_extra_row ~height t "bottom-space" in
         layout @ [extra] in
    (* TODO Call 'cluster will change' here *)
    html t layout;
    (* TODO Call 'cluster changed' here *)
    ()
  else if only_bottom_offset_changed
  then
    let last = t.options.content_element##.lastChild in
    begin match Option.map Js.Unsafe.coerce @@ Js.Opt.to_option last with
    | None -> ()
    | Some (last : Dom_html.element Js.t) ->
       let height = Printf.sprintf "%dpx" data.bottom_offset in
       last##.style##.height := Js.string height
    end

let add (t : t) = function
  | `Prepend rows ->
     t.rows <- (rows @ t.rows);
     insert_to_dom t
  | `Append rows ->
     print_endline "clusterize - before update var";
     t.rows <- (t.rows @ rows);
     print_endline "clusterize - after append var";
     insert_to_dom t;
     print_endline "clusterize - after insert to dom"

let update (t : t) (rows : row list) : unit =
  let scroll_top = t.options.scroll_element##.scrollTop in
  t.rows <- rows;
  if List.length rows * t.item_height < scroll_top
  then (
    t.options.scroll_element##.scrollTop := 0;
    t.last_cluster <- 0);
  insert_to_dom t;
  t.options.scroll_element##.scrollTop := scroll_top

let refresh ?(force = false) (t : t) : unit =
  if force || get_rows_height t then update t t.rows

let add_listeners (t : t) : unit =
  let scroll_ev = fun _ _ ->
    (* TODO add mac scrolling fix *)
    let cluster = get_cluster_num t in
    if t.last_cluster <> cluster
    then insert_to_dom t;
    t.last_cluster <- cluster;
    (* TODO Call 'scrolling progress' here *)
    true in
  (* FIXME calling repeatedly *)
  let resize_ev = fun _ _ ->
    Option.iter (fun x -> Dom_html.window##clearTimeout x) t.resize_debounce;
    let cb = Js.wrap_callback @@ (fun () -> refresh t) in
    let timer = Dom_html.window##setTimeout cb 100. in
    t.resize_debounce <- Some timer;
    true in
  Dom_events.(listen t.options.scroll_element Typ.scroll scroll_ev)
  |> (fun l -> t.scroll_listener <- Some l);
  Dom_events.(listen Dom_html.window Typ.resize resize_ev)
  |> (fun l -> t.resize_listener <- Some l)

let destroy ?(clean = true) (t : t) : unit =
  Option.iter Dom_events.stop_listen t.scroll_listener;
  t.scroll_listener <- None;
  Option.iter Dom_events.stop_listen t.resize_listener;
  t.resize_listener <- None;
  let layout = match clean with
    | false -> t.rows
    | true -> generate_empty_row t in
  html t layout

let clear (t : t) : unit =
  update t []

let get_rows_amount (t : t) : int =
  List.length t.rows

let get_scroll_progress (t : t) : float =
  let st = float_of_int t.scroll_top in
  st /. (float_of_int ((get_rows_amount t) * t.item_height)) *. 100.

let append (t : t) (rows : row list) : unit =
  add t (`Append rows)

let prepend (t : t) (rows : row list) : unit =
  add t (`Prepend rows)

let make ?(rows_in_block = 50)
      ?(blocks_in_cluster = 4)
      ?rows
      ?(keep_parity = true)
      ~scroll_element
      ~content_element
      ?make_empty_row
      ~make_extra_row
      () : t =
  let options =
    { rows_in_block
    ; blocks_in_cluster
    ; keep_parity
    ; scroll_element
    ; content_element
    ; make_empty_row
    ; make_extra_row
    } in
  let cache =
    { data = []
    ; top = 0
    ; bottom = 0
    } in
  let t =
    { rows_in_cluster = 0
    ; item_height = 32 (* FIXME not set initially *)
    ; cluster_height = 0
    ; block_height = 0
    ; last_cluster = 0
    ; scroll_top = 0
    ; resize_debounce = None
    ; scroll_listener = None
    ; resize_listener = None
    ; options
    ; cache
    ; rows = []
    } in
  begin match rows with
  | None | Some [] -> fetch_markup t | Some l -> t.rows <- l
  end;
  (* tabindex forces the browser to keep focus on the scrolling list *)
  let tabindex = Js.string "tabindex" in
  if Js.to_bool @@ content_element##hasAttribute tabindex
  then content_element##setAttribute tabindex (Js.string "0");
  let scroll_top = scroll_element##.scrollTop in
  insert_to_dom t;
  scroll_element##.scrollTop := scroll_top;
  add_listeners t;
  t
