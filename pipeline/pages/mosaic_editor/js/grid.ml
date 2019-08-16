open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components

(* Inspired by
 * https://github.com/nathancahill/split
 * https://grid.layoutit.com/
*)

include Page_mosaic_editor_tyxml.Grid
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type direction = Col | Row

type dimensions =
  { direction : direction
  ; a_track : int
  ; b_track : int
  ; a_track_start : float
  ; b_track_end : float
  ; total_fr : int
  ; percentage_to_pixels : float
  ; fr_to_pixels : float
  ; tracks : string array
  ; track_values : value array
  ; track_values_px : float array
  ; gap : float
  }

type resize_properties =
  { cell : Dom_html.element Js.t
  ; first_cells : Dom_html.element Js.t list
  ; grid : Dom_html.element Js.t
  ; col : dimensions option
  ; row : dimensions option
  }

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

let name = "resizable-grid"

let fail s = failwith @@ Printf.sprintf "%s: %s" name s

module Attr = struct
  let rowindex = "aria-rowindex"
  let colindex = "aria-colindex"
  let colspan = "aria-colspan"
  let rowspan = "aria-rowspan"
end

module Selector = struct
  let grid = Printf.sprintf ".%s" CSS.root
  let cell = Printf.sprintf ".%s" CSS.cell
end

module Event = struct

  class type item =
    object
      method item : Dom_html.element Js.t Js.readonly_prop
      method rect : Dom_html.clientRect Js.t Js.readonly_prop
    end

  type detail = item Js.t Js.js_array Js.t

  class type resize = [detail] Widget.custom_event

  module Typ = struct
    let (input : resize Js.t Dom_html.Event.typ) =
      Dom_html.Event.make (name ^ ":input")

    let (change : resize Js.t Dom_html.Event.typ) =
      Dom_html.Event.make (name ^ ":change")
  end

  let input ?use_capture t =
    Lwt_js_events.make_event ?use_capture Typ.input t

  let inputs ?cancel_handler ?use_capture t h =
    Lwt_js_events.seq_loop ?cancel_handler ?use_capture input t h

  let change ?use_capture t =
    Lwt_js_events.make_event ?use_capture Typ.change t

  let changes ?cancel_handler ?use_capture t h =
    Lwt_js_events.seq_loop ?cancel_handler ?use_capture change t h
end

module Util = struct

  let insert_at_idx i x l =
    let rec aux l acc i x = match l with
      | [] -> List.rev_append acc [x]
      | y :: l' when i = 0 -> List.rev_append acc (x :: y :: l')
      | y :: l' -> aux l' (y :: acc) (pred i) x
    in
    let i = if i < 0 then List.length l + i else i in
    aux l [] i x

  let remove_at_idx i l0 =
    let rec aux l acc i = match l with
      | [] -> l0
      | _ :: l' when i = 0 -> List.rev_append acc l'
      | y :: l' -> aux l' (y :: acc) (pred i)
    in
    let i = if i < 0 then List.length l0 + i else i in
    aux l0 [] i

  let get_cursor_position ?touch_id (event : Dom_html.event Js.t) =
    Js.Opt.case (Dom_html.CoerceTo.mouseEvent event)
      (fun () ->
         let (e : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
         let touches = e##.changedTouches in
         let rec aux acc i =
           if i >= touches##.length then acc else
             let touch = Js.Optdef.get (touches##item i) (fun () -> assert false) in
             match touch_id with
             | None -> Some touch
             | Some id ->
               if touch##.identifier = id then Some touch else
                 aux acc (succ i) in
         (match aux None 0 with
          | None -> failwith "no touch event found"
          | Some t -> t##.pageX, t##.pageY))
      (fun event -> match Js.Optdef.(to_option event##.pageX,
                                     to_option event##.pageY) with
      | Some page_x, Some page_y -> page_x, page_y
      | _ -> failwith "no page coordinates in mouse event")

  let cell_of_event
      (items : Dom_html.element Js.t list)
      (e : #Dom_html.event Js.t) : Dom_html.element Js.t option =
    let target = Dom_html.eventTarget e in
    let selector = Printf.sprintf ".%s, .%s" CSS.cell CSS.root in
    let nearest_parent = Js.Opt.to_option @@ Element.closest target selector in
    match nearest_parent with
    | None -> None
    | Some parent ->
      if not @@ Element.matches parent ("." ^ CSS.cell)
      then None
      else List.find_opt (Element.equal parent) items

  let get_size_at_track ?(gap = 0.) (tracks : float array) =
    let gap = gap *. (float_of_int @@ pred @@ Array.length tracks) in
    let rec aux sum = function
      | n when n = Array.length tracks -> sum
      | n -> aux (sum +. tracks.(n)) (succ n) in
    (aux 0. 0) +. gap

  let get_parent_grid (cell : Dom_html.element Js.t) =
  let rec aux elt =
    Js.Opt.case (Element.get_parent elt)
      (fun () -> failwith "parent grid not found")
      (fun elt ->
         if Element.has_class elt CSS.root
         then elt else aux elt) in
  aux cell

  let get_cell_position (cell : Dom_html.element Js.t) =
    let parse_span n s =
      let s = String.split_on_char ' ' s in
      match s with
      | ["auto"] -> 1
      | ["span"; v] -> int_of_string v
      | [s] -> int_of_string s - n
      | _ -> failwith "unknown cell span value" in
    let style = Dom_html.window##getComputedStyle cell in
    let col = Js.parseInt (Js.Unsafe.coerce style)##.gridColumnStart in
    let row = Js.parseInt (Js.Unsafe.coerce style)##.gridRowStart in
    let col_end = Js.to_string (Js.Unsafe.coerce style)##.gridColumnEnd in
    let row_end = Js.to_string (Js.Unsafe.coerce style)##.gridRowEnd in
    { col
    ; row
    ; col_span = parse_span col col_end
    ; row_span = parse_span row row_end
    }

  let set_cell_row ?(span = 1) (row : int) (cell : Dom_html.element Js.t) =
    let start = Js.string @@ Printf.sprintf "%d" row in
    let end' = Js.string @@ Printf.sprintf "%d" (row + span) in
    (Js.Unsafe.coerce cell##.style)##.gridRowStart := start;
    (Js.Unsafe.coerce cell##.style)##.gridRowEnd := end';
    cell##setAttribute (Js.string Attr.rowindex) start;
    cell##setAttribute (Js.string Attr.rowspan) (Js.string @@ string_of_int span)

  let set_cell_col ?(span = 1) (col : int) (cell : Dom_html.element Js.t) =
    let start = Js.string @@ Printf.sprintf "%d" col in
    let end' = Js.string @@ Printf.sprintf "%d" (col + span) in
    (Js.Unsafe.coerce cell##.style)##.gridColumnStart := start;
    (Js.Unsafe.coerce cell##.style)##.gridColumnEnd := end';
    cell##setAttribute (Js.string Attr.colindex) start;
    cell##setAttribute (Js.string Attr.colspan) (Js.string @@ string_of_int span)

  let set_cell_position { col; row; col_span; row_span }
      (cell : Dom_html.element Js.t) =
    set_cell_col ~span:col_span col cell;
    set_cell_row ~span:row_span row cell

  let find_first_cell dir n cells =
    snd
    @@ Option.get
    @@ List.fold_left (fun acc cell ->
        let pos = get_cell_position cell in
        let main, aux = match dir with
          | Col -> pos.col, pos.row
          | Row -> pos.row, pos.col in
        match acc with
        | None -> if main = n then Some (aux, cell) else None
        | Some (aux', _) ->
          if n = main && aux < aux'
          then Some (aux, cell)
          else acc) None cells

  let first_non_zero f a =
    let rec aux = function
      | n when n < 0 -> None
      | n ->
        match f a.(n) with
        | None -> aux (pred n)
        | Some v ->
          if v <> 0. then Some (n, v)
          else aux (pred n) in
    aux (pred @@ Array.length a)

  let fr_to_pixels track_values computed_values =
    match first_non_zero (function
        | Fr x -> Some x
        | _ -> None) track_values with
    | None -> 0.
    | Some (track, v) -> computed_values.(track) /. v

  let percentage_to_pixels track_values computed_values =
    match first_non_zero (function
        | Pc x -> Some x
        | _ -> None) track_values with
    | None -> 0.
    | Some (track, v) -> computed_values.(track) /. v

  class type stylish =
    object
      method style : Dom_html.cssStyleDeclaration Js.t Js.prop
    end

  let get_matched_css_rules (elt : #Dom_html.element Js.t) : #stylish Js.t list =
    let stylesheets = (Js.Unsafe.coerce elt)##.ownerDocument##.styleSheets in
    let make_list coll =
      let rec aux acc = function
        | n when n < 0 -> acc
        | n ->
          let v = coll##item n in
          aux (v :: acc) (pred n) in
      aux [] coll##.length in
    List.filter (fun x ->
        try Element.matches elt (Js.to_string x##.selectorText)
        with _ -> false)
    @@ List.concat
    @@ List.map (fun x ->
        try make_list x##.cssRules
        with _ -> [])
    @@ make_list stylesheets

  let get_styles (rule : string) (elt : Dom_html.element Js.t) =
    let rule = Js.string rule in
    let matched = get_matched_css_rules elt in
    let get_style x = Js.Unsafe.get x##.style rule in
    let styles = get_style elt :: (List.map get_style matched) in
    List.filter_map (fun (x : Js.js_string Js.t Js.optdef) ->
        Js.Optdef.case x
          (fun () -> None)
          (fun x -> match Js.to_string x with
             | "" -> None
             | s -> Some s))
      styles

  let gen_cells ~f ~rows ~cols =
    let rec gen_rows acc row =
      let rec gen_cols acc col =
        if col = 0 then acc
        else
          let elt = f ~col ~row () in
          gen_cols (elt :: acc) (pred col) in
      if row = 0 then acc
      else gen_rows (gen_cols acc cols) (pred row) in
    gen_rows [] rows

  let is_merge_possible (_cells : Dom_html.element Js.t list) : bool =
    (* TODO implement *)
    (* XXX Merge is only possible when a group of cells forms a rectangle
       and all cells belong to the same parent grid. *)
    true
end

class t
    ?drag_interval
    ?(snap_offset = 0.)
    ?(min_size_start = 20.)
    ?(min_size_end = 20.)
    ?(on_cell_insert = fun _ _ -> ())
    (elt : Dom_html.element Js.t) () = object(self)
  inherit Widget.t elt () as super

  val mutable _listeners = []
  val mutable _move_listeners = []

  method! destroy () : unit =
    self#stop_move_listeners ();
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    List.iter Lwt.cancel _move_listeners;
    _move_listeners <- [];
    super#destroy ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ mousedowns super#root (fun e t ->
              match e##.button with
              | 0 -> self#handle_drag_start (e :> Dom_html.event Js.t) t
              | _ -> Lwt.return_unit)
        ; touchstarts super#root (fun e ->
              self#handle_drag_start (e :> Dom_html.event Js.t))
        ]);
    super#initial_sync_with_dom ()

  method empty : bool = match self#cells with [] -> true | _ -> false

  method rows : value array =
    Array.map value_of_string
    @@ self#raw_tracks super#root Row

  method cols : value array =
    Array.map value_of_string
    @@ self#raw_tracks super#root Col

  method add_column_before ?size (cell : Dom_html.element Js.t) : unit =
    self#add_row_or_column ?size ~before:true Col cell

  method add_column_after (cell : Dom_html.element Js.t) : unit =
    self#add_row_or_column ~before:false Col cell

  method add_row_before (cell : Dom_html.element Js.t) : unit =
    self#add_row_or_column ~before:true Row cell

  method add_row_after (cell : Dom_html.element Js.t) : unit =
    self#add_row_or_column ~before:false Row cell

  method remove_row (cell : Dom_html.element Js.t) : unit =
    self#remove_row_or_column Row cell

  method remove_column (cell : Dom_html.element Js.t) : unit =
    self#remove_row_or_column Col cell

  method reset
      ?(col_size = Fr 1.)
      ?(row_size = Fr 1.)
      ~(cols : int)
      ~(rows : int)
      () =
    Element.remove_children super#root;
    self#set_style super#root Col (gen_template ~size:col_size cols);
    self#set_style super#root Row (gen_template ~size:row_size rows);
    List.iter (fun x ->
        let elt = Tyxml_js.To_dom.of_element x in
        Element.append_child super#root elt;
        on_cell_insert self elt)
    @@ Util.gen_cells
      ~f:(fun ~col ~row () -> Markup.create_cell (make_cell_position ~col ~row ()))
      ~cols ~rows

  method insert_table
      ?(col_size = Fr 1.)
      ?(row_size = Fr 1.)
      ~(cols : int)
      ~(rows : int)
      (cell : Dom_html.element Js.t) : unit =
    let subgrid = Element.query_selector cell Selector.grid in
    Option.iter (Element.remove_child_safe cell) subgrid;
    let content = [] in (* TODO implement *)
    let grid =
      Tyxml_js.To_dom.of_element
      @@ Markup.create
        ~cols:(`Repeat (cols, col_size))
        ~rows:(`Repeat (rows, row_size))
        ~content
        () in
    Element.append_child cell grid

  method merge (cells : Dom_html.element Js.t list) : Dom_html.element Js.t option =
    match cells with
    | [] | [_] -> None
    | x :: tl ->
      (* FIXME consider different grids *)
      let { col; row; col_span; row_span } = Util.get_cell_position x in
      let col, col_end, row, row_end =
        List.fold_left (fun (cs, ce, rs, re) cell ->
            let { col; row; col_span; row_span } = Util.get_cell_position cell in
            min col cs, max (col + col_span) ce,
            min row rs, max (row + row_span) re)
          (col, col + col_span, row, row + row_span) tl in
      let position =
        { col
        ; row
        ; col_span = col_end - col
        ; row_span = row_end - row
        } in
      let (merged : Dom_html.element Js.t) =
        Tyxml_js.To_dom.of_element
        @@ Markup.create_cell position in
      Element.append_child super#root merged;
      List.iter (Element.remove_child_safe super#root) cells;
      on_cell_insert self merged;
      Some merged

  method cells' ?include_subgrids
      ?(grid = super#root)
      () : Dom_html.element Js.t list =
    self#cells_ ?include_subgrids grid

  method cells : Dom_html.element Js.t list =
    self#cells_ super#root

  (* Private methods *)

  method private cells_ ?(include_subgrids = true) grid : Dom_html.element Js.t list =
    match include_subgrids with
    | true -> Element.query_selector_all grid Selector.cell
    | false ->
      List.filter (fun x -> Element.has_class x CSS.cell)
      @@ Element.children grid

  method private clear_styles_ () : unit =
    self#set_style super#root Col "";
    self#set_style super#root Row ""

  method private remove_row_or_column
      (direction : direction)
      (cell : Dom_html.element Js.t) : unit =
    let grid = Util.get_parent_grid cell in
    let ({ col; row; _ } : cell_position) = Util.get_cell_position cell in
    let tracks = Array.to_list @@ self#raw_tracks grid direction in
    let n = match direction with Col -> col | Row -> row in
    (* Update positions and remove cells *)
    List.iter (fun cell ->
        let { col; row; col_span; row_span } = Util.get_cell_position cell in
        let n' = match direction with Col -> col | Row -> row in
        if n' > n
        then begin match direction with
          | Col -> Util.set_cell_col ~span:col_span (pred n') cell
          | Row -> Util.set_cell_row ~span:row_span (pred n') cell
        end
        else if n' = n
        then Element.remove_child_safe grid cell)
    @@ self#cells' ~include_subgrids:false ~grid ();
    (* Update style *)
    let style =
      String.concat " "
      @@ Util.remove_at_idx (n - 1) tracks in
    self#set_style grid direction style;
    if self#empty then self#clear_styles_ ()

  method private add_row_or_column
      ?(size = Fr 1.)
      ?(before = false)
      (direction : direction)
      (cell : Dom_html.element Js.t) : unit =
    let grid = Util.get_parent_grid cell in
    let ({ col; row; _ } : cell_position) = Util.get_cell_position cell in
    let tracks = Array.to_list @@ self#raw_tracks grid direction in
    (* Opposite tracks -
       rows if a column is being added,
       columns if a row is being added *)
    let opposite_tracks =
      self#track_values_px grid
        (match direction with Col -> Row | Row -> Col) in
    let n = match direction with
      | Col -> if before then col else succ col
      | Row -> if before then row else succ row in
    (* Update positions of existing elements *)
    List.iter (fun cell ->
        let { col; row; col_span; row_span } = Util.get_cell_position cell in
        match direction with
        | Col -> if col >= n then Util.set_cell_col ~span:col_span (succ col) cell
        | Row -> if row >= n then Util.set_cell_row ~span:row_span (succ row) cell)
    @@ self#cells' ~include_subgrids:false ~grid ();
    (* Add new items to each of the opposite tracks *)
    Array.iteri (fun i _ ->
        let col, row = match direction with
          | Row -> succ i, n
          | Col -> n, succ i in
        let (elt : Dom_html.element Js.t) =
          Tyxml_js.To_dom.of_element
          @@ Markup.create_cell (make_cell_position ~col ~row ()) in
        Element.append_child grid elt;
        on_cell_insert self elt) opposite_tracks;
    let style =
      String.concat " "
      @@ Util.insert_at_idx (n - 1) (value_to_string size) tracks in
    self#set_style grid direction style

  method private notify_input () : unit =
    super#emit ~detail:(new%js Js.array_empty) Event.Typ.input

  method private notify_change () : unit =
    super#emit ~detail:(new%js Js.array_empty) Event.Typ.change

  method private handle_drag_start (e : Dom_html.event Js.t) _ : unit Lwt.t =
    let target = Dom_html.eventTarget e in
    let cell = Util.cell_of_event (self#cells_ super#root) e in
    let direction = Element.(
        if has_class target CSS.col_handle then Some `Col
        else if has_class target CSS.row_handle then Some `Row
        else if has_class target CSS.mul_handle then Some `Mul
        else None) in
    match direction, cell with
    | None, _ | _, None -> Lwt.return_unit
    | Some direction, Some cell ->
      Dom_html.stopPropagation e;
      Dom.preventDefault e;
      let grid = Util.get_parent_grid target in
      let ({ col; row; _ } : cell_position) = Util.get_cell_position cell in
      let cells = self#cells' ~include_subgrids:false ~grid () in
      let first_cells, row, col = match direction with
        | `Row ->
          let first_cell = Util.find_first_cell Row row cells in
          Element.add_class first_cell CSS.cell_dragging_row;
          [first_cell], Some (self#get_dimensions ~col ~row grid Row), None
        | `Col ->
          let first_cell = Util.find_first_cell Col col cells in
          Element.add_class first_cell CSS.cell_dragging_column;
          [first_cell], None, Some (self#get_dimensions ~col ~row grid Col)
        | `Mul ->
          let col_cell = Util.find_first_cell Col col cells in
          let row_cell = Util.find_first_cell Row row cells in
          Element.add_class col_cell CSS.cell_dragging_column;
          Element.add_class row_cell CSS.cell_dragging_row;
          [col_cell; row_cell],
          Some (self#get_dimensions ~col ~row grid Col),
          Some (self#get_dimensions ~col ~row grid Row) in
      let resize_properties =
        { grid
        ; cell
        ; row
        ; col
        ; first_cells
        } in
      let wnd = Dom_html.window in
      let coerce e = (e :> Dom_html.event Js.t) in
      Js.Opt.case (Dom_html.CoerceTo.mouseEvent e)
        (fun () ->
           Dom.preventDefault e;
           _move_listeners <- Events.(
               [ touchmoves wnd (self#handle_drag resize_properties % coerce)
               ; touchends wnd (self#handle_drag_stop resize_properties % coerce)
               ; touchcancels wnd (self#handle_drag_stop resize_properties % coerce)
               ]))
        (fun _ ->
           Dom.preventDefault e;
           _move_listeners <- Events.(
               [ mousemoves wnd (self#handle_drag resize_properties % coerce)
               ; mouseups wnd (self#handle_drag_stop resize_properties % coerce)
               ]));
      Lwt.return_unit

  method private handle_drag props (e : Dom_html.event Js.t) _ : unit Lwt.t =
    Dom_html.stopPropagation e;
    Dom.preventDefault e;
    List.iter (function
        | None -> ()
        | Some x ->
          self#update_position props.grid (Util.get_cursor_position e) x;
          self#notify_input ();)
      [props.col; props.row];
    Lwt.return_unit

  method private handle_drag_stop props _ _ : unit Lwt.t =
    List.iter (fun cell ->
        Element.remove_class cell CSS.cell_dragging_column;
        Element.remove_class cell CSS.cell_dragging_row)
      props.first_cells;
    self#stop_move_listeners ();
    self#notify_change ();
    Lwt.return_unit

  method private stop_move_listeners () : unit =
    List.iter Lwt.cancel _move_listeners;
    _move_listeners <- []

  method private set_style grid direction (style : string) : unit =
    let v = Js.string style in
    match direction with
    | Row -> (Js.Unsafe.coerce grid##.style)##.gridTemplateRows := v
    | Col -> (Js.Unsafe.coerce grid##.style)##.gridTemplateColumns := v

  method private get_dimensions ~col ~row grid direction : dimensions =
    let start = match direction with
      | Row -> grid##getBoundingClientRect##.top
      | Col -> grid##getBoundingClientRect##.left in
    let tracks = self#raw_tracks grid direction in
    let track_values = Array.map value_of_string tracks in
    let track_values_px = self#track_values_px grid direction in
    let gap = self#gap grid direction in
    let a_track, b_track = match direction with
      | Col -> col - 2, col - 1
      | Row -> row - 2, row - 1 in
    let a_track_start =
      start
      +. Util.get_size_at_track
        ~gap
        (Array.sub track_values_px 0 a_track) in
    let b_track_end =
      start
      +. Util.get_size_at_track
        ~gap
        (Array.sub track_values_px 0 (succ b_track)) in
    { a_track
    ; b_track
    ; a_track_start
    ; b_track_end
    ; direction
    ; total_fr = Array.fold_left (fun acc -> function
          | Fr _ -> succ acc | _ -> acc) 0 track_values
    ; percentage_to_pixels = Util.percentage_to_pixels track_values track_values_px
    ; fr_to_pixels = Util.fr_to_pixels track_values track_values_px
    ; tracks
    ; track_values
    ; track_values_px
    ; gap
    }

  method private update_position grid position (dimensions : dimensions) =
    let position = match dimensions.direction with
      | Col -> float_of_int @@ fst position
      | Row -> float_of_int @@ snd position in
    let min_position =
      dimensions.a_track_start
      +. min_size_start
      +. dimensions.gap in
    let max_position =
      dimensions.b_track_end
      -. min_size_end
      -. dimensions.gap in
    let min_position_offset = min_position +. snap_offset in
    let max_position_offset = max_position -. snap_offset in
    let position =
      position
      |> (fun x -> if x < min_position_offset then min_position else x)
      |> (fun x -> if x > max_position_offset then max_position else x)
      |> max min_position
      |> min max_position in
    let a_track_size =
      position
      -. dimensions.a_track_start
      -. dimensions.gap in
    let b_track_size =
      dimensions.b_track_end
      -. position
      -. dimensions.gap in
    let drag_interval = match drag_interval with
      | None -> 1.
      | Some Fr x -> x *. dimensions.fr_to_pixels
      | Some Px x -> x
      | Some Pc x -> x *. dimensions.percentage_to_pixels
      | Some Auto -> fail "`auto` interval is not supported" in
    let a_track_size, b_track_size =
      if drag_interval > 1.
      then
        let a_track_size_interleaved =
          Js.math##round (a_track_size /. drag_interval) *. drag_interval in
        a_track_size_interleaved,
        b_track_size -. (a_track_size_interleaved -. a_track_size)
      else a_track_size, b_track_size in
    self#adjust_position
      ~a_track_size
      ~b_track_size
      dimensions;
    let style = String.concat " " @@ Array.to_list dimensions.tracks in
    self#set_style grid dimensions.direction style

  method private adjust_position
      ~a_track_size
      ~b_track_size
      ({ a_track
       ; b_track
       ; tracks
       ; track_values
       ; total_fr
       ; fr_to_pixels
       ; percentage_to_pixels
       ; _ } : dimensions) =
    let update_value track track_size =
      match track_values.(track) with
      | Px _ -> Px track_size
      | Fr _ ->
        (match total_fr with
         | 1 -> Fr 1.
         | _ -> Fr (track_size /. fr_to_pixels))
      | Pc _ -> Pc (track_size /. percentage_to_pixels)
      | x -> x in
    let a_track_value = update_value a_track a_track_size in
    let b_track_value = update_value b_track b_track_size in
    tracks.(a_track) <- value_to_string a_track_value;
    tracks.(b_track) <- value_to_string b_track_value

  method private raw_tracks grid direction : string array =
    let prop = match direction with
      | Col -> "grid-template-columns"
      | Row -> "grid-template-rows" in
    let tracks = Util.get_styles prop grid in
    if List.length tracks = 0
    then [||]
    else Array.of_list @@ String.split_on_char ' ' @@ List.hd tracks

  method private track_values_px grid direction: float array =
    let style = Dom_html.window##getComputedStyle grid in
    let v = match direction with
      | Row -> (Js.Unsafe.coerce style)##.gridTemplateRows
      | Col -> (Js.Unsafe.coerce style)##.gridTemplateColumns in
    Js.Optdef.case v
      (fun () -> [||])
      (Array.of_list
       % List.map (fun (x : string) ->
           match value_of_string x with
           | Px v -> v
           | _ -> fail @@ Printf.sprintf "failed to parse value (%s)" x)
       % String.split_on_char ' '
       % Js.to_string)

  method private gap grid direction : float =
    let style = Dom_html.window##getComputedStyle grid in
    let v = match direction with
      | Row -> (Js.Unsafe.coerce style)##.gridRowGap
      | Col -> (Js.Unsafe.coerce style)##.gridColumnGap in
    Js.Optdef.case v
      (fun () -> 0.)
      (fun x ->
         let s = Js.to_string x in
         match value_of_string_opt s with
         | Some Px v -> v
         | _ -> 0.)

end

let make ?drag_interval
    ?snap_offset
    ?min_size_start
    ?min_size_end
    ?on_cell_insert
    () =
  let elt = Dom_html.(createDiv document) in
  new t ?drag_interval ?snap_offset
    ?min_size_start
    ?min_size_end
    ?on_cell_insert
    elt
    ()

let attach ?drag_interval ?snap_offset ?min_size_start ?min_size_end
    ?on_cell_insert
    (elt : Dom_html.element Js.t) : t =
  new t ?drag_interval ?snap_offset ?min_size_start ?min_size_end
    ?on_cell_insert elt ()
