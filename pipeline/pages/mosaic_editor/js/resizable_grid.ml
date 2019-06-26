open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Resizable_grid_utils

(* Inspired by
 * https://github.com/nathancahill/split
 * https://grid.layoutit.com/
*)

module CSS = Markup.CSS.Container_grid
module Markup = Markup.Container_grid

module Selector = struct
  let grid = Printf.sprintf ".%s" CSS.root
  let cell = Printf.sprintf ".%s" CSS.cell
end

module Event = struct
  class type item =
    object
      method item : Dom_html.element Js.t
      method rect : Dom_html.clientRect Js.t Js.readonly_prop
    end

  type detail = item Js.t Js.js_array Js.t

  class type selected = [Dom_html.element Js.t] Widget.custom_event

  class type resize = [detail] Widget.custom_event

  let (selected : selected Js.t Events.Typ.t) =
    Events.Typ.make "resizable-grid:selected"

  let (resize : resize Js.t Events.Typ.t) =
    Events.Typ.make "resizable-grid:resize"
end

type direction =
  | Col
  | Row [@@deriving show]

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

let fail s =
  failwith @@ Printf.sprintf "resizable-grid: %s" s

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
  } [@@deriving show]

type resize_properties =
  { cell : Dom_html.element Js.t
  ; grid : Dom_html.element Js.t
  ; col : dimensions option
  ; row : dimensions option
  }

class t
    ?drag_interval
    ?(snap_offset = 0.)
    ?(min_size_start = 20.)
    ?(min_size_end = 20.)
    (elt : Dom_html.element Js.t) () = object(self)
  inherit Widget.t elt () as super

  val mutable _listeners = []
  val mutable _move_listeners = []

  method! destroy () : unit =
    self#stop_move_listeners ();
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ mousedowns super#root (fun e t ->
              match e##.button with
              | 0 -> self#handle_drag_start (Mouse e) t
              | _ -> Lwt.return_unit)
        ; touchstarts super#root (fun e ->
              self#handle_drag_start (Touch e))
        ]);
    super#initial_sync_with_dom ()

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

  method split
      ?(col_size = `Fr 1.)
      ?(row_size = `Fr 1.)
      ~(cols : int)
      ~(rows : int)
      (cell : Dom_html.element Js.t) : unit =
    let subgrid = Element.query_selector cell Selector.grid in
    Utils.Option.iter (Element.remove_child_safe cell) subgrid;
    let rec gen_rows acc row =
      let rec gen_cols acc col =
        if col = 0 then acc
        else
          let elt = Markup.create_cell ~col ~row () in
          gen_cols (elt :: acc) (pred col) in
      if row = 0 then acc
      else gen_rows (gen_cols acc cols) (pred row)
    in
    let rec loop f acc = function
      | 0 -> acc
      | n -> loop f ((f n) :: acc) (pred n) in
    let grid =
      Tyxml_js.To_dom.of_element
      @@ Markup.create ~content:(gen_rows [] rows) () in
    let col_style = loop (fun _ -> value_to_string @@ col_size) [] cols in
    let row_style = loop (fun _ -> value_to_string @@ row_size) [] rows in
    self#set_style grid Col (String.concat " " @@ col_style);
    self#set_style grid Row (String.concat " " @@ row_style);
    Element.append_child cell grid

  method cells ?include_subgrids
      ?(grid = super#root)
      () : Dom_html.element Js.t list =
    self#cells_ ?include_subgrids grid

  (* Private methods *)

  method private cells_ ?(include_subgrids = true) grid : Dom_html.element Js.t list =
    match include_subgrids with
    | true ->
      Element.query_selector_all grid Selector.cell
    | false ->
      List.filter (fun x -> Element.has_class x CSS.cell)
      @@ Element.children grid

  method private remove_row_or_column
      (direction : direction)
      (cell : Dom_html.element Js.t) : unit =
    let grid = self#parent_grid cell in
    let col, row = self#get_cell_position cell in
    let tracks = Array.to_list @@ self#raw_tracks grid direction in
    let n = match direction with Col -> col | Row -> row in
    (* Update positions and remove cells *)
    List.iter (fun cell ->
        let col, row = self#get_cell_position cell in
        let n' = match direction with Col -> col | Row -> row in
        if n' > n
        then begin match direction with
          | Col -> set_cell_col cell (pred n')
          | Row -> set_cell_row cell (pred n')
        end
        else if n' = n
        then Element.remove_child_safe grid cell)
    @@ self#cells ~include_subgrids:false ~grid ();
    (* Update style *)
    let style =
      String.concat " "
      @@ remove_at_idx (n - 1) tracks in
    self#set_style grid direction style

  method private add_row_or_column
      ?(size = `Fr 1.)
      ?(before = false)
      (direction : direction)
      (cell : Dom_html.element Js.t) : unit =
    let grid = self#parent_grid cell in
    let col, row = self#get_cell_position cell in
    let tracks = Array.to_list @@ self#raw_tracks grid direction in
    (* Opposite tracks -
       rows if a column is being added,
       columns if a row is being added *)
    let opposite_tracks =
      self#track_values_px grid
        (match direction with Col -> Row | Row -> Col) in
    let n = match direction with
      | Col -> if before then max 1 (pred col) else succ col
      | Row -> if before then max 1 (pred row) else succ row in
    (* Update positions of existing elements *)
    List.iter (fun cell ->
        let col, row = self#get_cell_position cell in
        match direction with
        | Col -> if col >= n then set_cell_col cell (succ col)
        | Row -> if row >= n then set_cell_row cell (succ row))
    @@ self#cells ~include_subgrids:false ~grid ();
    (* Add new items to each of the opposite tracks *)
    Array.iteri (fun i _ ->
        let col, row = match direction with
          | Row -> succ i, n
          | Col -> n, succ i in
        let (elt : Dom_html.element Js.t) =
          Tyxml_js.To_dom.of_element
          @@ Markup.create_cell ~row ~col () in
        Element.append_child grid elt) opposite_tracks;
    let style =
      String.concat " "
      @@ insert_at_idx (n - 1) (value_to_string size) tracks in
    self#set_style grid direction style

  method private notify_change () : unit =
    ()

  method private notify_selected cell : unit =
    super#emit ~should_bubble:true ~detail:cell Event.selected

  method private handle_drag_start
      (e : event) _ : unit Lwt.t =
    Dom_html.stopPropagation (coerce_event e);
    Dom.preventDefault (coerce_event e);
    let target = Dom_html.eventTarget (coerce_event e) in
    let cell = cell_of_event (self#cells_ super#root) (coerce_event e) in
    (* FIXME this definetely may not be a cell, rewrite *)
    let direction =
      if Element.has_class target CSS.col_handle
      then Some `Col
      else if Element.has_class target CSS.row_handle
      then Some `Row
      else if Element.has_class target CSS.mul_handle
      then Some `Mul
      else None in
    match direction, cell with
    | _, None -> Lwt.return_unit
    | None, Some cell ->
      Events.mouseup super#root
      >>= fun _ -> self#notify_selected cell; Lwt.return_unit
    | Some direction, Some cell ->
      let grid = self#parent_grid target in
      let row, col = match direction with
        | `Row ->
          Element.add_class cell CSS.cell_dragging_row;
          Some (self#get_dimensions ~cell grid Row), None
        | `Col ->
          Element.add_class cell CSS.cell_dragging_column;
          None, Some (self#get_dimensions ~cell grid Col)
        | `Mul ->
          Element.add_class cell CSS.cell_dragging_row;
          Element.add_class cell CSS.cell_dragging_column;
          Some (self#get_dimensions ~cell grid Col),
          Some (self#get_dimensions ~cell grid Row) in
      let resize_properties = { grid; cell; row; col } in
      (match e with
       | Touch e ->
         Dom.preventDefault e;
         _move_listeners <- Events.(
             [ touchmoves Dom_html.window (fun e ->
                   self#handle_drag resize_properties (Touch e))
             ; touchends Dom_html.window (fun e ->
                   self#handle_drag_stop cell (Touch e))
             ; touchcancels Dom_html.window (fun e ->
                   self#handle_drag_stop cell (Touch e))
             ])
       | Mouse e ->
         Dom.preventDefault e;
         _move_listeners <- Events.(
             [ mousemoves Dom_html.window (fun e ->
                   self#handle_drag resize_properties (Mouse e))
             ; mouseups Dom_html.window (fun e ->
                   self#handle_drag_stop cell (Mouse e))
             ]));
      Lwt.return_unit

  method private handle_drag props (e : event) _ : unit Lwt.t =
    Dom_html.stopPropagation (coerce_event e);
    Dom.preventDefault (coerce_event e);
    List.iter (function
        | None -> ()
        | Some x ->
          self#update_position props.grid (get_cursor_position e) x)
      [props.col; props.row];
    Lwt.return_unit

  method private handle_drag_stop cell (e : event) _ : unit Lwt.t =
    Element.remove_class cell CSS.cell_dragging_column;
    Element.remove_class cell CSS.cell_dragging_row;
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

  method private get_dimensions ~cell grid direction : dimensions =
    let start = match direction with
      | Row -> grid##getBoundingClientRect##.top
      | Col -> grid##getBoundingClientRect##.left in
    let tracks = self#raw_tracks grid direction in
    let track_values = Array.map value_of_string tracks in
    let track_values_px = self#track_values_px grid direction in
    let gap = self#gap grid direction in
    let col, row = self#get_cell_position cell in
    let a_track, b_track = match direction with
      | Col -> col - 2, col - 1
      | Row -> row - 2, row - 1 in
    let a_track_start =
      start
      +. get_size_at_track
        ~gap
        (Array.sub track_values_px 0 a_track) in
    let b_track_end =
      start
      +. get_size_at_track
        ~gap
        (Array.sub track_values_px 0 (succ b_track)) in
    { a_track
    ; b_track
    ; a_track_start
    ; b_track_end
    ; direction
    ; total_fr = Array.fold_left (fun acc -> function
          | `Fr _ -> succ acc | _ -> acc) 0 track_values
    ; percentage_to_pixels = percentage_to_pixels track_values track_values_px
    ; fr_to_pixels = fr_to_pixels track_values track_values_px
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
      | Some `Fr x -> x *. dimensions.fr_to_pixels
      | Some `Px x -> x
      | Some `Pc x -> x *. dimensions.percentage_to_pixels in
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
      grid
      dimensions;
    let style = String.concat " " @@ Array.to_list dimensions.tracks in
    self#set_style grid dimensions.direction style

  method private adjust_position
      ~a_track_size
      ~b_track_size
      (grid : Dom_html.element Js.t)
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
      | `Px x -> `Px track_size
      | `Fr x ->
        (match total_fr with
         | 1 -> `Fr 1.
         | _ -> `Fr (track_size /. fr_to_pixels))
      | `Pc x -> `Pc (track_size /. percentage_to_pixels)
      | x -> x in
    let a_track_value = update_value a_track a_track_size in
    let b_track_value = update_value b_track b_track_size in
    tracks.(a_track) <- value_to_string a_track_value;
    tracks.(b_track) <- value_to_string b_track_value

  method private raw_tracks grid direction : string array =
    let prop = match direction with
      | Col -> "grid-template-columns"
      | Row -> "grid-template-rows" in
    let tracks = get_styles prop grid in
    if List.length tracks = 0
    then failwith "gutter: unable to determine grid template tracks from styles"
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
           | `Px v -> v
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
         | Some `Px v -> v
         | _ -> 0.)

  method private parent_grid (item : Dom_html.element Js.t) =
    let rec aux elt =
      if Element.equal super#root elt
      then elt
      else Js.Opt.case (Element.get_parent elt)
          (fun () -> fail "parent grid not found")
          (fun elt ->
             if Element.has_class elt CSS.root
             then elt else aux elt) in
    aux item

  method private get_cell_position (cell : Dom_html.element Js.t) : int * int =
    get_cell_position cell

end

let make ?drag_interval
    ?snap_offset
    ?min_size_start
    ?min_size_end
    () =
  let elt = Dom_html.(createDiv document) in
  new t ?drag_interval ?snap_offset
    ?min_size_start
    ?min_size_end
    elt
    ()

let attach ?drag_interval ?snap_offset ?min_size_start ?min_size_end
    (elt : Dom_html.element Js.t) : t =
  new t ?drag_interval ?snap_offset ?min_size_start ?min_size_end elt ()