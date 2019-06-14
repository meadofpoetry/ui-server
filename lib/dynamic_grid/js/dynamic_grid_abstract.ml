open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

include Dynamic_grid_types
include Dynamic_grid_overlay

module Option = Utils.Option
module Item = Dynamic_grid_item

type add_error = Collides of Position.t list

let ( % ) f g x = f (g x)

let to_grid ?max_col_width ?(min_col_width = 1)
      ?rows ?row_height
      ?(vertical_compact = false)
      ?(items_margin = (0, 0))
      ?(multi_select = false)
      ?(restrict_move = false)
      ?draggable ?resizable ?selectable ~cols () =
  { min_col_width
  ; max_col_width
  ; cols
  ; rows
  ; row_height
  ; vertical_compact
  ; items_margin
  ; multi_select
  ; restrict_move
  ; draggable
  ; resizable
  ; selectable
  }

class ['a, 'b, 'c] t ~grid
        ~(get : 'c -> 'a item)
        ~(items : 'a item list)
        () =
  let e_modify, e_modify_push = React.E.create () in
  let s_selected, s_selected_push = React.S.create ~eq:(fun _ _ -> false) [] in
  let s_col_w, s_col_w_push = React.S.create grid.min_col_width in
  let s_row_h = match grid.row_height with
    | Some rh -> React.S.const rh
    | None -> s_col_w in
  let s_grid, s_grid_push = React.S.create ~eq:equal_grid grid in
  let s_items =
    React.S.fold (fun acc -> function
        | `Add x -> x :: acc
        | `Remove x -> List.filter (not % Widget.equal x) acc)
      [] e_modify in
  let new_item item =
    new Item.t ~s_grid ~s_items ~s_selected ~s_selected_push
      ~s_col_w ~s_row_h ~item () in
  let items = List.map (fun item -> new_item item) items in
  let s_change =
    let m a x = x :: a in
    React.S.map ~eq:(==)
      (fun l -> React.S.merge m [] (List.map (fun x -> x#s_change) l)) s_items
    |> React.S.switch in
  let s_changing =
    let m a x = x :: a in
    React.S.map ~eq:(==)
      (fun l -> React.S.merge m [] (List.map (fun x -> x#s_changing) l)) s_items
    |> React.S.switch in
  let s_rows =
    React.S.map ~eq:(==)
      (fun grid ->
        match grid.rows with
        | Some h -> React.S.const h
        | None -> let merge = (fun acc (x:Position.t) ->
                      if (x.h + x.y) > acc then (x.h + x.y) else acc) in
                  React.S.map (fun (l:Position.t list) -> List.fold_left merge 1 l)
                    s_changing)
      s_grid
    |> React.S.switch in
  let elt = Tyxml_js.To_dom.of_element @@ Markup.create ~items:[] () in

  object(self)

    constraint 'b = 'a #Item.t

    inherit Widget.t elt () as super

    val _overlay_grid =
      new overlay_grid
        ~parent:elt
        ~s_col_w
        ~s_row_h
        ~s_cols:(React.S.map (fun g -> g.cols) s_grid)
        ~s_rows
        ~s_im:(React.S.map (fun g -> g.items_margin) s_grid)
        ()
    val _s_selected = React.S.map (fun x -> x) s_selected
    val _e_selected = React.S.changes s_selected

    (** API *)

    method! init () : unit =
      (* FIXME save state *)
      super#init ();
      let remove_listener =
        Events.listen_lwt super#root Item.remove_event (fun e _ ->
            let item = (Js.Unsafe.coerce e)##.detail in
            begin match List.find_opt (fun x -> x#root == item) self#items with
            | None -> ()
            | Some i -> e_modify_push (`Remove i)
            end;
            Lwt.return_unit) in
      Lwt.ignore_result remove_listener;
      React.S.map (fun _ -> self#layout ()) s_grid |> ignore;
      (* add item add/remove listener *)
      React.E.map (fun action ->
          (match action with
           | `Add (x : 'b) -> self#append_child x
           | `Remove x -> self#remove_child x);
          (* FIXME make vertical compact variable *)
          if self#grid.vertical_compact then self#compact ()) e_modify
      |> ignore;
      (* add initial items *)
      List.iter (fun x -> e_modify_push (`Add x)) items;
      (* add min/max width update listener *)
      React.S.map (fun (grid : grid) ->
          let m_top = snd grid.items_margin in
          self#root##.style##.minWidth := Utils.px_js (grid.cols * grid.min_col_width + m_top);
          Option.iter (fun x -> self#root##.style##.maxWidth := Utils.px_js @@ grid.cols * x + m_top)
            grid.max_col_width)
        s_grid
      |> ignore;
      (* add height update listener *)
      React.S.l3 (fun h row_h grid ->
          let my = snd grid.items_margin in
          self#root##.style##.height := Js.string @@ Utils.px (h * row_h + my))
        s_rows s_row_h s_grid
      |> ignore;
      Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ ->
          self#layout (); true) |> ignore;

    method! destroy () : unit =
      super#destroy ();
      List.iter (fun x -> x#destroy ()) self#items;
      self#remove_all ()

    method! layout () : unit =
      super#layout ();
      (match Js.Opt.to_option @@ self#root##.parentNode with
       | None -> ()
       | Some p ->
          let w = (Js.Unsafe.coerce p)##.offsetWidth in
          let w = if w < self#grid.cols then self#grid.cols else w in
          let mx = fst self#item_margin in
          let col = (w - mx) / self#grid.cols in
          let col = if col < mx + 1 then mx + 1 else col in
          s_col_w_push col;
          self#root##.style##.width := Utils.px_js (col * self#grid.cols + mx);
          _overlay_grid#show_dividers ();
          _overlay_grid#layout ())

    method s_changing : Position.t list React.signal =
      s_changing

    method s_change : Position.t list React.signal =
      s_change

    method s_items : 'b list React.signal =
      s_items

    method s_selected : 'b list React.signal =
      _s_selected

    method e_selected : 'b list React.event  =
      _e_selected

    method grid : grid =
      React.S.value s_grid

    method items : 'b list =
      Position.sort_by_y ~f:(fun x -> x#pos) @@ React.S.value s_items

    method positions : Position.t list =
      React.S.value s_change

    method item_margin : int * int =
      self#grid.items_margin

    method set_item_margin (x : int * int) =
      s_grid_push { self#grid with items_margin = x }

    method overlay_grid : Dynamic_grid_overlay.overlay_grid =
      _overlay_grid

    method add (x : 'c) : ('a Item.t, add_error) result =
      let (x : 'a item) = get x in
      let items = List.map (fun x -> x#pos) (React.S.value s_items) in
      match Position.get_all_collisions ~f:(fun x -> x) x.pos items with
      | [] ->
        let item = new_item x in
        e_modify_push (`Add item);
        Ok item
      | l -> print_endline "error: collides"; Error (Collides l)

    method draggable : bool option =
      self#grid.draggable

    method set_draggable (x : bool option) : unit =
      s_grid_push { self#grid with draggable = x }

    method resizable : bool option =
      self#grid.resizable

    method set_resizable (x : bool option) : unit =
      s_grid_push { self#grid with resizable = x }

    method selectable : bool option =
      self#grid.selectable

    method set_selectable (x : bool option) : unit =
      s_grid_push { self#grid with selectable = x }

    method remove (item : 'b) : unit =
      match List.find_opt (Widget.equal item) self#items with
      | None -> ()
      | Some x ->
         x#set_selected false;
         self#remove_child x;
         x#destroy ()

    method clear_selection () : unit =
      List.iter (fun x -> x#set_selected false)
      @@ React.S.value s_selected

    method remove_all () : unit =
      List.iter (fun x -> e_modify_push (`Remove x)) self#items;
      s_selected_push []

    (* Private methods *)

    method private s_grid = s_grid
    method private s_col_w = s_col_w
    method private s_row_h = s_row_h
    method private s_rows = s_rows
    method private s_grid_push = s_grid_push

    method private get_event_pos : Dom_html.mouseEvent Js.t -> Position.t option =
      fun e ->
      let rect = super#root##getBoundingClientRect in
      let x, y =
        e##.clientX - (int_of_float rect##.left),
        e##.clientY - (int_of_float rect##.top) in
      if x <= super#root##.offsetWidth
      && x >= 0
      && y <= super#root##.offsetHeight && y >= 0
      then Some { x; y; w = 1; h = 1 } else None

    method private compact () : unit =
      let other i = List.filter (not % Widget.equal i) self#items in
      List.iter (fun x ->
          x#set_pos
          @@ Position.compact ~f:(fun x -> x#pos)
               x#pos
               (other x)) self#items

  end
