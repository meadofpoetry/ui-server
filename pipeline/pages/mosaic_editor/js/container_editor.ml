open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pipeline_types
open Types

(* TODO
   [ ] undo/redo
   [X] selection (single/multi with selection area)
   [ ] editing mode selection: table, containers
   [X] overflow menu
   [ ] contextual actions
*)

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

module Selector = struct
  let grid_wrapper = Printf.sprintf ".%s" Card.CSS.media
  let cell = Printf.sprintf ".%s" Resizable_grid.CSS.cell
  let grid = Printf.sprintf ".%s" Resizable_grid.CSS.grid
  let actions = Printf.sprintf ".%s" Card.CSS.actions
end

module Event = struct
  class type selected = [Dom_html.element Js.t Js.js_array Js.t] Widget.custom_event

  let (selected : selected Js.t Dom_html.Event.typ) =
    Dom_html.Event.make "container-editor:selected"
end

class t ?(containers = [])
    ~resolution
    ~(scaffold : Scaffold.t)
    elt () = object(self)
  val grid_wrapper = match Element.query_selector elt Selector.grid_wrapper with
    | None -> failwith "container-editor: grid wrapper element not found"
    | Some x -> x
  val actions = match Element.query_selector elt Selector.actions with
    | None -> failwith "container-editor: actions element not found"
    | Some x -> x
  val grid = match Element.query_selector elt Selector.grid with
    | None -> failwith "container-editor: grid element not found"
    | Some x -> Resizable_grid.attach ~drag_interval:(Fr 0.05) x

  val mutable _containers : (string * Wm.container) list = containers
  val mutable _listeners = []
  val mutable _focused_item = None
  val mutable _toolbar = Widget.create_div ()
  val mutable _selection = None
  val mutable _actions : action list = []

  inherit Widget.t elt () as super

  method! init () : unit =
    let class_ = Resizable_grid.CSS.cell_selected in
    _selection <- Some (
        Selection.make
          ~validate_start:(fun e ->
              match Js.to_string e##._type with
              | "mousedown" ->
                let (e : Dom_html.mouseEvent Js.t) = Js.Unsafe.coerce e in
                e##.button = 0
              | _ -> true)
          ~selectables:[Query Selector.cell]
          ~start_areas:[Node grid_wrapper]
          ~boundaries:[Node grid_wrapper]
          ~on_start:(fun { selected; selection; _ } ->
              List.iter (fun x -> Element.remove_class x class_) selected;
              selection#deselect_all ())
          ~on_move:(fun { selected; removed; _ } ->
              List.iter (fun x -> Element.add_class x class_) selected;
              List.iter (fun x -> Element.remove_class x class_) removed)
          ~on_stop:(fun { selection; selected; _ } ->
              selection#keep_selection ();
              self#notify_selected ())
          ~on_select:(fun item { selection; selected; _ } ->
              let is_selected = Element.has_class item class_ in
              List.iter (fun x -> Element.remove_class x class_) selected;
              selection#deselect_all ();
              if is_selected
              then (Element.remove_class item class_;
                    selection#deselect item)
              else (Element.add_class item class_;
                    selection#keep_selection ());
              self#notify_selected ())
          ());
    grid#reset ~rows:5 ~cols:5 ();
    List.iter (Element.append_child actions % Widget.root) @@ self#create_grid_actions ();
    _actions <- self#create_actions ();
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ keydowns super#root self#handle_keydown
        ]);
    super#initial_sync_with_dom ()

  method! layout () : unit =
    let parent_width, parent_height =
      grid_wrapper##.offsetWidth, grid_wrapper##.offsetHeight in
    let width = parent_height * (fst resolution) / (snd resolution) in
    grid#root##.style##.width := Utils.px_js width;
    grid#root##.style##.height := Utils.px_js parent_height;
    let force = parent_width - 2 > width in
    grid#toggle_class ~force Resizable_grid.CSS.grid_bordered;
    super#layout ()

  method! destroy () : unit =
    Utils.Option.iter (fun x -> x#destroy ()) _selection;
    _selection <- None;
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method selected : Dom_html.element Js.t list =
    match _selection with
    | None -> []
    | Some x -> x#selected

  method clear_selection () : unit =
    match _selection with
    | None -> ()
    | Some x ->
      List.iter (fun x ->
          Element.remove_class x Resizable_grid.CSS.cell_selected)
        x#selected;
      x#deselect_all ()

  method toolbar = _toolbar

  method actions : action list = _actions

  (* Private methods *)

  method private notify_selected () : unit =
    let detail = Js.array @@ Array.of_list self#selected in
    super#emit ~detail Event.selected

  method private create_actions () : action list =
    let merge =
      { callback = (fun () -> grid#merge self#selected)
      ; name = "Объединить ячейки"
      ; icon = Icon.SVG.Path.table_merge_cells
      ; context = Selected
      } in
    let add_row_above =
      { callback = (fun () -> grid#add_row_before (List.hd self#selected))
      ; name = "Добавить ряд сверху"
      ; icon = Icon.SVG.Path.table_row_plus_before
      ; context = All
      } in
    let add_row_below =
      { callback = (fun () -> grid#add_row_after (List.hd self#selected))
      ; name = "Добавить ряд снизу"
      ; icon = Icon.SVG.Path.table_row_plus_after
      ; context = All
      } in
    let remove_row =
      { callback = (fun () -> grid#remove_row (List.hd self#selected))
      ; name = "Удалить ряд"
      ; icon = Icon.SVG.Path.table_row_remove
      ; context = Selected
      } in
    let add_col_left =
      { callback = (fun () -> grid#add_column_before (List.hd self#selected))
      ; name = "Добавить столбец слева"
      ; icon = Icon.SVG.Path.table_column_plus_before
      ; context = All
      } in
    let add_col_right =
      { callback = (fun () -> grid#add_column_after (List.hd self#selected))
      ; name = "Добавить столбец справа"
      ; icon = Icon.SVG.Path.table_column_plus_after
      ; context = All
      } in
    let remove_col =
      { callback = (fun () -> grid#remove_column (List.hd self#selected))
      ; name = "Удалить столбец"
      ; icon = Icon.SVG.Path.table_column_remove
      ; context = Selected
      } in
    [ merge
    ; add_row_above
    ; add_row_below
    ; remove_row
    ; add_col_left
    ; add_col_right
    ; remove_col
    ]

  method private create_grid_actions () =
    let undo = Icon_button.make
        ~icon:Icon.SVG.(make_simple Path.undo)#root
        () in
    let redo = Icon_button.make
        ~icon:Icon.SVG.(make_simple Path.redo)#root
        () in
    let icons =
      Card.Actions.make_icons
        [ undo
        ; redo
        ] in
    let submit = Button.make ~label:"Применить" () in
    let buttons = Card.Actions.make_buttons [submit] in
    [icons; buttons]

  method private handle_keydown _ _ : unit Lwt.t =
    (* TODO implement *)
    Lwt.return_unit

end

let make ~(scaffold : Scaffold.t) (wm : Wm.t) =
  let cells = Resizable_grid_utils.gen_cells
      ~f:(fun ~col ~row () -> Resizable_grid.Markup.create_grid_cell
             ~col_start:col ~row_start:row ())
      ~cols:5
      ~rows:5 in
  let grid = Resizable_grid.Markup.create_grid
      ~rows:5
      ~cols:5
      ~content:cells
      () in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Resizable_grid.Markup.create
      ~classes:[Card.CSS.root]
      ~width:(fst wm.resolution)
      ~height:(snd wm.resolution)
      ~grid () in
  new t
    ~resolution:wm.resolution
    ~containers:wm.layout
    ~scaffold
    elt ()
