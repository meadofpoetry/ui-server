open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pipeline_types
open Resizable_grid_utils
open Types

(* TODO
   [ ] undo/redo
   [X] selection (single/multi with selection area)
   [ ] editing mode selection: table, containers
   [X] overflow menu
   [X] contextual actions
   [ ] keyboard navigation
   [ ] add description to tiles
   [ ] add switching between widget/container mode
*)

type event =
  [ `Layout of Wm.t
  ]

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

(** Switches top app bar between contextual action
    mode and normal mode *)
let transform_top_app_bar
    ?(actions = [])
    ?(title : string option)
    ~(class_ : string)
    (scaffold : Scaffold.t) =
  match scaffold#top_app_bar with
  | None -> fun () -> ()
  | Some x ->
    let prev_title = x#title in
    let prev_actions = x#actions in
    Utils.Option.iter x#set_title title;
    x#add_class class_;
    x#set_actions @@ List.map Widget.root actions;
    List.iter Widget.layout actions;
    (fun () ->
       scaffold#set_on_navigation_icon_click_default ();
       List.iter Widget.destroy actions;
       x#set_title prev_title;
       x#set_actions prev_actions;
       x#remove_class class_)

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
  val mutable _selection = None
  val mutable _selected_actions = None
  val mutable _top_app_bar_context = None
  val mutable _widget_editor = None
  val mutable _resize_observer = None

  inherit Widget.t elt () as super

  method! init () : unit =
    let class_ = Resizable_grid.CSS.cell_selected in
    _resize_observer <- Some (
        Ui_templates.Resize_observer.observe
          ~f:(fun _ -> self#layout ())
          ~node:super#root
          ());
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
              self#handle_selected ())
          ~on_select:(fun item { selection; _ } ->
              let is_selected = Element.has_class item class_ in
              List.iter (fun x -> Element.remove_class x class_) selection#selected;
              (match List.length selection#selected > 1
                     && List.memq item selection#selected with
               | true ->
                 selection#deselect_all ();
                 selection#keep_selection ();
                 Element.add_class item class_
               | _ ->
                 selection#deselect_all ();
                 if is_selected
                 then (Element.remove_class item class_; selection#deselect item)
                 else (Element.add_class item class_; selection#keep_selection ()));
              self#handle_selected ())
          ());
    grid#reset ~rows:5 ~cols:5 ();
    _selected_actions <- Some (self#create_selected_actions ());
    List.iter (Element.append_child actions % Widget.root)
    @@ self#create_grid_actions ();
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
    super#layout ()

  method! destroy () : unit =
    Utils.Option.iter Ui_templates.Resize_observer.disconnect _resize_observer;
    _resize_observer <- None;
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

  method notify : event -> unit = function
    | `Layout wm ->
      (match _widget_editor with
       | None -> ()
       | Some (id, editor) ->
         match List.assoc_opt id wm.layout with
         | None -> () (* FIXME container lost, handle it somehow *)
         | Some x -> editor#notify @@ `Container x)

  (* Private methods *)

  method private restore_top_app_bar_context () : unit =
    match _top_app_bar_context with
    | None -> ()
    | Some f -> f (); _top_app_bar_context <- None

  (* TODO implement *)
  method private handle_keydown e _ : unit Lwt.t =
    (match Dom_html.Keyboard_code.of_event e with
     | Escape ->
       begin match self#selected with
         | [] -> ()
         | _ -> self#clear_selection ()
       end
     | _ -> ());
    Lwt.return_unit

  method private handle_selected () : unit =
    match self#selected with
    | [] -> self#restore_top_app_bar_context ()
    | _ ->
      let title =
        Printf.sprintf "Выбрано ячеек: %d"
        @@ List.length self#selected in
      let restore = transform_top_app_bar
          ~class_:Page_mosaic_editor_tyxml.CSS.top_app_bar_contextual
          ~actions:[Utils.Option.get _selected_actions]
          ~title
          scaffold in
      (match _top_app_bar_context with
       | Some _ -> ()
       | None ->
         scaffold#set_on_navigation_icon_click (fun _ _ ->
             self#clear_selection ();
             self#restore_top_app_bar_context ();
             Lwt.return_unit);
         _top_app_bar_context <- Some restore)

  method private create_overflow_menu actions : Overflow_menu.t =
    let actions, menu_items =
      List.split
      @@ List.map (fun x ->
          icon_button_of_action x,
          menu_item_of_action x)
        actions in
    let menu = Menu.make_of_item_list
        ~body:scaffold#app_content_inner
        ~viewport:(Element scaffold#app_content_inner)
        (Item_list.make menu_items) in
    let overflow = Icon_button.make
        ~icon:Icon.SVG.(make_simple Path.dots_vertical)#root
        () in
    Overflow_menu.make
      ~actions:(List.map Widget.root actions)
      ~overflow:overflow#root
      ~menu
      ()

  (* TODO consider inner grids *)
  method private create_selected_actions () : Overflow_menu.t =
    let merge =
      { callback = (fun () ->
            grid#merge self#selected;
            self#clear_selection ();
            self#handle_selected ())
      ; name = "Объединить ячейки"
      ; icon = Icon.SVG.Path.table_merge_cells
      } in
    let add_row_above =
      { callback = (fun () -> match self#selected with
            | [] -> ()
            | cells -> grid#add_row_before @@ get_topmost_cell cells)
      ; name = "Добавить ряд сверху"
      ; icon = Icon.SVG.Path.table_row_plus_before
      } in
    let add_row_below =
      { callback = (fun () -> match self#selected with
            | [] -> ()
            | cells -> grid#add_row_after @@ get_bottommost_cell cells)
      ; name = "Добавить ряд снизу"
      ; icon = Icon.SVG.Path.table_row_plus_after
      } in
    let remove_row =
      { callback = (fun () ->
            let _, cells =
              List.split
              @@ List.fold_left (fun acc x ->
                  let pos = get_cell_position x in
                  if List.mem_assoc pos.row acc
                  then acc else (pos.row, x) :: acc)
                [] self#selected in
            List.iter grid#remove_row cells;
            self#clear_selection ();
            self#handle_selected ())
      ; name = "Удалить ряд"
      ; icon = Icon.SVG.Path.table_row_remove
      } in
    let add_col_left =
      { callback = (fun () -> match self#selected with
            | [] -> ()
            | cells -> grid#add_column_before @@ get_leftmost_cell cells)
      ; name = "Добавить столбец слева"
      ; icon = Icon.SVG.Path.table_column_plus_before
      } in
    let add_col_right =
      { callback = (fun () -> match self#selected with
            | [] -> ()
            | cells -> grid#add_column_after @@ get_rightmost_cell cells)
      ; name = "Добавить столбец справа"
      ; icon = Icon.SVG.Path.table_column_plus_after
      } in
    let remove_col =
      { callback = (fun () ->
            let _, cells =
              List.split
              @@ List.fold_left (fun acc x ->
                  let pos = get_cell_position x in
                  if List.mem_assoc pos.col acc
                  then acc else (pos.col, x) :: acc)
                [] self#selected in
            List.iter grid#remove_column cells;
            self#clear_selection ();
            self#handle_selected ())
      ; name = "Удалить столбец"
      ; icon = Icon.SVG.Path.table_column_remove
      } in
    self#create_overflow_menu
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

  method private edit_container (cell : Dom_html.element Js.t) : unit =
    ()

end

let make
    ~(scaffold : Scaffold.t)
    (wm : Wm.t) =
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
