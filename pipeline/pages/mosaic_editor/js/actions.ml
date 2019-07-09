open Js_of_ocaml
open Js_of_ocaml_lwt
open Components
open Resizable_grid_utils

(* TODO
   [ ] Add cell split action
   [X] Add container info action
   [X] Add container edit action
   [ ] Add an ability to disable action
*)

type t =
  { callback : Dom_html.element Js.t list -> unit Lwt.t
  ; name : string
  ; icon : string
  }

let make ~name ~icon ~callback () =
  { callback
  ; name
  ; icon
  }

let ( >>= ) = Lwt.bind

let apply_opt_cb = function
  | None -> ()
  | Some f -> f ()

let undo (undo_manager : Undo_manager.t) =
  make ~callback:(fun _ -> Undo_manager.undo undo_manager; Lwt.return_unit)
    ~name:"Отменить"
    ~icon:Icon.SVG.Path.undo
    ()

let redo (undo_manager : Undo_manager.t) =
  make ~callback:(fun _ -> Undo_manager.redo undo_manager; Lwt.return_unit)
    ~name:"Повторить"
    ~icon:Icon.SVG.Path.redo
    ()

let wizard (grid : Resizable_grid.t) =
  make ~callback:(fun _ -> Lwt.return_unit)
    ~name:"Мастер"
    ~icon:Icon.SVG.Path.auto_fix
    ()

let description ((input : string Textfield.t), (dialog : Dialog.t)) =
  make ~callback:(function
      | [cell] ->
        let title = get_cell_title cell in
        input#set_value title;
        (dialog#open_await ()
         >>= function
         | Close | Destroy | Custom _ -> Lwt.return_unit
         | Accept ->
           (* TODO
              - Check if title is valid (empty, repeating, etc)
              - Change top app bar title *)
           let value' = input#value_as_string in
           if not (String.equal title value')
           then set_cell_title cell value';
           Lwt.return_unit)
      | _ -> Lwt.return_unit)
    ~name:"Описание"
    ~icon:Icon.SVG.Path.information
    ()

let edit f =
  make ~callback:(function
      | [cell] -> f cell
      | _ -> Lwt.return_unit)
    ~name:"Редактировать"
    ~icon:Icon.SVG.Path.pencil
    ()

let merge ?f
    (undo_manager : Undo_manager.t)
    (grid : Resizable_grid.t) =
  let action = fun cells ->
    apply_opt_cb f;
    grid#merge cells in
  make ~callback:(fun cells ->
      match action cells with
      | None -> Lwt.return_unit
      | Some merged ->
        let v =
          { Undo_manager.
            undo = (fun () ->
                Dom.removeChild grid#root merged;
                List.iter (Dom.appendChild grid#root) cells;
                apply_opt_cb f)
          ; redo = (fun () ->
                List.iter (Dom.removeChild grid#root) cells;
                Dom.appendChild grid#root merged;
                apply_opt_cb f)
          } in
        Undo_manager.add undo_manager v;
        Lwt.return_unit)
    ~name:"Объединить ячейки"
    ~icon:Icon.SVG.Path.table_merge_cells
    ()

let add_row_above (grid : Resizable_grid.t) =
  make ~callback:(function
      | [] -> Lwt.return_unit
      | cells ->
        let cell = get_topmost_cell cells in
        grid#add_row_before cell;
        Lwt.return_unit)
    ~name:"Добавить ряд сверху"
    ~icon:Icon.SVG.Path.table_row_plus_before
    ()

let add_row_below (grid : Resizable_grid.t) =
  make ~callback:(function
      | [] -> Lwt.return_unit
      | cells ->
        let cell = get_bottommost_cell cells in
        grid#add_row_after cell;
        Lwt.return_unit)
    ~name:"Добавить ряд снизу"
    ~icon:Icon.SVG.Path.table_row_plus_after
    ()

let remove_row ?f (grid : Resizable_grid.t) =
  make ~callback:(fun cells ->
      let _, cells =
        List.split
        @@ List.fold_left (fun acc x ->
            let pos = get_cell_position x in
            if List.mem_assoc pos.row acc
            then acc else (pos.row, x) :: acc)
          [] cells in
      List.iter grid#remove_row cells;
      apply_opt_cb f;
      Lwt.return_unit)
    ~name:"Удалить ряд"
    ~icon:Icon.SVG.Path.table_row_remove
    ()

let add_col_left (grid : Resizable_grid.t) =
  make ~callback:(function
      | [] -> Lwt.return_unit
      | cells ->
        grid#add_column_before @@ get_leftmost_cell cells;
        Lwt.return_unit)
    ~name:"Добавить столбец слева"
    ~icon:Icon.SVG.Path.table_column_plus_before
    ()

let add_col_right (grid : Resizable_grid.t) =
  make ~callback:(function
      | [] -> Lwt.return_unit
      | cells ->
        grid#add_column_after @@ get_rightmost_cell cells;
        Lwt.return_unit)
    ~name:"Добавить столбец справа"
    ~icon:Icon.SVG.Path.table_column_plus_after
    ()

let remove_col ?f (grid : Resizable_grid.t) =
  make ~callback:(fun cells ->
      let _, cells =
        List.split
        @@ List.fold_left (fun acc x ->
            let pos = get_cell_position x in
            if List.mem_assoc pos.col acc
            then acc else (pos.col, x) :: acc)
          [] cells in
      List.iter grid#remove_column cells;
      apply_opt_cb f;
      Lwt.return_unit)
  ~name:"Удалить столбец"
  ~icon:Icon.SVG.Path.table_column_remove
  ()

let add_widget (scaffold : Scaffold.t) =
  make ~callback:(fun _ ->
      match scaffold#side_sheet with
      | None -> Lwt.return_unit
      | Some sidesheet -> sidesheet#toggle ())
    ~name:"Добавить виджет"
    ~icon:Icon.SVG.Path.plus
    ()

let make_icon_button get_selected action =
  let icon =
    Icon_button.make
      ~on_click:(fun _ _ -> action.callback @@ get_selected ())
      ~icon:(Icon.SVG.make_simple action.icon)#root
      () in
  icon#set_attribute "title" action.name;
  icon

let make_menu_item action =
  Item_list.Item.make
    ~role:"menuitem"
    ~graphic:Icon.SVG.(make_simple action.icon) (* FIXME *)
    action.name

let make_overflow_menu
    ?body
    ?viewport
    (get_selected : unit -> Dom_html.element Js.t list)
    (actions : t list) =
  let actions, menu_items =
    List.split
    @@ List.map (fun x ->
        make_icon_button get_selected x,
        make_menu_item x)
      actions in
  let menu = Menu.make_of_item_list ?body ?viewport (Item_list.make menu_items) in
  let overflow = Icon_button.make
      ~icon:Icon.SVG.(make_simple Path.dots_vertical)#root
      () in
  let listener =
    Lwt_js_events.seq_loop
      (Lwt_js_events.make_event Menu.Event.selected)
      menu#root
      (fun e _ ->
         let detail = Widget.event_detail e in
         let button = List.nth actions detail##.index in
         Js.Opt.iter (Dom_html.CoerceTo.button button#root)
           (fun (button : Dom_html.buttonElement Js.t) -> button##click);
         Lwt.return_unit) in
  menu#set_on_destroy (fun () -> Lwt.cancel listener);
  Overflow_menu.make
    ~actions:(List.map Widget.root actions)
    ~overflow:overflow#root
    ~menu
    ()


(** Switches top app bar between contextual action
    mode and normal mode *)
let transform_top_app_bar
    ?(actions = [])
    ?(title : string option)
    ?(class_ : string option)
    (scaffold : Scaffold.t) =
  match scaffold#top_app_bar with
  | None -> fun () -> ()
  | Some x ->
    let prev_title = x#title in
    let prev_actions = x#actions in
    Utils.Option.iter x#set_title title;
    Utils.Option.iter x#add_class class_;
    x#set_actions @@ List.map Widget.root actions;
    List.iter Widget.layout actions;
    (fun () ->
       scaffold#set_on_navigation_icon_click_default ();
       List.iter Widget.destroy actions;
       x#set_title prev_title;
       x#set_actions prev_actions;
       Utils.Option.iter x#remove_class class_)
