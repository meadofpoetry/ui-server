open Js_of_ocaml
open Js_of_ocaml_lwt
open Components

let ( >>= ) = Lwt.bind

type t =
  { callback : Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t
  ; name : string
  ; icon : string
  }

let make ~name ~icon ~callback () =
  { callback
  ; name
  ; icon
  }

let make_icon_button action =
  let icon =
    Icon_button.make
      ~classes:[Top_app_bar.CSS.action_item]
      ~on_click:(fun e -> action.callback (e :> Dom_html.event Js.t))
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
    (actions : t list) =
  let actions, menu_items =
    List.split
    @@ List.map (fun x ->
        make_icon_button x,
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
    ?on_navigation_icon_click
    (scaffold : Scaffold.t) =
  match scaffold#top_app_bar with
  | None -> fun () -> ()
  | Some x ->
    let prev_nav_icon_click = scaffold#on_navigation_icon_click in
    let prev_title = x#title in
    let prev_actions = x#actions in
    Utils.Option.iter scaffold#set_on_navigation_icon_click on_navigation_icon_click;
    Utils.Option.iter x#set_title title;
    Utils.Option.iter x#add_class class_;
    x#set_actions @@ List.map Widget.root actions;
    List.iter Widget.layout actions;
    (fun () ->
       (match prev_nav_icon_click with
        | None -> scaffold#set_on_navigation_icon_click_default ()
        | Some f -> scaffold#set_on_navigation_icon_click f);
       x#set_title prev_title;
       x#set_actions prev_actions;
       Utils.Option.iter x#remove_class class_)

module Undo = struct

  let undo (undo_manager : Undo_manager.t) =
    make ~callback:(fun _ _ -> Undo_manager.undo undo_manager; Lwt.return_unit)
      ~name:"Отменить"
      ~icon:Icon.SVG.Path.undo
      ()

  let redo (undo_manager : Undo_manager.t) =
    make ~callback:(fun _ _ -> Undo_manager.redo undo_manager; Lwt.return_unit)
      ~name:"Повторить"
      ~icon:Icon.SVG.Path.redo
      ()

end

module Container_actions = struct
  include Undo

  let wizard (wizard : Wizard.t) (grid : Grid.t) =
    make ~callback:(fun _ _ ->
        wizard#open_await ()
        >>= function
        | Close | Destroy | Custom _ -> Lwt.return_unit
        | Accept ->
          ignore wizard#value;
          (* TODO implement *)
          Lwt.return_unit)
      ~name:"Мастер"
      ~icon:Icon.SVG.Path.auto_fix
      ()

  let make_menu (wizard_widget : Wizard.t) undo_manager (grid : Grid.t) =
    make_overflow_menu
      [ Undo.undo undo_manager
      ; Undo.redo undo_manager
      ; wizard wizard_widget grid
      ]
end

module Container_selected_actions = struct

  let make_description_dialog () =
    let input = Textfield.make_textfield ~label:"Наименование" Text in
    let title =
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ Dialog.Markup.create_title_simple "Описание" () in
    let content =
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ Dialog.Markup.create_content [input#markup] () in
    let actions = Dialog.(
        [ make_action ~label:"Отмена" ~action:Close ()
        ; make_action ~label:"ОК" ~action:Accept ()
        ]) in
    input, Dialog.make ~title ~content ~actions ()

  let description ~get_selected
      ((input : string Textfield.t), (dialog : Dialog.t)) =
    make ~callback:(fun _ _ ->
        match get_selected () with
        | [cell] ->
          let title = Container_utils.get_cell_title cell in
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
             then Container_utils.set_cell_title cell value';
             Lwt.return_unit)
        | _ -> Lwt.return_unit)
      ~name:"Описание"
      ~icon:Icon.SVG.Path.information
      ()

  let edit ~get_selected ~edit_container =
    make ~callback:(fun _ _ ->
        match get_selected () with
        | [cell] -> edit_container cell
        | _ -> Lwt.return_unit)
      ~name:"Редактировать"
      ~icon:Icon.SVG.Path.pencil
      ()

  let make_menu ~get_selected ~edit_container () =
    let dialog = make_description_dialog () in
    let menu =
      make_overflow_menu
        [ edit ~get_selected ~edit_container
        ; description ~get_selected dialog
        ] in
    Dom.appendChild Dom_html.document##.body (snd dialog)#root;
    menu#set_on_destroy (fun () ->
        Element.remove_child_safe Dom_html.document##.body (snd dialog)#root;
        (snd dialog)#destroy ());
    menu

end

module Cell_selected_actions = struct

  let merge ~on_remove ~get_selected undo_manager grid =
    make ~callback:(fun _ _ ->
        let cells = get_selected () in
        match grid#merge cells with
        | None -> Lwt.return_unit
        | Some merged ->
          on_remove ();
          let v =
            { Undo_manager.
              undo = (fun () ->
                  Dom.removeChild grid#root merged;
                  List.iter (Dom.appendChild grid#root) cells)
            ; redo = (fun () ->
                  List.iter (Dom.removeChild grid#root) cells;
                  Dom.appendChild grid#root merged)
            } in
          Undo_manager.add undo_manager v;
          Lwt.return_unit)
      ~name:"Объединить ячейки"
      ~icon:Icon.SVG.Path.table_merge_cells
      ()

  let get_cell' f = function
    | [] -> invalid_arg "list is empty"
    | x :: tl ->
      snd @@ List.fold_left (fun ((pos', _) as acc) x ->
          let pos = Grid.Util.get_cell_position x in
          if f pos' pos then (pos, x) else acc)
        (Grid.Util.get_cell_position x, x) tl

  let get_topmost_cell cells =
    get_cell' (fun acc pos -> pos.row < acc.row) cells

  let get_bottommost_cell cells =
    get_cell' (fun acc pos -> pos.row > acc.row) cells

  let get_leftmost_cell cells =
    get_cell' (fun acc pos -> pos.col < acc.col) cells

  let get_rightmost_cell cells =
    get_cell' (fun acc pos -> pos.col > acc.col) cells

  let add_row_above ~get_selected grid =
    make ~callback:(fun _ _ ->
        match get_selected () with
        | [] -> Lwt.return_unit
        | cells ->
          let cell = get_topmost_cell cells in
          grid#add_row_before cell;
          Lwt.return_unit)
      ~name:"Добавить ряд сверху"
      ~icon:Icon.SVG.Path.table_row_plus_before
      ()

  let add_row_below ~get_selected grid =
    make ~callback:(fun _ _ ->
        match get_selected () with
        | [] -> Lwt.return_unit
        | cells ->
          let cell = get_bottommost_cell cells in
          grid#add_row_after cell;
          Lwt.return_unit)
      ~name:"Добавить ряд снизу"
      ~icon:Icon.SVG.Path.table_row_plus_after
      ()

  let remove_row ~on_remove ~get_selected grid =
    make ~callback:(fun _ _ ->
        let cells = get_selected () in
        let _, cells =
          List.split
          @@ List.fold_left (fun acc x ->
              let pos = Grid.Util.get_cell_position x in
              if List.mem_assoc pos.row acc
              then acc else (pos.row, x) :: acc)
            [] cells in
        List.iter grid#remove_row cells;
        on_remove ();
        Lwt.return_unit)
      ~name:"Удалить ряд"
      ~icon:Icon.SVG.Path.table_row_remove
      ()

  let add_col_left ~get_selected (grid : Grid.t) =
    make ~callback:(fun _ _ ->
        match get_selected () with
        | [] -> Lwt.return_unit
        | cells ->
          grid#add_column_before @@ get_leftmost_cell cells;
          Lwt.return_unit)
      ~name:"Добавить столбец слева"
      ~icon:Icon.SVG.Path.table_column_plus_before
      ()

  let add_col_right ~get_selected grid =
    make ~callback:(fun _ _ ->
        match get_selected () with
        | [] -> Lwt.return_unit
        | cells ->
          grid#add_column_after @@ get_rightmost_cell cells;
          Lwt.return_unit)
      ~name:"Добавить столбец справа"
      ~icon:Icon.SVG.Path.table_column_plus_after
      ()

  let remove_col ~on_remove ~get_selected grid =
    make ~callback:(fun _ _ ->
        let cells = get_selected () in
        let _, cells =
          List.split
          @@ List.fold_left (fun acc x ->
              let pos = Grid.Util.get_cell_position x in
              if List.mem_assoc pos.col acc
              then acc else (pos.col, x) :: acc)
            [] cells in
        List.iter grid#remove_column cells;
        on_remove ();
        Lwt.return_unit)
      ~name:"Удалить столбец"
      ~icon:Icon.SVG.Path.table_column_remove
      ()

  let make_menu
      ~clear_selection
      ~get_selected
      ~empty_placeholder
      undo_manager
      (grid : Grid.t) =
    let on_remove () =
      clear_selection ();
      if grid#empty
      then Dom.appendChild grid#root empty_placeholder#root
      else Element.remove_child_safe grid#root empty_placeholder#root in
    make_overflow_menu
      [ merge ~on_remove ~get_selected undo_manager grid
      ; add_row_above ~get_selected grid
      ; add_row_below ~get_selected grid
      ; remove_row ~on_remove ~get_selected grid
      ; add_col_left ~get_selected grid
      ; add_col_right ~get_selected grid
      ; remove_col ~on_remove ~get_selected grid
      ]

end
