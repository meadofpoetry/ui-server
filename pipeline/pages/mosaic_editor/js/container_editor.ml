open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js
open Components
open Pipeline_types
open Resizable_grid_utils

(* TODO
   [ ] undo/redo
   [X] container selection (single/multi with selection area)
   [ ] widget selection
   [X] editing mode selection: table, containers
   [X] overflow menu
   [X] contextual actions
   [ ] keyboard navigation for widgets/containers
   [ ] add description to tiles
   [ ] add switching between widget/container mode
   [ ] simple wizard
   [ ] `add` widget ui
   [ ] `add` container ui (predefined)
   [ ] container table split ui
   [ ] migrate to 'selection' module in widget editor
   [ ] Take aspect ratio into account (if any) while resizing widget
   [X] Respect parent boundaries when resizing/moving widgets
   [X] Show helper alignment lines in widgets editor
   [X] Stick widget to neighbour elements
   [ ] Extend widget `resize` dir to handle Top, Left, Right, Bottom dirs
*)

include Page_mosaic_editor_tyxml.Container_editor
module Markup = Make(Xml)(Svg)(Html)

type editing_mode = Table | Content

type event =
  [ `Layout of Wm.t
  ]

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

let flip f x y = f y x

module Selector = struct
  let grid_wrapper = Printf.sprintf ".%s" Card.CSS.media
  let cell = Printf.sprintf ".%s" Resizable_grid.CSS.cell
  let grid = Printf.sprintf ".%s" Resizable_grid.CSS.root
  let actions = Printf.sprintf ".%s" Card.CSS.actions
  let mode_switch = Printf.sprintf ".%s" CSS.mode_switch
end

module Selection = struct
  include Selection

  let class_ = Resizable_grid.CSS.cell_selected

  let selectables = [Query Selector.cell]

  let validate_start = fun e ->
    match Js.to_string e##._type with
    | "mousedown" ->
      let (e : Dom_html.mouseEvent Js.t) = Js.Unsafe.coerce e in
      e##.button = 0
    | _ -> true

  let on_start = fun { selected; selection; _ } ->
    List.iter (fun x -> Element.remove_class x class_) selected;
    selection#deselect_all ()

  let on_move = fun { selected; removed; _ } ->
    List.iter (fun x -> Element.add_class x class_) selected;
    List.iter (fun x -> Element.remove_class x class_) removed

  let on_stop handle_selected = fun { selection; selected; _ } ->
    selection#keep_selection ();
    handle_selected selection#selected

  let on_select handle_selected = fun item { selection; _ } ->
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
    handle_selected selection#selected

  let make handle_selected elt =
    let boundaries = [Node elt] in
    Selection.make ~validate_start
      ~selectables
      ~boundaries
      ~start_areas:boundaries
      ~on_start
      ~on_move
      ~on_stop:(on_stop handle_selected)
      ~on_select:(on_select handle_selected)
      ()

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
  val mutable _drag_target = Js.null

  val mutable _selection = None
  val mutable _resize_observer = None
  val mutable _top_app_bar_context = None
  val mutable _edit_mode = Table

  val mutable _basic_actions = None
  val mutable _cells_selected_actions = None

  val mutable _widget_editor = None
  val mutable _mode_switch = None

  inherit Widget.t elt () as super

  method! init () : unit =
    _resize_observer <- Some (self#create_resize_observer ());
    _selection <- Some (Selection.make self#handle_selected grid_wrapper);
    let basic_actions = self#create_actions () in
    let cells_selected_actions = self#create_selected_actions () in
    _basic_actions <- Some basic_actions;
    _cells_selected_actions <- Some cells_selected_actions;
    _mode_switch <- begin match Element.query_selector elt Selector.mode_switch with
      | None -> failwith "container-editor: mode switch element not found"
      | Some x ->
        let tab_bar =
          Tab_bar.attach
            ~on_change:(fun _ tab_bar ->
                match tab_bar#active_tab_index with
                | None -> assert false
                | Some 0 -> self#switch_mode Table
                | Some _ -> self#switch_mode Content)
            x in
        Some tab_bar
    end;
    (match scaffold#top_app_bar with
     | None -> ()
     | Some x -> x#set_actions @@ List.map Widget.root [basic_actions]);
    (* FIXME *)
    List.iter (Element.append_child actions % Widget.root) @@ self#create_grid_actions ();
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ keydowns super#root self#handle_keydown
        ; dragstarts grid#root self#handle_dragstart
        ; dragends grid#root self#handle_dragend
        ; dragenters grid#root self#handle_dragenter
        ; dragleaves grid#root self#handle_dragleave
        ; dragovers grid#root self#handle_dragover
        ; drops grid#root self#handle_drop
        ]);
    super#initial_sync_with_dom ()

  method! layout () : unit =
    let parent_h = grid_wrapper##.offsetHeight in
    let width = parent_h * (fst resolution) / (snd resolution) in
    grid#root##.style##.width := Utils.px_js width;
    grid#root##.style##.height := Utils.px_js parent_h;
    Utils.Option.iter Widget.layout _basic_actions;
    Utils.Option.iter Widget.layout _cells_selected_actions;
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
    self#selection#selected

  method clear_selection () : unit =
    List.iter (flip Element.remove_class Selection.class_) self#selected;
    self#selection#deselect_all ()

  method notify : event -> unit = function
    | `Layout wm ->
      match _widget_editor with
      | None -> ()
      | Some (id, editor) ->
        match List.assoc_opt id wm.layout with
        | None -> () (* FIXME container lost, handle it somehow *)
        | Some x -> editor#notify @@ `Container x

  (* Private methods *)

  method private handle_dragstart e _ =
    let target = Dom_html.eventTarget e in
    _drag_target <- e##.target;
    e##.dataTransfer##setData (Js.string "text/html") target##.innerHTML;
    e##.dataTransfer##.effectAllowed := Js.string "move";
    Element.add_class target CSS.cell_dragging;
    Lwt.return_unit

  method private handle_dragenter e _ =
    let target = Dom_html.eventTarget e in
    if Element.has_class target Resizable_grid.CSS.cell
    && (Js.Opt.case _drag_target (fun () -> true) (not % Element.equal target))
    then Element.add_class target CSS.cell_dragover;
    Lwt.return_unit

  method private handle_dragleave e _ =
    let target = Dom_html.eventTarget e in
    Element.remove_class target CSS.cell_dragover;
    Lwt.return_unit

  method private handle_dragover e _ =
    let target = Dom_html.eventTarget e in
    if Element.has_class target Resizable_grid.CSS.cell
    then (Dom.preventDefault e;
          e##.dataTransfer##.dropEffect := Js.string "move");
    Lwt.return_unit

  method private handle_drop e _ =
    print_endline "drop";
    Dom_html.stopPropagation e;
    let target = Dom_html.eventTarget e in
    Js.Opt.iter _drag_target (fun dragged ->
        if not @@ Element.equal dragged target
        then (
          let id, html = dragged##.id, dragged##.innerHTML in
          dragged##.id := target##.id;
          dragged##.innerHTML := target##.innerHTML;
          target##.id := id;
          target##.innerHTML := html));
    Lwt.return_unit

  method private handle_dragend e _ =
    let target = Dom_html.eventTarget e in
    Element.remove_class target CSS.cell_dragging;
    List.iter (flip Element.remove_class CSS.cell_dragover) @@ grid#cells ();
    Lwt.return_unit

  (* TODO implement *)
  method private handle_keydown e _ : unit Lwt.t =
    (match Dom_html.Keyboard_code.of_event e with
     | Escape -> begin match self#selected with
         | [] -> ()
         | _ -> self#clear_selection ()
       end
     | _ -> ());
    Lwt.return_unit

  method private restore_top_app_bar_context () : unit =
    Utils.Option.iter (fun f -> f (); _top_app_bar_context <- None)
      _top_app_bar_context

  method private handle_selected = function
    | [] -> self#restore_top_app_bar_context ()
    | cells ->
      let title = match _edit_mode, cells with
        | Content, [cell] -> Printf.sprintf "Выбрана ячейка" (* FIXME *)
        | _ ->
          Printf.sprintf "Выбрано ячеек: %d"
          @@ List.length cells in
      let restore = Container_editor_actions.transform_top_app_bar
          ~class_:Page_mosaic_editor_tyxml.CSS.top_app_bar_contextual
          ~actions:(match _edit_mode with
              | Table -> [Utils.Option.get _cells_selected_actions]
              | Content -> [])
          ~title
          scaffold in
      match _top_app_bar_context with
      | Some _ -> ()
      | None ->
        scaffold#set_on_navigation_icon_click (fun _ _ ->
            self#clear_selection ();
            self#restore_top_app_bar_context ();
            Lwt.return_unit);
        _top_app_bar_context <- Some restore

  method private create_actions () : Overflow_menu.t =
    let open Container_editor_actions in
    make_overflow_menu (fun () -> self#selected) scaffold [wizard grid]

  (* TODO consider inner grids *)
  method private create_selected_actions () : Overflow_menu.t =
  let open Container_editor_actions in
    let f () =
      self#clear_selection ();
      self#restore_top_app_bar_context () in
    make_overflow_menu
      (fun () -> self#selected)
      scaffold
      [ merge ~f grid
      ; add_row_above grid
      ; add_row_below grid
      ; remove_row ~f grid
      ; add_col_left grid
      ; add_col_right grid
      ; remove_col ~f grid
      ]

  method private create_grid_actions () =
    let make_icon path = Icon.SVG.(make_simple path)#root in
    let undo = Icon_button.make ~icon:(make_icon Icon.SVG.Path.undo) () in
    let redo = Icon_button.make ~icon:(make_icon Icon.SVG.Path.redo) () in
    let icons = Card.Actions.make_icons [undo; redo] in
    let submit = Button.make ~label:"Применить" () in
    let buttons = Card.Actions.make_buttons [submit] in
    [icons; buttons]

  method private edit_container (cell : Dom_html.element Js.t) : unit =
    ()

  method private create_resize_observer () =
    let f = fun _ -> self#layout () in
    Ui_templates.Resize_observer.observe ~f ~node:super#root ()

  method private selection : Selection.t =
    Utils.Option.get _selection

  method private switch_mode (mode : editing_mode) : unit Lwt.t =
    self#clear_selection ();
    self#restore_top_app_bar_context ();
    _edit_mode <- mode;
    (match mode with
     | Table ->
       self#selection#set_disabled false;
       super#remove_class CSS.content_mode
     | Content ->
       self#selection#set_disabled true;
       super#add_class CSS.content_mode);
    Lwt.return_unit

end

let make
    ~(scaffold : Scaffold.t)
    (wm : Wm.t) =
  let cells = Resizable_grid_utils.gen_cells
      ~f:(fun ~col ~row () ->
          let content =
            Markup.create_cell_description
              ~content:[Html.txt @@ Printf.sprintf "%dx%d" col row]
              () in
          Resizable_grid.Markup.create_cell
            ~col_start:col
            ~row_start:row
            ~content:[content]
            ())
      ~cols:5
      ~rows:5 in
  let grid = Resizable_grid.Markup.create
      ~rows:5
      ~cols:5
      ~content:cells
      () in
  let elt =
    To_dom.of_element
    @@ Markup.create
      ~classes:[Card.CSS.root]
      ~width:(fst wm.resolution)
      ~height:(snd wm.resolution)
      ~grid () in
  new t
    ~resolution:wm.resolution
    ~containers:wm.layout
    ~scaffold
    elt ()
