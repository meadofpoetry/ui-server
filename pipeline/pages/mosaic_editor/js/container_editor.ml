open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pipeline_types
open Container_utils

let name = "container-editor"

let failwith s = failwith @@ Printf.sprintf "%s: %s" name s

type event =
  [ `Layout of Wm.Annotated.t
  | `Wizard of Wm.t
  | `Streams of Structure.Annotated.t ]

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

module Selector = struct
  let content = Printf.sprintf ".%s" Card.CSS.media

  let actions = Printf.sprintf ".%s" Card.CSS.actions

  let cell = Printf.sprintf ".%s" Grid.CSS.cell

  let grid = Printf.sprintf ".%s" Grid.CSS.root

  let widget_wrapper = Printf.sprintf ".%s" CSS.widget_wrapper
end

module Selection = struct
  include Components_lab.Selection

  let class_ = Grid.CSS.cell_selected

  let selectables = [Query Selector.cell]

  let before_start selection {original_event = e; selected; _} =
    let is_multiple =
      match selected with
      | [x] when Element.equal x (Dom.eventTarget e) -> false
      | _ -> true
    in
    selection#set_multiple is_multiple;
    if is_multiple then Dom.preventDefault e;
    Js.Opt.case
      (Dom_html.CoerceTo.mouseEvent e)
      (fun () -> true)
      (fun e -> e##.button = 0)

  let on_start (selection : t) _ =
    match selection#selected with
    | [] -> ()
    | [x] -> Element.remove_class x class_
    | selected ->
        List.iter (fun x -> Element.remove_class x class_) selected;
        selection#clear_selection ()

  let on_move _ {removed; added; _} =
    List.iter (Fun.flip Element.add_class class_) added;
    List.iter (Fun.flip Element.remove_class class_) removed

  let on_stop handle_selected selection {selected; _} =
    (match selection#selected with
    | [x] when not @@ List.memq x selected ->
        selection#remove_from_selection x;
        Element.remove_class x class_
    | [x] -> Element.add_class x class_
    | _ -> ());
    selection#keep_selection ();
    handle_selected selection#selected

  let make handle_selected elt =
    let boundaries = [Node elt] in
    make
      ~before_start
      ~selectables
      ~boundaries
      ~start_areas:boundaries
      ~on_start
      ~on_move
      ~on_stop:(on_stop handle_selected)
      ~on_outside_click:(fun selection _ ->
        List.iter (Fun.flip Element.remove_class class_) selection#selected;
        selection#clear_selection ();
        handle_selected [])
      ()
end

let on_cell_insert (grid : Grid.t) (cell : Dom_html.element Js.t) =
  match get_cell_title cell with
  | "" -> set_cell_title cell (gen_cell_title grid#cells)
  | _ -> ()

let init_top_app_bar_icon (scaffold : Scaffold.t) =
  match Option.bind scaffold#top_app_bar (fun x -> x#leading) with
  | None -> ()
  | Some x -> (
      let icons = Element.query_selector_all x @@ Printf.sprintf ".%s" Icon.CSS.root in
      match icons with
      | [x] -> Element.add_class x CSS.nav_icon_main
      | [x; y] ->
          Element.add_class x CSS.nav_icon_main;
          Element.add_class y CSS.nav_icon_aux
      | _ -> ())

let set_top_app_bar_icon (scaffold : Scaffold.t) typ icon =
  let class_, index =
    match typ with
    | `Main -> CSS.nav_icon_main, 0
    | `Aux -> CSS.nav_icon_aux, 1
  in
  match Option.bind scaffold#top_app_bar (fun x -> x#leading) with
  | None -> None
  | Some x ->
      let prev = Element.query_selector x @@ Printf.sprintf ".%s" class_ in
      (match prev with
      | None -> ()
      | Some prev -> Dom.removeChild x prev);
      Element.add_class icon class_;
      Element.insert_child_at_index x index icon;
      prev

let filter_available_widgets
    (widgets : (string * Wm.widget) list)
    (wm : (string * Wm.widget) list list) : (string * Wm.widget) list =
  List.fold_left
    (fun acc container ->
      List.filter (fun (id, _) -> not @@ List.mem_assoc id container) acc)
    widgets
    wm

type widget_mode_state =
  { icon : Dom_html.element Js.t option
  ; restore : unit -> unit
  ; editor : Widget_editor.t
  ; cell : Dom_html.element Js.t }

class t
  ~(scaffold : Scaffold.t)
  (structure : Structure.Annotated.t)
  (wm : Wm.Annotated.t)
  (elt : Dom_html.element Js.t) =
  object (self)
    val s_state = React.S.create []

    val close_icon : Dom_html.element Js.t =
      Tyxml_js.To_dom.of_element @@ Icon.SVG.(Markup_js.create_of_d Path.close)

    val back_icon =
      Tyxml_js.To_dom.of_element @@ Icon.SVG.(Markup_js.create_of_d Path.arrow_left)

    val grid : Grid.t =
      match Element.query_selector elt Selector.grid with
      | None -> failwith "grid element not found"
      | Some x -> Grid.attach ~on_cell_insert ~drag_interval:(Fr 0.05) x

    val table_dialog = UI.add_table_dialog ()

    val wizard_dialog = UI.make_wizard_dialog structure wm

    val content =
      match Element.query_selector elt Selector.content with
      | None -> failwith "content element not found"
      | Some x -> x

    val actions =
      match Element.query_selector elt Selector.actions with
      | None -> failwith "actions element not found"
      | Some x -> x

    val body = Dom_html.document##.body

    val undo_manager = Undo_manager.create ~limit:50 ()

    val list_of_widgets =
      let layout =
        List.map
          (fun (_, _, (x : Wm.Annotated.container)) ->
            List.map (fun (id, _, x) -> id, x) x.widgets)
          wm.layout
      in
      List_of_widgets.make scaffold (filter_available_widgets wm.widgets layout)

    val mutable _widgets = wm.widgets

    val mutable listeners = []

    val mutable drag_target = Js.null

    val mutable selection = None

    val mutable resize_observer = None

    val mutable top_app_bar_context = []

    val mutable top_app_bar_menu = None

    val mutable resolution = wm.resolution

    val mutable widget_editor = None

    inherit Widget.t elt () as super

    method! init () : unit =
      Option.iter
        (fun x -> x#root##.style##.display := Js.string "none")
        scaffold#side_sheet;
      init_top_app_bar_icon scaffold;
      ignore @@ set_top_app_bar_icon scaffold `Aux close_icon;
      resize_observer <-
        Some (Resize_observer.observe ~f:(fun _ _ -> self#layout ()) ~node:super#root ());
      (* Create selection widget *)
      selection <- Some (Selection.make self#handle_selected content);
      top_app_bar_menu <- Some (self#create_top_app_bar_menu ());
      (* Set list of widgets *)
      Option.iter
        (fun side_sheet -> Dom.appendChild side_sheet#root list_of_widgets#root)
        scaffold#side_sheet;
      (* Set top app bar actions *)
      (match scaffold#top_app_bar, top_app_bar_menu with
      | None, _ | _, None -> ()
      | Some top_app_bar, Some menu ->
          Undo_manager.set_callback undo_manager (fun _ -> menu#layout ());
          top_app_bar#set_actions [menu#root]);
      List.iter (Element.append_child actions % Widget.root)
      @@ self#create_main_actions ();
      let empty_placeholder =
        UI.make_empty_placeholder wizard_dialog table_dialog grid
      in
      Dom.appendChild body wizard_dialog#root;
      Dom.appendChild body (fst table_dialog)#root;
      Dom.appendChild content empty_placeholder#root;
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        Lwt_js_events.
          [ Grid.Event.inputs grid#root self#handle_grid_resize
          ; dragstarts grid#root self#handle_dragstart
          ; dragends grid#root self#handle_dragend
          ; dragenters grid#root self#handle_dragenter
          ; dragleaves grid#root self#handle_dragleave
          ; dragovers grid#root self#handle_dragover
          ; drops grid#root self#handle_drop ];
      super#initial_sync_with_dom ()

    method! layout () : unit =
      let frame_width = float_of_int @@ content##.offsetWidth in
      let frame_height = float_of_int @@ content##.offsetHeight in
      let frame_aspect = frame_width /. frame_height in
      let content_aspect =
        (float_of_int @@ fst resolution) /. (float_of_int @@ snd resolution)
      in
      (if frame_aspect < content_aspect
      then (
        (* Letterbox *)
        let height = Js.math##round (frame_height *. frame_aspect /. content_aspect) in
        grid#root##.style##.width := Js.string "100%";
        grid#root##.style##.height := Js.string @@ Printf.sprintf "%gpx" height)
      else
        (* Pillarbox *)
        let width = Js.math##round (frame_width *. content_aspect /. frame_aspect) in
        grid#root##.style##.height := Js.string "100%";
        grid#root##.style##.width := Js.string @@ Printf.sprintf "%gpx" width);
      Option.iter Widget.layout widget_editor;
      Option.iter Widget.layout top_app_bar_menu;
      super#layout ()

    method! destroy () : unit =
      Option.iter (fun x -> x##disconnect) resize_observer;
      resize_observer <- None;
      Option.iter (fun x -> x#destroy ()) selection;
      selection <- None;
      List.iter Lwt.cancel listeners;
      listeners <- [];
      Option.iter Widget.destroy widget_editor;
      (* Destroy menus *)
      Option.iter Widget.destroy top_app_bar_menu;
      (* Destroy dialogs *)
      Dom.removeChild body wizard_dialog#root;
      Dom.removeChild body (fst table_dialog)#root;
      wizard_dialog#destroy ();
      (fst table_dialog)#destroy ();
      super#destroy ()

    method selected : Dom_html.element Js.t list = (self#selection)#selected

    method resolution : int * int = resolution

    method value : Wm.t =
      let cols, rows = grid#cols, grid#rows in
      let cells = grid#cells in
      let layout =
        List.map
          (fun cell ->
            let position =
              cell_position_to_wm_position ~cols ~rows
              @@ Grid.Util.get_cell_position cell
            in
            let widgets = Widget_utils.widgets_of_container cell in
            let title = get_cell_title cell in
            title, {Wm.position; widgets})
          cells
      in
      {resolution = self#resolution; widgets = _widgets; layout}

    (* TODO implement layout update *)
    method notify : event -> unit =
      function
      | `Streams s -> (wizard_dialog#wizard)#notify (`Streams s)
      | `Layout wm -> (wizard_dialog#wizard)#notify (`Layout wm)
      | `Wizard wm ->
          let wm = Wm.Annotated.annotate ~active:wm ~stored:wm in
          let grid_props = grid_properties_of_layout wm in
          let cells =
            List.map
              (fun (id, ((container : Wm.Annotated.container), pos)) ->
                Tyxml_js.To_dom.of_element
                @@ Grid.Markup_js.create_cell
                     ~attrs:Tyxml_js.Html.[a_user_data "title" id]
                     ~content:(content_of_container container)
                     pos)
              grid_props.cells
          in
          grid#reset
            ~cells
            ~rows:(`Value grid_props.rows)
            ~cols:(`Value grid_props.cols)
            ()

    (* Private methods *)
    method private set_state state = (snd s_state) state

    method private selection : Selection.t = Option.get selection

    method private switch_to_widget_mode (cell : Dom_html.element Js.t) =
      let title = get_cell_title cell in
      let cols, rows, resolution = grid#cols, grid#rows, self#resolution in
      let position =
        cell_position_to_wm_position ~rows ~cols @@ Grid.Util.get_cell_position cell
      in
      let editor =
        Widget_editor.make
          ~scaffold
          ~resolution
          ~position
          ~list_of_widgets
          (`Nodes (Widget_utils.elements cell))
      in
      self#clear_selection ();
      widget_editor <- Some editor;
      let icon = set_top_app_bar_icon scaffold `Main back_icon in
      let restore =
        Actions.transform_top_app_bar
          ~actions:[(editor#top_app_bar_menu)#root]
          ~title
          scaffold
      in
      let state = {icon; restore; editor; cell} in
      let t, w = Lwt.wait () in
      scaffold#set_on_navigation_icon_click (fun _ _ ->
          Lwt.wakeup_later w state;
          Lwt.return_unit);
      (* Update view *)
      list_of_widgets#sync
      @@ filter_available_widgets _widgets
      @@ List.map Widget_utils.widgets_of_container grid#cells;
      Element.add_class body CSS.widget_mode;
      Dom.appendChild content editor#root;
      editor#layout ();
      Option.fold
        ~none:Lwt.return_unit
        ~some:(fun (x : Side_sheet.t) ->
          x#root##.style##.display := Js.string "";
          match scaffold#side_sheet_type with
          | Modal | Permanent -> Lwt.return_unit
          | Dismissible -> x#toggle ~force:true ())
        scaffold#side_sheet
      >>= fun () -> t

    method private switch_to_container_mode
        ({restore; icon; editor; cell} : widget_mode_state) =
      restore ();
      widget_editor <- None;
      (* Update view *)
      Option.iter (ignore % set_top_app_bar_icon scaffold `Main) icon;
      let items = editor#items in
      Widget_utils.Z_index.validate items;
      self#update_widget_elements items cell;
      Element.remove_class body CSS.widget_mode;
      Dom.removeChild content editor#root;
      grid#layout ();
      editor#destroy ();
      (* Hide side sheet *)
      match scaffold#side_sheet with
      | None -> Lwt.return_unit
      | Some x ->
          x#toggle ~force:false ()
          >>= fun () ->
          x#root##.style##.display := Js.string "none";
          Lwt.return_unit

    method private handle_grid_resize _ _ : unit Lwt.t =
      self#layout ();
      Lwt.return_unit

    method private handle_dragstart e _ =
      let target = Dom_html.eventTarget e in
      drag_target <- e##.target;
      e##.dataTransfer##setData (Js.string "text/html") target##.innerHTML;
      e##.dataTransfer##.effectAllowed := Js.string "move";
      Element.add_class target CSS.cell_dragging;
      Lwt.return_unit

    method private handle_dragenter e _ =
      let target = Dom_html.eventTarget e in
      if Element.has_class target Grid.CSS.cell
         && Js.Opt.case drag_target (fun () -> true) (not % Element.equal target)
      then Element.add_class target CSS.cell_dragover;
      Lwt.return_unit

    method private handle_dragleave e _ =
      let target = Dom_html.eventTarget e in
      Element.remove_class target CSS.cell_dragover;
      Lwt.return_unit

    method private handle_dragover e _ =
      let target = Dom_html.eventTarget e in
      if Element.has_class target Grid.CSS.cell && not (Js.some target == drag_target)
      then (
        Dom.preventDefault e;
        e##.dataTransfer##.dropEffect := Js.string "move");
      Lwt.return_unit

    method private handle_drop e _ =
      Dom_html.stopPropagation e;
      let target = Dom_html.eventTarget e in
      Js.Opt.iter drag_target (fun dragged ->
          if not @@ Element.equal dragged target
          then (
            swap dragged target;
            let action =
              { Undo_manager.undo = (fun () -> swap target dragged)
              ; redo = (fun () -> swap dragged target) }
            in
            Undo_manager.add undo_manager action));
      Lwt.return_unit

    method private handle_dragend e _ =
      let target = Dom_html.eventTarget e in
      Element.remove_class target CSS.cell_dragging;
      List.iter (Fun.flip Element.remove_class CSS.cell_dragover) grid#cells;
      Lwt.return_unit

    method private restore_top_app_bar_context () : unit =
      match top_app_bar_context with
      | [] -> ()
      | f :: tl ->
          f ();
          top_app_bar_context <- tl

    method private clear_selection () : unit =
      List.iter (Fun.flip Element.remove_class Selection.class_) self#selected;
      (self#selection)#clear_selection ();
      self#set_state [];
      self#restore_top_app_bar_context ()

    method private handle_selected cells =
      (match cells with
      | [] -> self#clear_selection ()
      | cells -> (
          let title =
            match cells with
            | [x] -> (
              match Element.get_attribute x Attr.title with
              | Some x -> x
              | None -> "Выбран контейнер")
            | _ -> Printf.sprintf "Выбрано ячеек: %d" @@ List.length cells
          in
          let restore =
            Actions.transform_top_app_bar
              ~title
              ~class_:CSS.top_app_bar_contextual
              ~on_navigation_icon_click:(fun _ _ ->
                self#clear_selection ();
                Lwt.return_unit)
              scaffold
          in
          match top_app_bar_context with
          | [] -> top_app_bar_context <- [restore]
          | _ -> ()));
      self#set_state cells

    method private create_top_app_bar_menu () : Components_lab.Overflow_menu.t =
      Actions.Containers.make_menu
        (fst s_state)
        ~edit_container:(fun x ->
          self#switch_to_widget_mode x >>= self#switch_to_container_mode)
        ~on_remove:(fun () -> self#clear_selection ())
        undo_manager
        wizard_dialog
        scaffold
        grid

    method private create_main_actions () =
      let submit =
        Button.make
          ~label:"Сохранить"
          ~on_click:(fun btn _ _ ->
            let value = self#value in
            let t = Pipeline_http_js.Http_wm.set_layout value in
            btn#set_loading_lwt t;
            t
            >>= function
            | Ok _ ->
                let label = "Мозаика сохранена" in
                let snackbar = Snackbar.make ~dismiss:True ~label () in
                snackbar#set_timeout 4.;
                scaffold#show_snackbar ~on_close:(fun _ -> snackbar#destroy ()) snackbar
            | Error e ->
                let label =
                  Printf.sprintf "Ошибка. %s" @@ Api_js.Http.error_to_string e
                in
                let snackbar = Snackbar.make ~label () in
                scaffold#show_snackbar ~on_close:(fun _ -> snackbar#destroy ()) snackbar)
          ()
      in
      [Card.Actions.make_buttons [submit]]

    method private update_widget_elements
        (widgets : Dom_html.element Js.t list)
        (cell : Dom_html.element Js.t)
        : unit =
      let wrapper =
        match Element.query_selector cell Selector.widget_wrapper with
        | None ->
            let elt = Tyxml_js.To_dom.of_element @@ Markup_js.create_widget_wrapper [] in
            Dom.appendChild cell elt;
            elt
        | Some x ->
            Element.remove_children x;
            x
      in
      List.iter
        (fun (x : Dom_html.element Js.t) ->
          let elt = Dom_html.(createDiv document) in
          elt##.style##.left := x##.style##.left;
          elt##.style##.top := x##.style##.top;
          elt##.style##.width := x##.style##.width;
          elt##.style##.height := x##.style##.height;
          Widget_utils.copy_attributes x elt;
          Widget_utils.Z_index.set (Widget_utils.Z_index.get x) elt;
          Element.add_class elt CSS.widget;
          Dom.appendChild wrapper elt)
        widgets
  end

let make_grid (props : grid_properties) =
  let cells =
    List.map
      (fun (id, ((container : Wm.Annotated.container), pos)) ->
        Grid.Markup_js.create_cell
          ~attrs:Tyxml_js.Html.[a_user_data "title" id]
          ~content:(content_of_container container)
          pos)
      props.cells
  in
  Grid.Markup_js.create
    ~rows:(`Value props.rows)
    ~cols:(`Value props.cols)
    ~content:cells
    ()

let make
    ~(scaffold : Scaffold.t)
    (structure : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let grid = make_grid @@ grid_properties_of_layout wm in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ Markup_js.create grid
  in
  new t ~scaffold structure wm elt
