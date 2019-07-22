open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pipeline_types

include Page_mosaic_editor_tyxml.Container_editor
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type editing_mode = Content | Table

let name = "container-editor"

let failwith s = failwith @@ Printf.sprintf "%s: %s" name s

let editing_mode_of_enum = function
  | 0 -> Content
  | 1 -> Table
  | _ -> invalid_arg "invalid mode value"

type event =
  [ `Layout of Wm.Annotated.t
  | `Streams of Structure.Annotated.t
  ]

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

let flip f x y = f y x

module Selector = struct
  let heading = Printf.sprintf ".%s" Card.CSS.primary
  let content = Printf.sprintf ".%s" Card.CSS.media
  let actions = Printf.sprintf ".%s" Card.CSS.actions
  let cell = Printf.sprintf ".%s" Grid.CSS.cell
  let grid = Printf.sprintf ".%s" Grid.CSS.root
  let mode_switch = Printf.sprintf ".%s" CSS.mode_switch
  let widget_wrapper = Printf.sprintf ".%s" CSS.widget_wrapper
end

module Selection = struct
  include Selection

  let class_ = Grid.CSS.cell_selected

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

  let on_select handle_selected = fun item { selection; selected; _ } ->
    let is_selected = Element.has_class item class_ in
    List.iter (fun x -> Element.remove_class x class_) selected;
    selection#deselect_all ();
    (match List.length selected > 1 && List.memq item selected with
     | true -> Element.add_class item class_; selection#select [item]
     | _ ->
       if is_selected
       then (Element.remove_class item class_; selection#deselect item)
       else (Element.add_class item class_; selection#select [item]));
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

let on_cell_insert
    (grid : Grid.t)
    (cell : Dom_html.element Js.t) =
  let title = Container_utils.gen_cell_title grid#cells in
  Container_utils.set_cell_title cell title

(** Updates properties of the aspect ratio sizer according to the new
    area resolution *)
let update_ar_sizer ~width ~height (sizer : Dom_html.element Js.t) =
  let viewbox = Printf.sprintf "0 0 %g %g" width height in
  Element.set_attribute sizer "viewBox" viewbox

let get f l =
  let rec aux acc = function
    | [] -> None, l
    | x :: tl ->
      if f x
      then Some x, (List.rev acc) @ tl
      else aux (x :: acc) tl in
  aux [] l

let init_top_app_bar_icon (scaffold : Scaffold.t) =
  match Utils.Option.bind (fun x -> x#leading) scaffold#top_app_bar with
  | None -> ()
  | Some x ->
    let icons =
      Element.query_selector_all x
      @@ Printf.sprintf ".%s" Icon.CSS.root in
    match icons with
    | [x] -> Element.add_class x CSS.nav_icon_main
    | [x; y] ->
      Element.add_class x CSS.nav_icon_main;
      Element.add_class x CSS.nav_icon_aux
    | _ -> ()

let set_top_app_bar_icon (scaffold : Scaffold.t) typ icon =
  let class_, index = match typ with
    | `Main -> CSS.nav_icon_main, 0
    | `Aux -> CSS.nav_icon_aux, 1 in
  match Utils.Option.bind (fun x -> x#leading) scaffold#top_app_bar with
  | None -> None
  | Some x ->
    let prev =
      Element.query_selector x
      @@ Printf.sprintf ".%s" class_ in
    (match prev with
     | None -> ()
     | Some prev -> Dom.removeChild x prev);
    Element.add_class icon class_;
    Element.insert_child_at_index x index icon;
    prev

let content_aspect_of_element (cell : Dom_html.element Js.t) =
  (* FIXME *)
  match Widget_utils.Attr.get_aspect cell with
  | Some x -> x
  | None -> failwith "no aspect provided"

let filter_available_widgets
    (widgets : (string * Wm.widget) list)
    (wm : (string * Wm.widget) list list)
  : (string * Wm.widget) list =
  List.fold_left (fun acc container ->
      List.filter (fun (id, _) -> not @@ List.mem_assoc id container) acc)
    widgets wm

type widget_mode_state =
  { icon : Dom_html.element Js.t option
  ; restore : unit -> unit
  ; editor : Widget_editor.t
  ; cell : Dom_html.element Js.t
  }

class t ~(scaffold : Scaffold.t)
    (structure : Structure.Annotated.t)
    (wm : Wm.Annotated.t)
    (elt : Dom_html.element Js.t)
    () =
  let (grid : Grid.t) =
    match Element.query_selector elt Selector.grid with
    | None -> failwith "grid element not found"
    | Some x -> Grid.attach ~on_cell_insert ~drag_interval:(Fr 0.05) x in
  let table_dialog = Container_utils.UI.add_table_dialog () in
  let wizard_dialog = Wizard.make structure wm in
  object(self)
    val close_icon = Icon.SVG.(make_simple Path.close)
    val back_icon = Icon.SVG.(make_simple Path.arrow_left)
    val content = match Element.query_selector elt Selector.content with
      | None -> failwith "content element not found"
      | Some x -> x
    val actions = match Element.query_selector elt Selector.actions with
      | None -> failwith "actions element not found"
      | Some x -> x
    val body = Dom_html.document##.body

    val undo_manager = Undo_manager.create ()
    val list_of_widgets =
      let layout = List.map (fun (_, _, (x : Wm.Annotated.container)) ->
          List.map (fun (id, _, x) -> id, x) x.widgets) wm.layout in
      List_of_widgets.make (filter_available_widgets wm.widgets layout)
    val empty_placeholder =
      Container_utils.UI.make_empty_placeholder
        wizard_dialog
        table_dialog
        grid

    val mutable _widgets = wm.widgets
    val mutable _listeners = []
    val mutable _content_listeners = []
    val mutable _drag_target = Js.null

    val mutable _selection = None
    val mutable _resize_observer = None
    val mutable _top_app_bar_context = []
    val mutable _edit_mode = Table

    val mutable _basic_actions = None
    val mutable _cell_selected_actions = []
    val mutable _cont_selected_actions = []

    val mutable _resolution = wm.resolution

    val mutable _widget_editor = None
    val mutable _mode_switch = None

    inherit Widget.t elt () as super

    method! init () : unit =
      init_top_app_bar_icon scaffold;
      ignore @@ set_top_app_bar_icon scaffold `Aux close_icon#root;
      _resize_observer <- Some (self#create_resize_observer ());
      _selection <- Some (Selection.make self#handle_selected content);
      let basic_actions = self#create_actions () in
      _basic_actions <- Some basic_actions;
      _cell_selected_actions <- self#create_cell_selected_actions ();
      _cont_selected_actions <- self#create_cont_selected_actions ();
      _mode_switch <- begin match Element.query_selector
                                    Dom_html.document##.body
                                    Selector.mode_switch with
        | None -> failwith @@ Printf.sprintf "%s: mode switch element not found" name
        | Some x ->
          let on_change = function
            | None -> assert false
            | Some x -> self#switch_mode (editing_mode_of_enum x) in
          let tab_bar = Tab_bar.attach
              ~on_change:(fun _ x ->
                  on_change x#active_tab_index;
                  Lwt.return_unit)
              x in
          on_change tab_bar#active_tab_index;
          Some tab_bar
      end;
      (match scaffold#side_sheet with
       | None -> ()
       | Some x -> Dom.appendChild x#root list_of_widgets#root);
      (match scaffold#top_app_bar with
       | None -> ()
       | Some x -> x#set_actions @@ List.map Widget.root [basic_actions]);
      List.iter (Element.append_child actions % Widget.root)
      @@ self#create_main_actions ();
      Dom.appendChild body wizard_dialog#root;
      Dom.appendChild body (fst table_dialog)#root;
      Dom.appendChild content empty_placeholder#root;
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <- Events.(
          [ Grid.Event.inputs grid#root self#handle_grid_resize
          ; dragstarts grid#root self#handle_dragstart
          ; dragends grid#root self#handle_dragend
          ; dragenters grid#root self#handle_dragenter
          ; dragleaves grid#root self#handle_dragleave
          ; dragovers grid#root self#handle_dragover
          ; drops grid#root self#handle_drop
          ]);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      let frame_width = float_of_int @@ content##.offsetWidth in
      let frame_height = float_of_int @@ content##.offsetHeight in
      let frame_aspect = frame_width /. frame_height in
      let content_aspect =
        (float_of_int @@ fst _resolution)
        /. (float_of_int @@ snd _resolution) in
      if frame_aspect < content_aspect
      then (
        (* Letterbox *)
        let height = (frame_height *. frame_aspect) /. content_aspect in
        grid#root##.style##.width := Js.string "100%";
        grid#root##.style##.height := Js.string @@ Printf.sprintf "%gpx" height)
      else (
        (* Pillarbox *)
        let width = (frame_width *. content_aspect) /. frame_aspect in
        grid#root##.style##.height := Js.string "100%";
        grid#root##.style##.width := Js.string @@ Printf.sprintf "%gpx" width);
      Utils.Option.iter (Widget.layout % snd) _widget_editor;
      Utils.Option.iter Widget.layout _basic_actions;
      List.iter Widget.layout _cell_selected_actions;
      List.iter Widget.layout _cont_selected_actions;
      super#layout ()

    method! destroy () : unit =
      Utils.Option.iter Ui_templates.Resize_observer.disconnect _resize_observer;
      _resize_observer <- None;
      Utils.Option.iter (fun x -> x#destroy ()) _selection;
      _selection <- None;
      List.iter Lwt.cancel _listeners;
      _listeners <- [];
      Utils.Option.iter (Widget.destroy % snd) _widget_editor;
      (* Destroy menus *)
      Utils.Option.iter Widget.destroy _basic_actions;
      List.iter Widget.destroy _cell_selected_actions;
      List.iter Widget.destroy _cont_selected_actions;
      (* Destroy dialogs *)
      Dom.removeChild body wizard_dialog#root;
      Dom.removeChild body (fst table_dialog)#root;
      wizard_dialog#destroy ();
      (fst table_dialog)#destroy ();
      super#destroy ()

    method selected : Dom_html.element Js.t list =
      self#selection#selected

    method clear_selection () : unit =
      List.iter (flip Element.remove_class Selection.class_) self#selected;
      self#selection#deselect_all ();
      self#restore_top_app_bar_context ()

    method resolution : int * int =
      _resolution

    method value : Wm.t =
      let cols, rows, resolution = grid#cols, grid#rows, self#resolution in
      let layout =
        List.map (fun cell ->
            let position =
              Container_utils.cell_position_to_wm_position
                ~cols
                ~rows
              @@ Grid.Util.get_cell_position cell in
            let widgets = Widget_utils.widgets_of_container cell in
            Container_utils.get_cell_title cell,
            { Wm. position; widgets })
          grid#cells in
      { resolution = self#resolution
      ; widgets = _widgets
      ; layout
      }

    (* TODO implement layout update *)
    method notify : event -> unit = function
      | `Streams _ -> ()
      | `Layout wm ->
        match _widget_editor with
        | None -> ()
        | Some (id, editor) ->
          match List.find_opt (fun (id', _, _) ->
              String.equal id' id) wm.layout with
          | None -> () (* FIXME container lost, handle it somehow *)
          | Some (_, state, container) ->
            editor#notify @@ `Container (state, container)

    (* Private methods *)

    method edit_container (container : Dom_html.element Js.t) : unit Lwt.t =
      self#switch_to_widget_mode container
      >>= self#switch_to_container_mode

    method private scale_container
        ~(cell : Dom_html.element Js.t)
        (container : Wm.container) =
      let cur_width = float_of_int cell##.offsetWidth in
      let cur_height = float_of_int cell##.offsetHeight in
      let cur_aspect = cur_width /. cur_height in
      let aw, ah = content_aspect_of_element cell in
      let aspect = float_of_int aw /. float_of_int ah in
      let scale_factor =
        if cur_aspect > aspect
        then cur_height /. float_of_int ah
        else cur_width /. float_of_int aw in
      scale_factor *. float_of_int aw,
      scale_factor *. float_of_int ah

    method private switch_to_widget_mode (cell : Dom_html.element Js.t) =
      let id = Container_utils.get_cell_title cell in
      let cols, rows, resolution = grid#cols, grid#rows, self#resolution in
      let position = Container_utils.cell_position_to_wm_position
          ~rows ~cols
        @@ Grid.Util.get_cell_position cell in
      let editor = Widget_editor.make
          ~scaffold
          ~resolution
          ~position
          ~list_of_widgets
          (`Nodes (Widget_utils.elements cell))
      in
      self#clear_selection ();
      _widget_editor <- Some (id, editor);
      let icon = set_top_app_bar_icon scaffold `Main back_icon#root in
      let restore = Actions.transform_top_app_bar
          ~title:id
          ~actions:editor#actions
          scaffold in
      let state = { icon; restore; editor; cell } in
      let t, w = Lwt.wait () in
      scaffold#set_on_navigation_icon_click (fun _ _ ->
          Lwt.wakeup_later w state;
          Lwt.return_unit);
      (* Update view *)
      list_of_widgets#sync
      @@ filter_available_widgets _widgets
      @@ List.map (fun x -> Widget_utils.widgets_of_container x) grid#cells;
      Element.add_class Dom_html.document##.body CSS.widget_mode;
      (match _mode_switch with
       | None -> ()
       | Some x -> x#add_class CSS.mode_switch_hidden);
      Dom.appendChild content editor#root;
      editor#layout ();
      t

    method private switch_to_container_mode
        ({ restore; icon; editor; cell } : widget_mode_state) =
      restore ();
      _widget_editor <- None;
      (* Update view *)
      (match _mode_switch with
       | None -> ()
       | Some x -> x#remove_class CSS.mode_switch_hidden);
      Utils.Option.iter (ignore % set_top_app_bar_icon scaffold `Main) icon;
      self#update_widget_elements editor#items cell;
      Element.remove_class Dom_html.document##.body CSS.widget_mode;
      Dom.removeChild content editor#root;
      grid#layout ();
      editor#destroy ();
      (* Hide side sheet *)
      (match scaffold#side_sheet with
       | None -> Lwt.return_unit
       | Some x -> x#toggle ~force:false ())

    method private handle_grid_resize (e : Grid.Event.resize Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      self#layout ();
      Lwt.return_unit

    method private handle_dragstart e _ =
      let target = Dom_html.eventTarget e in
      _drag_target <- e##.target;
      e##.dataTransfer##setData (Js.string "text/html") target##.innerHTML;
      e##.dataTransfer##.effectAllowed := Js.string "move";
      Element.add_class target CSS.cell_dragging;
      Lwt.return_unit

    method private handle_dragenter e _ =
      let target = Dom_html.eventTarget e in
      if Element.has_class target Grid.CSS.cell
      && (Js.Opt.case _drag_target (fun () -> true) (not % Element.equal target))
      then Element.add_class target CSS.cell_dragover;
      Lwt.return_unit

    method private handle_dragleave e _ =
      let target = Dom_html.eventTarget e in
      Element.remove_class target CSS.cell_dragover;
      Lwt.return_unit

    method private handle_dragover e _ =
      let target = Dom_html.eventTarget e in
      if Element.has_class target Grid.CSS.cell
      && (not (Js.some target == _drag_target))
      then (Dom.preventDefault e;
            e##.dataTransfer##.dropEffect := Js.string "move");
      Lwt.return_unit

    method private handle_drop e _ =
      Dom_html.stopPropagation e;
      let target = Dom_html.eventTarget e in
      Js.Opt.iter _drag_target (fun dragged ->
          if not @@ Element.equal dragged target
          then (
            Container_utils.swap dragged target;
            let action =
              { Undo_manager.
                undo = (fun () -> Container_utils.swap target dragged)
              ; redo = (fun () -> Container_utils.swap dragged target)
              } in
            Undo_manager.add undo_manager action));
      Lwt.return_unit

    method private handle_dragend e _ =
      let target = Dom_html.eventTarget e in
      Element.remove_class target CSS.cell_dragging;
      List.iter (flip Element.remove_class CSS.cell_dragover) grid#cells;
      Lwt.return_unit

    method private handle_click e _ : unit Lwt.t =
      match Grid.Util.cell_of_event grid#cells e with
      | None -> Lwt.return_unit
      | Some cell ->
        let is_selected = List.memq cell self#selected in
        self#clear_selection ();
        if is_selected
        then (self#selection#deselect cell;
              Element.remove_class cell Selection.class_)
        else (self#selection#select [cell];
              Element.add_class cell Selection.class_);
        self#handle_selected self#selected;
        Lwt.return_unit

    method private restore_top_app_bar_context () : unit =
      match _top_app_bar_context with
      | [] -> ()
      | f :: tl -> f (); _top_app_bar_context <- tl

    method private handle_selected = function
      | [] -> self#restore_top_app_bar_context ()
      | cells ->
        let title = match _edit_mode, cells with
          | Content, [cell] ->
            (match Element.get_attribute cell Container_utils.Attr.title with
             | None -> "Выбран контейнер"
             | Some x -> x)
          | _ ->
            Printf.sprintf "Выбрано ячеек: %d"
            @@ List.length cells in
        (match _mode_switch with
         | None -> ()
         | Some x -> x#add_class CSS.mode_switch_hidden);
        let restore = Actions.transform_top_app_bar
            ~title
            ~class_:CSS.top_app_bar_contextual
            ~actions:(match _edit_mode with
                | Table -> _cell_selected_actions
                | Content -> _cont_selected_actions)
            ~on_navigation_icon_click:(fun _ _ ->
                self#clear_selection ();
                Lwt.return_unit)
            scaffold in
        match _top_app_bar_context with
        | _ :: _ -> ()
        | [] ->
          let restore = fun () ->
            (match _mode_switch with
             | None -> ()
             | Some x -> x#remove_class CSS.mode_switch_hidden);
            restore () in
          _top_app_bar_context <- [restore]

    method private create_actions () : Overflow_menu.t =
      Actions.Container_actions.make_menu wizard_dialog undo_manager grid

    method private create_cell_selected_actions () : Widget.t list =
      let menu = Actions.Cell_selected_actions.make_menu
          ~on_remove:(fun () -> self#clear_selection ())
          ~get_selected:(fun () -> self#selected)
          undo_manager
          grid in
      [menu#widget]

    method private create_cont_selected_actions () : Widget.t list =
      let menu = Actions.Container_selected_actions.make_menu
          ~get_selected:(fun () -> self#selected)
          ~edit_container:self#edit_container
          () in
      [menu#widget]

    method private create_main_actions () =
      let submit = Button.make
          ~label:"Применить"
          ~on_click:(fun btn _ _ ->
              let value = self#value in
              let t = Pipeline_http_js.Http_wm.set_layout value in
              btn#set_loading_lwt t;
              t >>= fun _ -> Lwt.return_unit)
          () in
      let buttons = Card.Actions.make_buttons [submit] in
      [buttons]

    method private create_resize_observer () =
      let f = fun _ -> self#layout () in
      Ui_templates.Resize_observer.observe ~f ~node:super#root ()

    method private selection : Selection.t =
      Utils.Option.get _selection

    method private switch_mode (mode : editing_mode) : unit =
      self#clear_selection ();
      _edit_mode <- mode;
      match mode with
      | Table -> self#set_table_mode ()
      | Content -> self#set_content_mode ()

    method private set_content_mode () : unit =
      self#selection#set_disabled true;
      Element.add_class Dom_html.document##.body CSS.content_mode;
      _content_listeners <- Events.(
          [ clicks grid#root self#handle_click
          ])

    method private set_table_mode () : unit =
      self#selection#set_disabled false;
      Element.remove_class Dom_html.document##.body CSS.content_mode;
      List.iter Lwt.cancel _content_listeners;
      _content_listeners <- []

    method private update_widget_elements
        (widgets : Dom_html.element Js.t list)
        (cell : Dom_html.element Js.t) : unit =
      let wrapper = match Element.query_selector cell Selector.widget_wrapper with
        | None ->
          let elt =
            Tyxml_js.To_dom.of_element
            @@ Markup.create_widget_wrapper [] in
          Dom.appendChild cell elt;
          elt
        | Some x -> Element.remove_children x; x in
      List.iter (fun (x : Dom_html.element Js.t) ->
          let elt = Dom_html.(createDiv document) in
          elt##.style##.left := x##.style##.left;
          elt##.style##.top := x##.style##.top;
          elt##.style##.width := x##.style##.width;
          elt##.style##.height := x##.style##.height;
          Widget_utils.copy_attributes x elt;
          Widget_utils.Z_index.set elt (Widget_utils.Z_index.get x);
          Element.add_class elt CSS.widget;
          Dom.appendChild wrapper elt) widgets

  end

let content_of_container (container : Wm.Annotated.container) =
  let widgets = List.map Markup.create_widget container.widgets in
  [Markup.create_widget_wrapper widgets]

let make_grid (props : Container_utils.grid_properties) =
  let cells = List.map (fun (id, ((container : Wm.Annotated.container), pos)) ->
      Grid.Markup.create_cell
        ~attrs:Tyxml_js.Html.([a_user_data "title" id])
        ~content:(content_of_container container)
        pos)
      props.cells in
  Grid.Markup.create
    ~rows:(`Value props.rows)
    ~cols:(`Value props.cols)
    ~content:cells
    ()

let make
    ~(scaffold : Scaffold.t)
    (structure : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let grid = make_grid @@ Container_utils.grid_properties_of_layout wm in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create
      ~width:(float_of_int @@ fst wm.resolution)
      ~height:(float_of_int @@ snd wm.resolution)
      ~grid () in
  new t ~scaffold structure wm elt ()
