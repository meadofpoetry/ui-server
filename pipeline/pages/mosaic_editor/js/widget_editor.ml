open Js_of_ocaml
open Js_of_ocaml_tyxml
open Pipeline_types
open Components

type event =
  [ `Container of Wm.Annotated.state * Wm.Annotated.container
  ]

include Page_mosaic_editor_tyxml.Widget_editor
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Selector = struct
  let item = Printf.sprintf ".%s" CSS.item
  let grid_overlay = Printf.sprintf ".%s" CSS.overlay
  let grid_ghost = Printf.sprintf ".%s" CSS.ghost
  let parent = Printf.sprintf ".%s" Card.CSS.media
end

let set_tab_index ?prev
    (items : Dom_html.element Js.t list Lazy.t)
    (item : Dom_html.element Js.t) : unit =
  let set (i : int) (elt : Dom_html.element Js.t) =
    Element.set_attribute elt "tabindex" (string_of_int i) in
  (match prev with
   | Some prev -> if not @@ Element.equal item prev then set (-1) prev
   | None ->
     (* If no list item was selected, set first list item's tabindex to -1.
        Generally, tabindex is set to 0 on first list item of list that has
        no preselected items *)
     match Lazy.force items with
     | first :: _ -> if not @@ Element.equal first item then (set (-1) first)
     | _ -> ());
  set 0 item

let make_item ~parent_size (id, widget : string * Wm.widget) =
  let item = Tyxml_js.To_dom.of_element @@ Markup.create_item widget in
  Widget_utils.set_attributes ~id item widget;
  Widget_utils.Z_index.set widget.layer item;
  item

module Selection = struct
  include Selection

  let class_ = CSS.item_selected

  let selectables = [Query Selector.item]

  let boundaries = [Query Selector.parent]

  let item_of_event (e : #Dom_html.event Js.t) =
    let target = Dom.eventTarget e in
    let selector = Printf.sprintf ".%s, .%s" CSS.item CSS.root in
    let nearest_parent = Element.closest target selector in
    Js.Opt.bind nearest_parent (fun (parent : Dom_html.element Js.t) ->
        if not @@ Element.matches parent ("." ^ CSS.item)
        then Js.null else Js.some parent)

  let validate_start = fun e ->
    let item = Js.null in (* item_of_event e in *)
    Js.Opt.case item
      (fun () ->
         Js.Opt.case (Dom_html.CoerceTo.mouseEvent e)
           (fun () -> true)
           (fun e -> e##.button = 0))
      (fun _ -> false)

  let on_start = fun { selected; selection; _ } ->
    List.iter (fun x -> Element.remove_class x class_) selected;
    selection#deselect_all ()

  let on_move = fun { selected; removed; _ } ->
    List.iter (fun x -> Element.add_class x class_) selected;
    List.iter (fun x -> Element.remove_class x class_) removed

  let on_stop = fun handle_selected { selected; selection; _ } ->
    selection#keep_selection ();
    handle_selected selection#selected

  let on_select = fun handle_selected item selected selection ->
    List.iter (fun x -> Element.remove_class x class_) selected;
    selection#keep_selection ();
    selection#deselect_all ();
    selection#select [item];
    Element.add_class item class_;
    handle_selected selection#selected

  let make handle_selected =
    make ~validate_start
      ~selectables
      ~boundaries
      ~start_areas:boundaries
      ~on_start
      ~on_move
      ~on_stop:(on_stop handle_selected)
      ~on_select:(fun item { selected; selection; _ } ->
          on_select handle_selected item selected selection)
      ~on_outside_click:(fun x -> on_start x; handle_selected [])
      ()
end

class t
    ~(list_of_widgets : List_of_widgets.t)
    ~(resolution : int * int)
    ~(position : Position.Normalized.t)
    (scaffold : Scaffold.t)
    elt
    () =
  let width, height, aspect =
    let w, h =
      float_of_int (fst resolution),
      float_of_int (snd resolution) in
    let w, h = w *. position.w, (h *. position.h) in
    w, h, w /. h in
  object(self)

    inherit Drop_target.t elt () as super

    val grid_overlay = match Element.query_selector elt Selector.grid_overlay with
      | None -> failwith "widget-editor: grid overlay element not found"
      | Some x ->
        let show_grid_lines = Storage.(get_bool ~default:true show_grid_lines) in
        let show_snap_lines = Storage.(get_bool ~default:true show_snap_lines) in
        Grid_overlay.attach ~show_grid_lines ~show_snap_lines ~size:10 x

    val ghost = match Element.query_selector elt Selector.grid_ghost with
      | None -> failwith "widget-editor: grid ghost element not found"
      | Some x -> x

    val transform = Transform.make
        ~transformables:[Query (Printf.sprintf ".%s" CSS.item_selected)]
        ()

    val undo_manager = Undo_manager.create ()

    val mutable format = List_of_widgets.format
    val mutable _listeners = []
    val mutable _focused_item = None
    val mutable _top_app_bar_context = None
    val mutable min_size = 20.

    val mutable _selection = None
    val mutable _original_rects = []

    val mutable _basic_actions = []
    val mutable _selected_actions = []
    val mutable _transform_aspect = None

    method! init () : unit =
      Dom.appendChild super#root transform#root;
      _selection <- Some (Selection.make self#handle_selected);
      _basic_actions <- self#create_actions ();
      _selected_actions <- self#create_selected_actions ();
      self#clear_selection ();
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <- Events.(
          [ Transform.Event.inputs transform#root self#handle_transform_action
          ; Transform.Event.changes transform#root self#handle_transform_change
          ; Transform.Event.select transform#root self#handle_transform_select
          ; keydowns super#root self#handle_keydown
          ]);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      self#restore_top_app_bar_context ~hard:true ();
      List.iter Lwt.cancel _listeners; _listeners <- [];
      Utils.Option.iter Widget.destroy _selection;
      _selection <- None;
      List.iter Widget.destroy _basic_actions; _basic_actions <- [];
      List.iter Widget.destroy _selected_actions; _selected_actions <- [];
      super#destroy ()

    method! layout () : unit =
      let frame_width, frame_height, frame_aspect =
        Js.Opt.case (Element.get_parent super#root)
          (fun () -> 0., 0., 1.)
          (fun x ->
             let width = float_of_int x##.offsetWidth in
             let height = float_of_int x##.offsetHeight in
             width, height, width /. height) in
      if frame_aspect < aspect
      then (
        (* Letterbox *)
        let height = Js.math##round ((frame_height *. frame_aspect) /. aspect) in
        super#root##.style##.width := Js.string "100%";
        super#root##.style##.height := Js.string @@ Printf.sprintf "%gpx" height)
      else (
        (* Pillarbox *)
        let width = Js.math##round ((frame_width *. aspect) /. frame_aspect) in
        super#root##.style##.height := Js.string "100%";
        super#root##.style##.width := Js.string @@ Printf.sprintf "%gpx" width);
      grid_overlay#layout ();
      List.iter Widget.layout _basic_actions;
      List.iter Widget.layout _selected_actions;
      super#layout ()

    method items : Dom_html.element Js.t list =
      self#items_ ()

    method notify : event -> unit = function
      | `Container x ->
        (* TODO add notification if widget layout is changed
           & we have some unsaved work *)
        (* TODO implement simple update *)
        ()

    method value : Wm.container =
      { position
      ; widgets = List.map Widget_utils.widget_of_element self#items
      }

    method actions : Widget.t list =
      _basic_actions

    (* Private methods *)

    method private size : float * float =
      float_of_int elt##.offsetWidth,
      float_of_int elt##.offsetHeight

    method private add_item_ item =
      list_of_widgets#remove_by_id (Widget_utils.Attr.get_id item);
      Dom.appendChild super#root item

    (** Add item with undo *)
    method private add_item ((_, { position; _ }) as x : string * Wm.widget) =
      let item = make_item ~parent_size:self#size x in
      (match position with
       | None -> ()
       | Some p ->
         Position.Normalized.apply_to_element p item;
         Widget_utils.Attr.set_position p item);
      self#add_item_ item;
      Undo_manager.add undo_manager
        { undo = (fun () -> self#remove_item_ item)
        ; redo = (fun () -> self#add_item_ item)
        }

    method private remove_item_ (item : Dom_html.element Js.t) =
      list_of_widgets#append_item @@ Widget_utils.widget_of_element item;
      Element.remove_child_safe super#root item;

      (** Remove item with undo *)
    method private remove_item item =
      self#remove_item_ item;
      Undo_manager.add undo_manager
        { undo = (fun () -> self#add_item_ item)
        ; redo = (fun () -> self#remove_item_ item)
        }

    method private remove_items items =
      self#clear_selection ();
      List.iter self#remove_item_ items;
      Undo_manager.add undo_manager
        { undo = (fun () -> List.iter self#add_item_ items)
        ; redo = (fun () -> List.iter self#remove_item_ items)
        }

    method private create_selected_actions () : Widget.t list =
      let menu = Actions.make_overflow_menu
          Actions.(
            [ make ~callback:(fun _ _ ->
                  self#bring_to_front self#selected;
                  Lwt.return_unit)
                  ~name:"На передний план"
                  ~icon:Icon.SVG.Path.arrange_bring_to_front
                  ()
            ; make ~callback:(fun _ _ ->
                  self#send_to_back self#selected;
                  Lwt.return_unit)
                  ~name:"На задний план"
                  ~icon:Icon.SVG.Path.arrange_send_to_back
                  ()
            ; make ~callback:(fun _ _ ->
                  self#remove_items self#selected;
                  Lwt.return_unit)
                  ~name:"Удалить"
                  ~icon:Icon.SVG.Path.delete
                  ()
            ]) in
      [menu#widget]

    method private create_actions () : Widget.t list =
      let menu = Actions.make_overflow_menu
          Actions.[ Undo.undo undo_manager
                  ; Undo.redo undo_manager
                  ; make ~callback:(fun _ _ ->
                        match scaffold#side_sheet with
                        | None -> Lwt.return_unit
                        | Some sidesheet -> sidesheet#toggle ())
                      ~name:"Добавить виджет"
                      ~icon:Icon.SVG.Path.plus
                      ()
                  ] in
      [menu#widget]

    method private items_ ?(sort = false) () : Dom_html.element Js.t list =
      let get_position = Widget_utils.Attr.get_position in
      let items = Element.query_selector_all super#root Selector.item in
      if sort
      then
        List.sort (fun x y ->
            let compare f a b = match a, b with
              | None, None -> 0
              | Some a, Some b -> compare a b
              | None, Some _ -> -1
              | Some _, None -> 1 in
            compare Position.Normalized.compare (get_position x) (get_position y))
          items
      else items

    method private handle_keydown e _ =
      (* TODO Implement as described in
         https://www.w3.org/TR/wai-aria-practices/#layoutGrid *)
      Js.Opt.iter Dom_html.document##.activeElement (fun active ->
          let items = self#items_ ~sort:true () in
          match Dom_html.Keyboard_code.of_event e with
          (* Navigation keys *)
          | ArrowLeft -> ()
          | ArrowRight -> ()
          | ArrowDown -> ()
          | ArrowUp -> ()
          | PageUp -> () (* XXX optional *)
          | PageDown -> () (* XXX optional *)
          | Home -> ()
          | End -> ()
          (* Other keys *)
          | Enter | Space -> () (* XXX maybe move to the next layer here? *)
          | Delete ->
            self#remove_item active;
            (match items with
             | hd :: _ ->
               set_tab_index ~prev:active (Lazy.from_val items) hd;
               _focused_item <- Some hd
             | _ -> ())
          | _ -> ());
      Lwt.return_unit

    method private restore_top_app_bar_context ?hard () : unit =
      match _top_app_bar_context with
      | None -> ()
      | Some f -> f (); _top_app_bar_context <- None

    method private transform_top_app_bar items =
      let title = match items with
        | [] -> assert false
        | [x] -> Widget_utils.(title @@ snd @@ widget_of_element x)
        | x -> Printf.sprintf "Выбрано виджетов: %d" @@ List.length x in
      let restore = Actions.transform_top_app_bar
          ~title
          (* FIXME move class to some common module *)
          ~class_:Page_mosaic_editor_tyxml.Container_editor.CSS.top_app_bar_contextual
          ~actions:_selected_actions
          ~on_navigation_icon_click:(fun _ _ ->
              self#clear_selection ();
              Lwt.return_unit)
          scaffold in
      match _top_app_bar_context with
      | Some _ -> ()
      | None -> _top_app_bar_context <- Some restore

    method private selection : Selection.t =
      match _selection with
      | None -> failwith "`selection` instance is not initialized"
      | Some x -> x

    method private clear_selection () : unit =
      transform#root##.style##.visibility := Js.string "hidden";
      List.iter (fun x -> Element.remove_class x Selection.class_) self#selected;
      self#selection#deselect_all ();
      self#restore_top_app_bar_context ()

    method private selected : Dom_html.element Js.t list =
      self#selection#selected

    method private handle_selected (items : Dom_html.element Js.t list) =
      match items with
      | [] -> self#clear_selection ()
      | l ->
        let rect =
          Position.Normalized.bounding_rect
          @@ List.map Position.Normalized.of_element l in
        transform#root##.style##.visibility := Js.string "visible";
        Position.Normalized.apply_to_element rect transform#root;
        self#transform_top_app_bar l

    method private handle_transform_select e _ =
      Js.Opt.case e##.detail
        (fun () -> self#clear_selection ())
        (fun item ->
           Selection.on_select self#handle_selected
             item
             self#selection#selected
             self#selection);
      Lwt.return_unit

    method private handle_transform_action e _ =
      let target = Dom_html.eventTarget e in
      (match _focused_item with
       | None -> ()
       | Some x -> if not @@ Element.equal x target then x##blur);
      let detail = Widget.event_detail e in
      let siblings, items =
        List.split
        @@ Utils.List.filter_map (fun x ->
            if List.mem x self#selected
            then None else Some (Position.Absolute.of_element x, x))
          self#items in
      let original_positions = match _original_rects with
        | [] ->
          _original_rects <- List.map Position.Absolute.of_element self#selected;
          _original_rects
        | l -> l in
      let shift_key = Js.Opt.case
          (Dom_html.CoerceTo.mouseEvent detail##.originalEvent)
          (fun () -> false)
          (fun e -> Js.to_bool e##.shiftKey) in
      let aspect_ratio = match shift_key, _transform_aspect with
        | false, Some _ -> _transform_aspect <- None; None
        | false, None -> None
        | true, (Some _ as x) -> x
        | true, None ->
          let rect = detail##.rect in
          let width = Js.Optdef.get rect##.width
              (fun () -> rect##.right -. rect##.left) in
          let height = Js.Optdef.get rect##.height
              (fun () -> rect##.bottom -. rect##.top) in
          let aspect =
            Some (Utils.resolution_to_aspect
                    (int_of_float width,
                     int_of_float height)) in
          _transform_aspect <- aspect;
          aspect in
      let parent_size = self#size in
      let frame_position = Position.Absolute.of_client_rect detail##.rect in
      let frame, adjusted, lines =
        Position.Absolute.adjust
          ?aspect_ratio
          ~min_width:min_size
          ~grid_step:(float_of_int grid_overlay#size)
          ~min_height:min_size
          ~snap_lines:grid_overlay#snap_lines_visible
          ~action:(match detail##.action with
              | Move -> `Move
              | Resize -> `Resize detail##.direction)
          ~siblings
          ~parent_size
          ~frame_position
          original_positions
      in
      List.iter2 (fun (x : Position.Absolute.t) (item : Dom_html.element Js.t) ->
          let pos = Position.absolute_to_normalized ~parent_size x in
          Position.Normalized.apply_to_element pos item;
          Widget_utils.Attr.set_position pos item)
        adjusted self#selected;
      Position.Normalized.apply_to_element
        (Position.absolute_to_normalized ~parent_size frame)
        target;
      grid_overlay#set_snap_lines lines;
      Lwt.return_unit

    method private handle_transform_change e _ =
      (* let target = Dom_html.eventTarget e in *)
      _original_rects <- [];
      grid_overlay#set_snap_lines [];
      (* let position =
       *   Position.absolute_to_normalized ~parent_size:self#size
       *   @@ Position.Absolute.of_client_rect
       *   @@ Widget.event_detail e in
       * Widget_utils.Attr.set_position target position; *)
      Lwt.return_unit

    method private handle_dropped_json (json : Yojson.Safe.t) : unit Lwt.t =
      let of_yojson = function
        | `List [`String id; json] ->
          begin match Wm.widget_of_yojson json with
            | Ok x -> id, x
            | Error e -> failwith e
          end
        | _ -> failwith "failed to parse json" in
      let position = Some (
          Position.absolute_to_normalized
            ~parent_size:self#size
            (Position.Absolute.of_element ghost)) in
      let (id, widget) = of_yojson json in
      self#add_item (id, { widget with position });
      grid_overlay#set_snap_lines [];
      Lwt.return_unit

    method private move_ghost :
      'a. ?aspect:int * int -> (#Dom_html.event as 'a) Js.t -> unit =
      fun ?aspect event ->
      Dom.preventDefault event;
      (* FIXME too expensive to call getBoundingClientRect every time *)
      let rect = super#root##getBoundingClientRect in
      let (x, y) = Transform.get_cursor_position event in
      let point = x -. rect##.left, y -. rect##.top in
      let (position : Position.Absolute.t) =
        { x = fst point
        ; y = snd point
        ; w = 100. (* FIXME *)
        ; h = 100. (* FIXME *)
        } in
      (* let position = match aspect with
       *   | None -> position
       *   | Some aspect -> Position.fix_aspect position aspect in *)
      let parent_size = self#size in
      let _, adjusted, lines =
        Position.Absolute.adjust
          ?aspect_ratio:None
          ~min_width:min_size
          ~min_height:min_size
          ~grid_step:(float_of_int grid_overlay#size)
          ~snap_lines:grid_overlay#snap_lines_visible
          ~action:`Move
          ~siblings:(Utils.List.filter_map (fun x ->
              if Element.equal x ghost
              then None else Some (Position.Absolute.of_element x))
              self#items)
          ~parent_size
          ~frame_position:position
          [position]
      in
      Position.Absolute.apply_to_element (List.hd adjusted) ghost;
      grid_overlay#set_snap_lines lines

    method private bring_to_front (selected : Dom_html.element Js.t list) : unit =
      let open Widget_utils in
      let upper_selected_z = succ @@ Z_index.max_selected selected in
      Z_index.pack
      @@ List.sort (fun (a : Z_index.item) b ->
          match a.selected, b.selected with
          | false, true -> if a.z_index > upper_selected_z then 1 else -1
          | true, false -> if b.z_index > upper_selected_z then -1 else 1
          | true, true | false, false -> compare a.z_index b.z_index)
      @@ Z_index.make_item_list ~selected self#items

    method private send_to_back (selected : Dom_html.element Js.t list) : unit =
      let open Widget_utils in
      let first_selected_z = pred @@ Z_index.min_selected selected in
      Z_index.pack
      @@ List.sort (fun (a : Z_index.item) b ->
          match a.selected, b.selected with
          | false, true -> if a.z_index < first_selected_z then -1 else 1
          | true, false -> if b.z_index < first_selected_z then 1 else -1
          | true, true | false, false -> compare a.z_index b.z_index)
      @@ Z_index.make_item_list ~selected self#items

  end

let make ~(scaffold : Scaffold.t)
    ~(list_of_widgets : List_of_widgets.t)
    ~(resolution : int * int)
    ~(position : Wm.position)
    widgets =
  let items = match widgets with
    | `Nodes x ->
      List.map (fun (x : Dom_html.element Js.t) ->
          let _, widget = Widget_utils.widget_of_element x in
          let item = Tyxml_js.To_dom.of_element @@ Markup.create_item widget in
          Widget_utils.copy_attributes x item;
          Widget_utils.Z_index.set widget.layer item;
          (match Widget_utils.Attr.get_position item with
           | None -> ()
           | Some x -> Position.Normalized.apply_to_element x item);
          item) x
    | `Data x ->
      let parent_size = position.w, position.h in
      List.map (make_item ~parent_size) x in
  let content =
    Markup.create_overlay ()
    :: Markup.create_ghost ()
    :: List.map Tyxml_js.Of_dom.of_element items in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ~content () in
  new t ~list_of_widgets ~resolution ~position scaffold elt ()
