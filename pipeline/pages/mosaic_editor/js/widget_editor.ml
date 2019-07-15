open Js_of_ocaml
open Js_of_ocaml_tyxml
open Pipeline_types
open Components

type event =
  [ `Container of Wm.container
  ]

let ( >>= ) = Lwt.bind

let widget_of_yojson =
  Util_json.(Pair.of_yojson String.of_yojson Wm.widget_of_yojson)

include Page_mosaic_editor_tyxml.Widget_editor
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Selector = struct
  let item = Printf.sprintf ".%s" CSS.grid_item
  let grid_overlay = Printf.sprintf ".%s" CSS.grid_overlay
  let grid_ghost = Printf.sprintf ".%s" CSS.grid_ghost
  let parent = Printf.sprintf ".%s" Card.CSS.media
end

let widget_type_to_string : Wm.widget_type -> string = function
  | Video -> "video"
  | Audio -> "audio"

let compare_pair o_x o_y (x1, y1) (x2, y2) =
  let c = o_x x1 x2 in
  if c = 0
  then o_y y1 y2
  else c

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

let make_item_icon (widget : Wm.widget) =
  let path = match widget.type_ with
    | Video -> Icon.SVG.Path.video
    | Audio -> Icon.SVG.Path.music in
  Icon.SVG.make_simple path

let make_item_content (widget : Wm.widget) =
  let pid = match widget.pid with
    | None -> None
    | Some pid ->
      let text = Printf.sprintf "PID: %d" pid in
      Some (Typography.Text.make text)#markup in
  let ( ^:: ) x l = match x with None -> l | Some x -> x :: l in
  let text = Typography.Text.make widget.description in
  let icon = make_item_icon widget in
  Tyxml_js.To_dom.of_element
  @@ Tyxml_js.Html.(
      div ~a:[a_class [CSS.grid_item_content]]
        (icon#markup :: (pid ^:: [text#markup])))

let make_item ~parent_size (id, widget : string * Wm.widget) =
  let item = Resizable.make ~classes:[CSS.grid_item] () in
  item#root##.id := Js.string id;
  Widget_utils.set_attributes ~parent_size item#root widget;
  Element.append_child item#root (make_item_content widget);
  item

module Selection = struct
  include Selection

  let selectables = [Query Selector.item]

  let boundaries = [Query Selector.parent]

  let validate_start = fun e ->
    Js.Opt.case (Dom_html.CoerceTo.mouseEvent e)
      (fun () -> true)
      (fun e -> e##.button = 0)

  let make handle_selected =
    make ~validate_start
      ~selectables
      ~boundaries
      ~start_areas:boundaries
      ()
end

let aspect_of_wm_position (p : Wm.position) =
  Utils.resolution_to_aspect ((p.right - p.left), (p.bottom - p.top))

module Util = struct

  (* need functions:*)
  let get_z_index (elt : Dom_html.element Js.t) : int =
    Widget_utils.layer_of_element elt

  let set_z_index (elt : Dom_html.element Js.t) (z : int) : unit =
    elt##.style##.zIndex := Js.string (string_of_int z)

  let rec is_z_in_list
      (is_in : bool) (* init false *)
      (z : int)
      (z_list : int list) =
    match z_list with
    | [] -> is_in
    | hd :: tl ->  is_z_in_list (is_in || (z = hd)) z tl

  let rec create_all_z_list
      (acc : (int * (Dom_html.element Js.t) * bool ) list)  (* (z *
                                                               one of all_items *
                                                               true if it's selected item)
                                                            *)
      (all_items : Dom_html.element Js.t list)
      (selected_items_z : int list) =
    match all_items with
    | [] -> acc
    | hd :: tl ->
      let z = get_z_index hd in
      let acc = (z, hd, is_z_in_list false z selected_items_z) :: acc in
      create_all_z_list acc tl selected_items_z

  let rec pack_list (zib_items  : (int * (Dom_html.element Js.t) * bool ) list) =
    List.mapi (fun cnt (_, i, b) -> (cnt + 1, i, b)) zib_items

  let rec get_upper_selected_z
      (counter : int) (* initial 1 *)
      (selected_list_len : int)
      (zib_items  : (int * (Dom_html.element Js.t) * bool ) list)
    : int =
    match zib_items with
    | [] -> -1
    | hd :: tl -> let (_, _, b) = hd in
      if counter = selected_list_len
      then counter
      else get_upper_selected_z
          (if b then (counter + 1) else counter)
          selected_list_len tl

  let rec get_first_selected_z
      (zib_items  : (int * (Dom_html.element Js.t) * bool ) list)
    : int =
    match zib_items with
    | [] -> ( -1)
    | hd :: tl ->
      let (z, _, b) = hd in
      if b then z else get_first_selected_z tl

  (* separate selected and not selected items,
     assign z numbers continuosly*)
  let rec separate_selected
      (acc  : (int * (Dom_html.element Js.t) * bool ) list)
      (is_selected : bool)
      (z_begin : int)
      (z_end : int)
      (zib_items  : (int * (Dom_html.element Js.t) * bool ) list) =
    match zib_items with
    | [] -> acc
    | hd :: tl -> let (z, _, b) = hd in
      let acc = if b = is_selected && z_begin >= z && z_end < z
        then hd :: acc
        else acc in
      separate_selected acc is_selected z_begin z_end tl
end

class t
    ~(items : Resizable.t list)
    ~(list_of_widgets : List_of_widgets.t)
    ~(position : Wm.position)
    (scaffold : Scaffold.t)
    elt
    () =
  object(self)

    inherit Drop_target.t elt () as super

    val aspect =
      let w, h = aspect_of_wm_position position in
      float_of_int w /. float_of_int h
    val grid_overlay = match Element.query_selector elt Selector.grid_overlay with
      | None -> failwith "widget-editor: grid overlay element not found"
      | Some x ->
        let show_grid_lines = Storage.(get_bool ~default:true show_grid_lines) in
        let show_snap_lines = Storage.(get_bool ~default:true show_snap_lines) in
        Grid_overlay.attach ~show_grid_lines ~show_snap_lines ~size:10 x
    val ghost = match Element.query_selector elt Selector.grid_ghost with
      | None -> failwith "widget-editor: grid ghost element not found"
      | Some x -> x
    val undo_manager = Undo_manager.create ()

    val mutable format = List_of_widgets.format
    val mutable _items = items
    val mutable _listeners = []
    val mutable _focused_item = None
    val mutable min_size = 20

    val mutable _selection = None

    val mutable _basic_actions = []
    val mutable _widget_selected_actions = []

    method! init () : unit =
      super#init ();
      _selection <- Some (Selection.make (fun _ -> ()));
      _basic_actions <- self#create_actions ()

    method! initial_sync_with_dom () : unit =
      _listeners <- Events.(
          [ Resizable.Event.inputs super#root self#handle_item_action
          ; Resizable.Event.changes super#root self#handle_item_change
          ; Resizable.Event.selects super#root self#handle_item_selected
          ; keydowns super#root self#handle_keydown
          ]);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel _listeners; _listeners <- [];
      List.iter Widget.destroy _items; _items <- [];
      Utils.Option.iter Widget.destroy _selection;
      _selection <- None;
      super#destroy ()

    method! layout () : unit =
      let cur_w, cur_h, cur_aspect =
        Js.Opt.case (Element.get_parent super#root)
          (fun () -> 0., 0., 1.)
          (fun x ->
             let width = float_of_int x##.offsetWidth in
             let height = float_of_int x##.offsetHeight in
             width, height, width /. height) in
      let w = float_of_int @@ position.right - position.left in
      let h = float_of_int @@ position.bottom - position.top in
      let scale_factor = if cur_aspect > aspect then cur_h /. h else cur_w /. w in
      let width' = w *. scale_factor in
      let height' = h *. scale_factor in
      super#root##.style##.width := Js.string (Printf.sprintf "%gpx" width');
      super#root##.style##.height := Js.string (Printf.sprintf "%gpx" height');
      List.iter Widget.layout _items;
      grid_overlay#layout ();
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
      let parent_size =
        float_of_int @@ position.right - position.left,
        float_of_int @@ position.bottom - position.top in
      { position
      ; widgets = List.map (Widget_utils.widget_of_element ~parent_size) self#items
      }

    method actions : Widget.t list =
      _basic_actions

    (* Private methods *)

    method private size : float * float =
      float_of_int elt##.offsetWidth,
      float_of_int elt##.offsetHeight

    method private add_item_ id item (position : Position.t) =
      list_of_widgets#remove_by_id id;
      Dom.appendChild super#root item;
      Position.apply_to_element position item;
      self#set_position_attributes item position

    (** Add item with undo *)
    method private add_item w p =
      let item = make_item ~parent_size:self#size w in
      self#add_item_ (fst w) item#root p;
      Undo_manager.add undo_manager
        { undo = (fun () -> self#remove_item_ item#root)
        ; redo = (fun () -> self#add_item_ (fst w) item#root p)
        }

    method private remove_item_ (item : Dom_html.element Js.t) =
      list_of_widgets#append_item @@ Widget_utils.widget_of_element item;
      Element.remove_child_safe super#root item;
      _items <- List.filter (fun (x : Resizable.t) ->
          let b = Element.equal item x#root in
          if b then x#destroy ();
          not b) _items

    (** Remove item with undo *)
    method private remove_item item =
      let id = Js.to_string item##.id in
      let position = Position.of_element item in
      self#remove_item_ item;
      Undo_manager.add undo_manager
        { undo = (fun () -> self#add_item_ id item position)
        ; redo = (fun () -> self#remove_item_ item)
        }

    method private create_actions () : Widget.t list =
      let menu = Actions.make_overflow_menu
          (fun () -> self#selected)
          Actions.[ undo undo_manager
                  ; redo undo_manager
                  ; add_widget scaffold
                  ] in
      [menu#widget]

    method private selected = []

    method private items_ ?(sort = false) () : Dom_html.element Js.t list =
      let get_position = Widget_utils.Attr.get_position ~parent_size:self#size in
      let items = Element.query_selector_all super#root Selector.item in
      if sort
      then
        List.sort (fun x y ->
            let pos_x, pos_y = get_position x, get_position y in
            compare_pair compare compare
              (pos_x.left, pos_x.top)
              (pos_y.left, pos_y.top))
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

    method private handle_item_selected e _ =
      let target = Dom_html.eventTarget e in
      target##focus;
      set_tab_index ?prev:_focused_item (Lazy.from_fun self#items_) target;
      _focused_item <- Some target;
      Lwt.async (fun () ->
          Events.blur target
          >>= fun _ -> (* TODO do smth *)
          Lwt.return_unit);
      Lwt.return_unit

    method private handle_item_action e _ =
      let target = Dom_html.eventTarget e in
      (match _focused_item with
       | None -> ()
       | Some x -> if not @@ Element.equal x target then x##blur);
      let detail = Widget.event_detail e in
      let position = Position.of_client_rect detail##.rect in
      let original_position = Position.of_client_rect detail##.originalRect in
      let parent_size = self#size in
      let adjusted, lines =
        Position.adjust
          ?aspect_ratio:(Widget_utils.Attr.get_aspect target)
          ~min_width:min_size
          ~min_height:min_size
          ~snap_lines:grid_overlay#snap_lines_visible
          ~action:(match detail##.action with
              | Move -> `Move
              | Resize -> `Resize detail##.direction)
          ~position
          ~original_position
          ~siblings:self#items
          ~parent_size
          target
      in
      let adjusted = Position.to_relative ~parent_size adjusted in
      grid_overlay#set_snap_lines lines;
      Position.apply_to_element adjusted target;
      Lwt.return_unit

    (* TODO this is a next task *)
    method private handle_item_change e _ =
      let target = Dom_html.eventTarget e in
      grid_overlay#set_snap_lines [];
      let position =
        Position.to_relative ~parent_size:self#size
        @@ Position.of_client_rect
        @@ Widget.event_detail e in
      self#set_position_attributes target position;
      Lwt.return_unit

    method private set_position_attributes
        (elt : Dom_html.element Js.t)
        (pos : Position.t) =
      Widget_utils.Attr.set_position elt pos

    method private handle_dropped_json (json : Yojson.Safe.t) : unit Lwt.t =
      let of_yojson = function
        | `List [`String id; json] ->
          begin match Wm.widget_of_yojson json with
            | Ok x -> id, x
            | Error e -> failwith e
          end
        | _ -> failwith "failed to parse json" in
      let position = Position.to_relative
          ~parent_size:self#size
          (Position.of_element ghost) in
      self#add_item (of_yojson json) position;
      grid_overlay#set_snap_lines [];
      Lwt.return_unit

    method private move_ghost :
      'a. ?aspect:int * int -> (#Dom_html.event as 'a) Js.t -> unit =
      fun ?aspect event ->
      (* FIXME too expensive to call getBoundingClientRect every time *)
      let rect = super#root##getBoundingClientRect in
      let (x, y) = Resizable.get_cursor_position event in
      let point = x -. rect##.left, y -. rect##.top in
      let position =
        { Position.
          x = fst point
        ; y = snd point
        ; w = 100. (* FIXME *)
        ; h = 100. (* FIXME *)
        } in
      let position = match aspect with
        | None -> position
        | Some aspect -> Position.fix_aspect position aspect in
      Dom.preventDefault event;
      let parent_size = self#size in
      let adjusted, lines =
        Position.adjust
          ?aspect_ratio:None
          ~min_width:min_size
          ~min_height:min_size
          ~snap_lines:grid_overlay#snap_lines_visible
          ~action:`Move
          ~position
          ~original_position:position
          ~siblings:self#items
          ~parent_size
          ghost
      in
      let adjusted = Position.to_relative ~parent_size adjusted in
      grid_overlay#set_snap_lines lines;
      Position.apply_to_element adjusted ghost

    method private bring_to_front (items : Dom_html.element Js.t list) : unit =
      let z_selected_items = List.map Util.get_z_index items in
      let all_items = self#items in
      let zib_all_list = Util.create_all_z_list [] all_items z_selected_items in
      let zib_all_list_sorted = List.sort
          (fun
            (x : (int * (Dom_html.element Js.t) * bool ))
            (y : (int * (Dom_html.element Js.t) * bool )) ->
            let (z1, _, _) = x in
            let (z2, _, _) = y in
            compare z1 z2) zib_all_list in
      let zib_all_list_packed = Util.pack_list zib_all_list_sorted in
      let upper_selected_z = Util.get_upper_selected_z
          1 (List.length z_selected_items) zib_all_list_packed in
      let insert_position_z = upper_selected_z + 1 - (List.length z_selected_items) in
      let all_zib_list_result =
        if insert_position_z <= 0 || insert_position_z > (List.length zib_all_list_packed)
        then zib_all_list_packed
        else
          let zib_non_selected_begin = Util.separate_selected
              [] false 1 upper_selected_z zib_all_list_packed in
          let zib_selected = Util.separate_selected
              [] true 1 (List.length zib_all_list_packed) zib_all_list_packed in
          let zib_non_selected_end =
            Util.separate_selected
              [] false
              (upper_selected_z + 1) (List.length zib_all_list_packed)
              zib_all_list_packed in
          Util.pack_list
            (List.append zib_non_selected_begin (List.append zib_selected zib_non_selected_end))
      in
      List.iter (fun (z, i, _) -> Util.set_z_index i z) all_zib_list_result

    method private send_to_back (items : Dom_html.element Js.t list) : unit =
      let z_selected_items = List.map Util.get_z_index items in
      let all_items = self#items in
      let zib_all_list = Util.create_all_z_list [] all_items z_selected_items in
      let zib_all_list_sorted = List.sort
          (fun
            (x : (int * (Dom_html.element Js.t) * bool ))
            (y : (int * (Dom_html.element Js.t) * bool )) ->
            let (z1, _, _) = x in
            let (z2, _, _) = y in
            compare z1 z2) zib_all_list in
      let zib_all_list_packed = Util.pack_list zib_all_list_sorted in
      let first_selected_z = Util.get_first_selected_z zib_all_list_packed in
      let insert_position_z = first_selected_z - 1 in
      let all_zib_list_result =
        if insert_position_z <= 0 || insert_position_z > (List.length zib_all_list_packed)
        then zib_all_list_packed
        else
          let zib_non_selected_begin = Util.separate_selected
              [] false 1 first_selected_z zib_all_list_packed in
          let zib_selected = Util.separate_selected
              [] true 1 (List.length zib_all_list_packed) zib_all_list_packed in
          let zib_non_selected_end = Util.separate_selected
              [] false
              (first_selected_z + 1) (List.length zib_all_list_packed)
              zib_all_list_packed in
          Util.pack_list
            (List.append zib_non_selected_begin (List.append zib_selected zib_non_selected_end))
      in
      List.iter (fun (z, i, _) -> Util.set_z_index i z) all_zib_list_result

  end

let make ~(scaffold : Scaffold.t)
    ~(list_of_widgets : List_of_widgets.t)
    ~(position : Wm.position)
    widgets =
  let items = match widgets with
    | `Nodes x ->
      List.map (fun (x : Dom_html.element Js.t) ->
          let item = Resizable.make ~classes:[CSS.grid_item] () in
          item#root##.id := x##.id;
          Widget_utils.copy_attributes x item#root;
          let _, widget = Widget_utils.widget_of_element x in
          let pos = Widget_utils.Attr.get_relative_position item#root in
          Dom.appendChild item#root (make_item_content widget);
          Position.apply_to_element ~unit:`Pc pos item#root;
          item) x
    | `Data x ->
      let parent_size =
        float_of_int @@ position.right - position.left,
        float_of_int @@ position.bottom - position.top in
      List.map (make_item ~parent_size) x in
  let content =
    Markup.create_grid_overlay ()
    :: Markup.create_grid_ghost ()
    :: List.map Widget.to_markup items in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.create_grid ~content () in
  new t ~items ~list_of_widgets ~position scaffold elt ()
