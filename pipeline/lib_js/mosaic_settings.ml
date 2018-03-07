open Containers
open Components
open Requests
open Lwt_result.Infix

type editor_config =
  { show_grid_lines : bool
  }

type action =
  { icon   : string
  ; name   : string
  }

type add_candidate =
  { icon : string
  ; typ  : string
  ; name : string
  } [@@deriving yojson]

let drag_data_type = "add_candidate"

module type Item = sig

  type item

  val create_item     : add_candidate -> item
  val pos_of_item     : item -> Wm.position
  val layer_of_item   : item -> int
  val update_pos      : item -> Wm.position -> item
  val update_layer    : item -> int -> item

  val max_layers      : int
  val add_candidates  : add_candidate list
  val make_item_name  : add_candidate -> int -> string
  val make_item_props : (string * item) Dynamic_grid.Item.t -> Widget.widget

end

module Container_item : Item with type item = Wm.container = struct

  type item = Wm.container

  let max_layers     = 1
  let add_candidates = [ { icon = "crop_16_9"; name = "Контейнер"; typ = "container" } ]

  let create_item _ : item = { position = { left=0;top=0;right=0;bottom=0 }
                             ; widgets  = []
                             }
  let pos_of_item (item : item) = item.position
  let layer_of_item _           = 0
  let update_pos  (item : item) (pos : Wm.position) = { item with position = pos }
  let update_layer (item : item) _ = item
  let make_item_name (ac : add_candidate) (index : int) =
    Printf.sprintf "%s #%d" ac.name index
  let make_item_props (item: (string * item) Dynamic_grid.Item.t) =
    let (name,cont) = item#get_value in
    let name        = Printf.sprintf "Имя: %s" name in
    let num         = Printf.sprintf "Количество виджетов: %d" @@ List.length cont.widgets in
    let name_w      = new Typography.Text.t ~text:name () in
    let num_w       = new Typography.Text.t ~text:num () in
    let name_i      = new Textfield.t ~label:"Имя" ~input_type:Text () in
    let apply       = new Button.t ~label:"Применить" () in
    let box         = new Box.t ~vertical:true ~widgets:[ name_w#widget
                                                        ; num_w#widget
                                                        ; name_i#widget
                                                        ; apply#widget] () in
    let ()          = (Js.Unsafe.coerce apply#style)##.alignSelf := Js.string "flex-end" in
    let _           = React.E.map (fun _ -> match React.S.value name_i#s_input with
                                            | None   -> ()
                                            | Some s -> item#set_value (s,cont))
                                  apply#e_click
    in
    box#widget


end

module Widget_item : Item with type item = Wm.widget = struct

  type item = Wm.widget
  let max_layers     = 10
  let add_candidates = [ { icon = "tv"; name = "Видео"; typ = "video" }
                       ; { icon = "audiotrack"; name = "Аудио"; typ = "audio" }
                       ; { icon = "font_download"; name = "Текст"; typ = "text" }
                       ]

  let create_item ac : item = { type_       = ac.typ
                              ; domain      = ""
                              ; position    = { left=0;top=0;right=0;bottom=0 }
                              ; layer       = 1
                              ; aspect      = (1,1)
                              ; description = ""
                              }
  let layer_of_item (item : item) = item.layer
  let pos_of_item (item : item) = item.position
  let update_layer (item : item) (layer : int) = { item with layer }
  let update_pos  (item : item) (pos : Wm.position) = { item with position = pos }
  let make_item_name (ac : add_candidate) (index : int) =
    Printf.sprintf "%s #%d" ac.name index
  let make_item_props _ =
    let box         = new Box.t ~vertical:true ~widgets:[] () in
    box#widget

end

let make_placeholder ?action ~text ~icon () =
  let _class        = "wm-placeholder" in
  let content_class = Markup.CSS.add_element _class "content"   in
  (* let bordered_class  = Markup.CSS.add_modifier _class     "bordered"    in *)

  let ph  = Dom_html.createDiv Dom_html.document |> Widget.create in
  let txt = new Typography.Text.t ~adjust_margin:false ~text () in
  let ico = match action with
    | Some _ -> let ico = new Icon.Button.Font.t ~icon () in
                let ()  = Option.iter (fun f -> f ico#e_click |> ignore) action in
                ico#widget
    | None   -> let ico = new Icon.Font.t ~icon () in
                ico#widget
  in
  let box = new Box.t ~widgets:[txt#widget;ico#widget] () in

  let ()  = box#set_align_items `Center in
  let ()  = box#set_justify_content `Center in
  let ()  = box#add_class content_class in
  let ()  = ph#add_class _class in
  let ()  = Dom.appendChild ph#root box#root in
  ph

let rm_children container =
  Dom.list_of_nodeList @@ container##.childNodes
  |> List.iter (fun x -> Dom.removeChild container x)

module Selectable_title = struct

  let base_class = "wm-selectable-title"

  class title ~title ~widget () =
    let title_class  = Markup.CSS.add_element  base_class  "title"  in
    let active_class = Markup.CSS.add_modifier title_class "active" in
    object(self)

      inherit Typography.Text.t ~adjust_margin:false ~text:title ~font:Subheading_2 ()

      method set_active x =
        self#add_or_remove_class x active_class;
        widget#style##.display := Js.string (if x then "" else "none")

      method get_title = title

      initializer
        self#add_class title_class

    end

  class t titles () =
    let (titles : title list) =
      List.map (fun (title,widget) -> new title ~title ~widget ()) titles in
    object(self)

      inherit Box.t ~vertical:false ~widgets:titles ()

      method titles : title list = titles
      method select (w:title) =
        List.iter (fun x -> if not @@ Equal.physical x#root w#root then x#set_active false) titles;
        w#set_active true
      method select_by_name n =
        match List.find_opt (fun x -> String.equal x#get_title n) self#titles with
        | None   -> ()
        | Some x -> self#select x

      initializer
        self#add_class base_class;
        let open Dom_events in
        let _ = List.map (fun w -> listen w#root Typ.click (fun _ _ -> self#select w; false)) self#titles in
        match self#titles with
        | []           -> failwith "Titles must not be empty"
        | [x] | x :: _ -> self#select x

    end

  let make titles = new t titles ()

end

module Layers = struct

  let make_layer_item positions =
    let _class            = "wm-layer-item" in
    let drag_handle_class = Markup.CSS.add_element _class "drag-handle" in

    let open Dynamic_grid.Position in
    let y = match List.rev positions with
      | []    -> 0
      | hd::_ -> hd.y + 1
    in
    let drag  = new Icon.Font.t ~icon:"drag_handle" () in
    let text  = new Typography.Text.t ~text:(Printf.sprintf "Слой %d" (y + 1)) () in
    let box   = new Box.t ~vertical:false ~widgets:[text#widget; drag#widget] () in
    let pos   = { x = 0; y; w = 1; h = 1 } in
    let item  = Dynamic_grid.Item.to_item ~pos ~move_widget:drag#widget ~widget:box#widget
                                          ~resizable:false ~selectable:true ~value:y ()
    in
    let ()    = drag#add_class drag_handle_class in
    let ()    = box#set_justify_content `Space_between in
    let ()    = box#add_class _class in
    item

  let move_layer_up layers layer =
    let open Dynamic_grid.Position in
    let pos = layer#pos in
    if pos.y <> 0
    then (let upper = List.find_opt (fun x -> x#pos.y = pos.y - 1) layers in
          Option.iter (fun x -> let new_pos = x#pos in x#set_pos pos; layer#set_pos new_pos) upper)

  let move_layer_down layers layer =
    let open Dynamic_grid.Position in
    let pos = layer#pos in
    if pos.y <> List.length layers - 1
    then (let lower = List.find_opt (fun x -> x#pos.y = pos.y + 1) layers in
          Option.iter (fun x -> let new_pos = x#pos in x#set_pos pos; layer#set_pos new_pos) lower)

  let make_layers_grid ~init =
    let _class = "wm-layers-grid" in
    let props  = Dynamic_grid.to_grid ~cols:1 ~min_col_width:20 ~row_height:50 ~vertical_compact:true
                                     ~restrict_move:true ()
    in
    let map    = Fun.(List.map (fun (x:'a Dynamic_grid.item) -> x.pos) %> List.rev) in
    let items  = List.fold_left (fun acc _ -> (make_layer_item @@ map acc) :: acc) [] @@ List.range' 0 init
                 |> List.rev in
    let grid   = new Dynamic_grid.t ~grid:props ~items () in

    let eq       = (fun x1 x2 -> Dynamic_grid.Position.equal x1#pos x2#pos) in
    let s_change = let m a x = x :: a in
                   let map x = React.S.map ~eq:(fun _ _ -> false)
                                           (fun _ -> print_endline "pos changed!"; x) x#s_change in
                   React.S.map ~eq:(fun _ _ -> false)
                               (fun l -> React.S.merge ~eq:(fun _ _ -> false) m [] (List.map map l))
                               grid#s_items
                   |> React.S.switch ~eq:(fun _ _ -> false)
    in
    let e      = React.S.diff (fun n o ->
                     let open Dynamic_grid.Position in
                     let np = List.fold_left (fun acc x ->
                                  let op,np = x#get_value,x#pos.y in
                                  Printf.printf "op:%d, np:%d\n" op np;
                                  if op <> np
                                  then (x#set_value np; (`Change (op,np)) :: acc)
                                  else acc) [] n in
                     np)
                              s_change in
    let _  = React.E.map (fun x -> List.iter (function
                                              | `Change (o,n) -> Printf.printf "old: %d, new: %d\n" o n) x) e
    in
    let ()     = grid#set_on_load @@ Some (fun () -> grid#layout) in
    grid

  let make_layers_actions max layers_grid =
    let _class = "wm-layers-actions" in
    let add    = new Icon.Button.Font.t ~icon:"add_box" () in
    let rm     = new Icon.Button.Font.t ~icon:"delete" () in
    let up     = new Icon.Button.Font.t ~icon:"arrow_upward" () in
    let down   = new Icon.Button.Font.t ~icon:"arrow_downward" () in
    let icons  = new Card.Actions.Icons.t ~widgets:[down#widget;up#widget;add#widget;rm#widget] () in
    let ()     = icons#add_class _class in
    (* Actions with layers *)
    let s_sel  = React.S.map (function [x] -> Some x | _ -> None) layers_grid#s_selected in
    let a_map (a,f) = React.E.map (fun _ -> Option.iter f @@ React.S.value s_sel) a#e_click in
    let _           = React.S.l2 (fun s l -> let len = List.length l in
                                             let sel = Option.is_some s in
                                             add#set_disabled  (len >= max);
                                             up#set_disabled   ((len <= 1) || not sel);
                                             down#set_disabled ((len <= 1) || not sel);
                                             rm#set_disabled   ((len <= 1) || not sel))
                                 s_sel layers_grid#s_items in
    let _           = React.E.map (fun _ -> layers_grid#add @@ make_layer_item
                                            @@ List.map (fun x -> x#pos) layers_grid#items) add#e_click in
    let l           = [ rm,   (fun w ->
                          let open Dynamic_grid.Position in
                          let y     = w#pos.y in
                          w#remove;
                          let items = layers_grid#items in
                          match List.find_pred (fun w -> w#pos.y = y) items with
                          | Some w -> w#set_selected true
                          | None   -> (match List.find_pred (fun w -> w#pos.y = y - 1) items with
                                       | Some w -> w#set_selected true
                                       | None   -> (match items with
                                                    | hd::_ -> hd#set_selected true
                                                    | _     -> ())))
                      ; up,   (fun w -> move_layer_up   layers_grid#items w)
                      ; down, (fun w -> move_layer_down layers_grid#items w)
                      ]
    in
    let _           = List.map a_map l in
    icons

  let make ~init ~max ~s_layer_push =
    let _class        = "wm-layers-card"  in
    let wrapper_class = "wm-layers-grid-wrapper" in

    let open Dynamic_grid.Position in
    let layers  = Dom_html.createDiv Dom_html.document |> Widget.create in
    let grid    = make_layers_grid ~init in
    let actions = new Card.Actions.t ~widgets:[(make_layers_actions max grid)#widget] () in
    let card    = new Card.t ~widgets:[layers#widget;actions#widget] () in
    let ()      = Option.iter (fun x -> x#set_selected true) @@ List.head_opt grid#items in
    let ()      = layers#add_class wrapper_class in
    let ()      = Dom.appendChild layers#root grid#root in
    let ()      = card#add_class _class in
    let title   = Selectable_title.make ["Слои",card] in
    let box     = new Box.t ~widgets:[title#widget; card#widget] () in
    let _       = React.S.map (function
                               | [x] -> s_layer_push x#pos.y
                               | _   -> ()) grid#s_selected
    in
    box

end

module Items(I : Item) = struct

  let base_class = "wm-items"

  module Add = struct

    let base_class = Markup.CSS.add_element base_class "add"
    let item_class = Markup.CSS.add_element base_class "item"

    class item ~props ~widgets () = object(self)

      inherit Box.t ~vertical:false ~widgets ()

      initializer
        Dom_events.listen self#root Dom_events.Typ.dragstart
                          (fun _ e -> let s = add_candidate_to_yojson props
                                              |> Yojson.Safe.to_string
                                              |> Js.string
                                      in
                                      e##.dataTransfer##setData (Js.string drag_data_type) s;
                                      self#style##.opacity := Js.def @@ Js.string "0.5";
                                      self#style##.zIndex := Js.string "5";
                                      true)
        |> ignore;
        Dom_events.listen self#root Dom_events.Typ.dragend
                          (fun _ _ -> self#style##.opacity := Js.def @@ Js.string "";
                                      self#style##.zIndex := Js.string "";
                                      false)
        |> ignore;
        self#add_class item_class;
        self#set_attribute "draggable" "true"

    end

    let make_item (props : add_candidate) =

      let icon = new Icon.Font.t ~icon:props.icon () in
      let text = new Typography.Text.t ~adjust_margin:false ~text:props.name () in
      let box  = new item ~props ~widgets:[icon#widget;text#widget] () in
      box

    let make () =
      let items  = List.map (fun x -> x,make_item x) I.add_candidates in
      let box    = new Box.t ~widgets:(List.map snd items) () in
      let ()     = box#add_class base_class in
      box

  end

  module Properties = struct

    let base_class = Markup.CSS.add_element base_class "properties"

    let make (s : (string * I.item) Dynamic_grid.Item.t option React.signal) =
      let ph     = make_placeholder ~text:"Выберите элемент в раскладке" ~icon:"touch_app" () in
      let box    = new Box.t ~widgets:[ph] () in
      let id     = "wm-item-properties" in
      let ()     = box#add_class base_class in
      let _      = React.S.map (fun selected ->
                       (try Dom.removeChild box#root (Dom_html.getElementById id) with _ -> ());
                       (match selected with
                        | Some x -> (try Dom.removeChild box#root ph#root with _ -> ());
                                    let w = I.make_item_props x in
                                    w#set_id id;
                                    Dom.appendChild box#root w#root
                        | None   -> Dom.appendChild box#root ph#root))
                               s
      in
      box

  end

  let make selected =
    let add_title   = "Добавить" in
    let props_title = "Свойства" in
    let add     = Add.make () in
    let props   = Properties.make selected in
    let title   = Selectable_title.make [ add_title, add
                                        ; props_title, props ] in
    let card    = new Card.t ~widgets:[add#widget; props#widget] () in
    let ()      = card#add_class base_class in
    let box     = new Box.t ~vertical:true ~widgets:[title#widget; card#widget] () in
    let sel     = function
      | `Add   -> title#select_by_name add_title
      | `Props -> title#select_by_name props_title
    in
    box,sel

end

module Items_grid(I : Item) = struct

  let base_class = "wm-grid"

  let layout_pos_of_grid_pos ~resolution ~cols ~rows (pos:Dynamic_grid.Position.t) : Wm.position =
    let w,h    = resolution in
    let cw,rh  = w / cols, h / rows in
    { left   = pos.x * cw
    ; right  = (pos.w + pos.x) * cw
    ; top    = pos.y * rh
    ; bottom = (pos.h + pos.y) * rh
    }

  let grid_pos_of_layout_pos ~resolution ~cols ~rows (pos:Wm.position) : Dynamic_grid.Position.t =
    let w,h   = resolution in
    let cw,rh = w / cols, h / rows in
    { x = pos.left / cw
    ; y = pos.top / rh
    ; w = (pos.right - pos.left) / cw
    ; h = (pos.bottom - pos.top) / rh
    }

  let item_to_grid_item ~resolution ~cols ~rows x =
    let pos = I.pos_of_item (snd x) in
    let pos = grid_pos_of_layout_pos ~resolution ~cols ~rows pos in
    Dynamic_grid.Item.to_item ~pos ~value:x ()

  class drop_grid ~layer ~resolution ~cols ~rows ~init () =
    let _class  = Markup.CSS.add_element base_class "grid" in
    let ph      = make_placeholder ~text:"Добавьте элементы в раскладку" ~icon:"add_box" () in
    let grid    = Dynamic_grid.to_grid ~cols ~rows ~items_margin:(2,2) () in
    let items   = List.map (item_to_grid_item ~resolution ~cols ~rows) init in
    object(self)

      inherit [string * I.item] Dynamic_grid.t ~grid ~items ()

      val mutable layer        = layer
      val mutable enter_target = Js.null

      method layer : int = layer

      method private update_item_value item position =
        let pos            = layout_pos_of_grid_pos ~resolution ~cols ~rows position in
        let (s,(v:I.item)) = item#get_value in
        let (nv : I.item)  = I.update_pos v pos in
        item#set_value (s,nv)

      method private add_from_candidate index pos ac =
        let open Dynamic_grid in
        if not @@ Position.equal pos Position.empty
        then let item = Dynamic_grid.Item.to_item ~pos ~value:(I.make_item_name ac index,I.create_item ac) () in
             Result.iter (fun i -> React.S.map (fun p -> self#update_item_value i p) i#s_change |> ignore)
                         (self#add item)

      initializer
        React.S.map (function
                     | [] -> Dom.appendChild self#root ph#root
                     | _  -> try Dom.removeChild self#root ph#root with _ -> ())
                    self#s_items |> ignore;
        self#set_on_load @@ Some (fun () -> self#layout);
        self#add_class _class;
        List.iter (fun i -> React.S.map (fun p -> self#update_item_value i p) i#s_change |> ignore) self#items;
        (let ghost = new Dynamic_grid.Item.cell
                         ~typ:`Ghost
                         ~s_col_w:self#s_col_w
                         ~s_row_h:self#s_row_h
                         ~s_item_margin:self#s_item_margin
                         ~pos:Dynamic_grid.Position.empty
                         ()
         in
         let typ = Js.string drag_data_type in
         ghost#style##.zIndex := Js.string "10000";
         Dom.appendChild self#root ghost#root;
         Dom_events.listen self#root Dom_events.Typ.dragenter
                           (fun _ e -> Dom_html.stopPropagation e; Dom.preventDefault e;
                                       enter_target <- e##.target;
                                       true) |> ignore;
         Dom_events.listen self#root Dom_events.Typ.dragleave
                           (fun _ e -> Dom_html.stopPropagation e;Dom.preventDefault e;
                                       if Equal.physical enter_target e##.target
                                       then ghost#set_pos Dynamic_grid.Position.empty;
                                       true) |> ignore;
         Dom_events.listen self#root Dom_events.Typ.dragover
                           (fun _ e ->
                             if Js.to_bool @@ (Js.Unsafe.coerce e##.dataTransfer##.types)##includes typ
                             then (Dom.preventDefault e;
                                   let p = self#get_event_pos (e :> Dom_html.mouseEvent Js.t) in
                                   self#move_ghost ghost p);
                             true)
         |> ignore;
         Dom_events.listen self#root Dom_events.Typ.drop
                           (fun _ e -> Dom.preventDefault e;
                                       let json = e##.dataTransfer##getData typ
                                                  |> Js.to_string
                                                  |> Yojson.Safe.from_string
                                       in
                                       let index = succ @@ List.length self#items in
                                       Result.iter (self#add_from_candidate index ghost#pos)
                                                   (add_candidate_of_yojson json);
                                       ghost#set_pos Dynamic_grid.Position.empty;
                                       true)
         |> ignore)

    end

  class t ~title ~resolution ~cols ~rows ~s_layers ~s_layer ~(init: (string * I.item) list) () =
    let grouped =
      List.fold_left (fun acc (n,x) ->
          let layer = I.layer_of_item x in
          List.Assoc.update ~eq:(=) ~f:(function Some l -> Some ((n,x) :: l) | None -> Some [(n,x)]) layer acc)
                     [] init
    in
    let grids   = List.map (fun x -> let init = Option.get_or ~default:[] @@ List.Assoc.get ~eq:(=) x grouped in
                                     x,new drop_grid ~layer:x ~init ~cols ~rows ~resolution ())
                  @@ React.S.value s_layers in
    let s_active_grid,s_active_grid_push = React.S.create @@ snd @@ List.hd grids in
    let wrapper = Dom_html.createDiv Dom_html.document |> Widget.create in
    let title   = new Typography.Text.t ~font:Subheading_2 ~text:title () in
    object(self)

      inherit Box.t ~vertical:true ~widgets:[title#widget;wrapper#widget] ()

      val s_sel =
        let eq = fun _ _ -> false in
        React.S.map ~eq:Equal.physical
                    (fun x -> let s = React.S.hold ~eq [] x#e_selected in
                              React.S.map ~eq (function [x] -> Some x | _ -> None) s)
                    s_active_grid
        |> React.S.switch ~eq

      method grid       = React.S.value s_active_grid
      method s_selected = s_sel
      method items      = List.fold_left (fun acc (_,x) -> x#items @ acc) [] grids

      initializer
        React.S.map (fun x -> let grid = List.Assoc.get ~eq:(=) x grids in
                              print_endline "s layer";
                              match grid with
                              | Some grid -> s_active_grid_push grid
                              | None      -> ()) s_layer |> ignore;
        React.S.map ~eq:Equal.physical (fun grid -> print_endline "s active grid";
                                 rm_children wrapper#root;
                                 Dom.appendChild wrapper#root grid#root) s_active_grid |> ignore;
        self#add_class base_class;
        self#set_justify_content `Center;
        wrapper#add_class @@ Markup.CSS.add_element base_class "wrapper"

    end

  let make ~title
           ~resolution
           ~cols
           ~rows
           ~(init: (string * I.item) list)
           ~s_layer
           () =
    new t ~title ~resolution ~cols ~rows ~init ~s_layer ()

end

module Left_toolbar = struct

  let base_class   = "wm-left-toolbar"
  let action_class = Markup.CSS.add_element base_class "action"

  let make_action (action : action) =
    let w  = new Fab.t ~mini:true ~icon:action.icon () in
    let () = w#add_class action_class in
    let () = w#set_attribute "title" action.name in
    w

  let make widgets =
    let box = new Box.t ~widgets () in
    let ()  = box#add_class base_class in
    box

end

module Right_toolbar(I : Item) = struct

  module It = Items(I)

  class t ~s_layer_push ~layers ~selected () =
    let items,sel = It.make selected in
    let layers    = Layers.make ~init:layers ~max:I.max_layers ~s_layer_push in
    let _class    = "wm-right-toolbar" in
    object(self)
      inherit Box.t ~vertical:true ~widgets:[items#widget;layers#widget] ()
      initializer
        React.S.map (fun i -> if Option.is_some i then sel `Props) selected |> ignore;
        self#add_class _class
    end

  let make ~s_layer_push ~layers ~selected = new t ~layers ~selected ~s_layer_push ()

end

let make_settings_dialog config =
  let show_grid_switch = new Switch.t () in
  let () = show_grid_switch#set_checked config.show_grid_lines in
  let show_grid = new Form_field.t ~align_end:true ~input:show_grid_switch ~label:"Показывать сетку" () in
  let box = new Box.t ~vertical:true ~widgets:[show_grid#widget] () in
  let d   = new Dialog.t
                ~title:"Настройки редактора мозаики"
                ~actions:[ new Dialog.Action.t ~typ:`Accept () ~label:"Применить" ]
                ~content:(`Widgets [ box#widget ])
                ()
  in
  let s,push = React.S.create config in
  let () = d#add_class "wm-editor-config-dialog" in
  let _ = React.E.map (function
                       | `Accept -> push { show_grid_lines = React.S.value show_grid_switch#s_state }
                       | `Cancel -> ()) d#e_action in
  d,s

module Make(I : Item) = struct

  module IG = Items_grid(I)
  module RT = Right_toolbar(I)

  type t =
    { ig : IG.t
    ; lt : Box.t
    ; rt : RT.t
    }

  let make ~(title:       string)
           ~(init:        (string * I.item) list)
           ~(widgets:     (string * Wm.widget) list React.signal)
           ~(actions:     Fab.t list)
           ~(resolution:  (int * int))
           ~(cols:        int)
           ~(rows:        int)
           ~(s_conf:      editor_config React.signal)
           () =
    let rm = Left_toolbar.make_action { icon = "delete"; name = "Удалить" } in

    let layers = List.fold_left (fun acc (_,x) -> if List.mem ~eq:(=) (I.layer_of_item x) acc
                                                  then acc else I.layer_of_item x :: acc) [] init
                 |> List.sort compare
                 |> (fun l -> if List.is_empty l then [0] else l)
    in
    (* fix layers indexes to be from 0 to n *)
    let init   = List.foldi (fun acc i x ->
                     List.map (fun (n,item) -> if I.layer_of_item item = x
                                               then n,I.update_layer item i
                                               else n,item) acc) init layers
    in

    let s_layer,s_layer_push = React.S.create (List.hd layers) in
    let s_layers             = React.S.const layers in

    let ig = IG.make ~title ~resolution ~cols ~rows ~init ~s_layer ~s_layers () in
    let rt = RT.make ~s_layer_push ~selected:ig#s_selected ~layers:(List.length layers) in
    let lt = Left_toolbar.make (actions @ [rm]) in

    let _ = React.E.map (fun _ -> Option.iter (fun x -> x#remove) @@ React.S.value ig#s_selected) rm#e_click in
    let _ = React.S.map (fun x -> rm#set_disabled @@ Option.is_none x) ig#s_selected in
    let _ = React.S.map (fun x -> if x.show_grid_lines then ig#grid#overlay_grid#show
                                  else ig#grid#overlay_grid#hide)
                        s_conf in
    { ig; lt; rt }

end

module Cont = Make(Container_item)
module Widg = Make(Widget_item)

let rec gcd a b =
  if a != 0 && b != 0
  then let a, b = if a > b then a mod b, b else a, b mod a in gcd a b
  else a + b

let resolution_to_aspect (w,h) =
  let d = gcd w h in w / d, h / d

let get_possible_grid ~(resolution:int * int) ~(positions:Wm.position list) =
  let w,h = resolution in
  let c,r = List.fold_left (fun (c,r) (x:Wm.position) -> gcd c (x.right - x.left),
                                                         gcd r (x.bottom - x.top)) resolution positions
            |> fun (c,r) -> let d = gcd c r in  w / d, h / d
  in
  c,r

let get_preferred_grid ~resolution =
  let (w,h)   = resolution in
  let (x,y)   = resolution_to_aspect resolution in
  let desired = 30 in
  if desired >= w      then resolution
  else if x >= desired then (x,y)
  else (let get_factors i =
          let rec aux acc cnt =
            if cnt = 0 then acc
            else (if i mod cnt = 0 then aux (cnt :: acc) (pred cnt) else aux acc (pred cnt))
          in
          aux [] i
        in
        let grids = List.map (fun factor -> let c = w / factor in c, c * y / x) @@ get_factors (gcd w h) in
        let c     = List.fold_left (fun acc (x,_) -> if (x - desired) < (acc - desired) && x - desired > 0
                                                     then x else acc) w grids
        in
        c, c * y / x)

let get_grid ~resolution ~positions =
  let possible  = get_possible_grid ~resolution ~positions in
  let preffered = get_preferred_grid ~resolution in
  List.iter (fun (x:Wm.position) -> Printf.printf "left: %d, right: %d, top: %d, bottom: %d\n"
                                                  x.left x.right x.top x.bottom) positions;
  Printf.printf "resolution: %dx%d, possible: %dx%d, preffered: %dx%d\n"
                (fst resolution) (snd resolution)
                (fst possible) (snd possible)
                (fst preffered) (snd preffered);
  match Pair.compare compare compare preffered possible with
  | 1 | 0 -> preffered
  | _     -> possible

let create_widgets_grid ~(container: string * Wm.container)
                        ~widgets
                        ~s_conf
                        ~on_apply
                        ~on_cancel
                        () =
  let cont_name  = fst container in
  let cont_item  = snd container in
  let init       = cont_item.widgets in
  let pos        = Container_item.pos_of_item cont_item in
  let resolution = pos.right - pos.left, pos.bottom - pos.top in
  let back       = Left_toolbar.make_action { icon = "arrow_back"; name = "Применить и выйти" } in
  let close      = Left_toolbar.make_action { icon = "close"; name = "Отменить и выйти" } in
  let positions  = List.map (fun (_,x) -> Widget_item.pos_of_item x) init in
  let cols,rows  = get_grid ~resolution ~positions in
  let dlg        = new Dialog.t
                       ~actions:[ new Dialog.Action.t ~typ:`Accept ~label:"Отмена" ()
                                ; new Dialog.Action.t ~typ:`Decline ~label:"Ok" ()
                                ]
                       ~title:"Отменить изменения?"
                       ~content:(`Widgets [])
                       ()
  in
  let ()         = dlg#add_class "wm-confirmation-dialog" in
  let title      = Printf.sprintf "%s. Виджеты" cont_name in
  let w          = Widg.make ~title ~init ~resolution ~cols ~rows ~widgets ~s_conf ~actions:[back;close] () in
  let _          = React.E.map (fun _ -> on_apply @@ List.map (fun x -> x#get_value) w.ig#items)
                               back#e_click
  in
  let _          = React.E.map (fun _ ->
                       Lwt.Infix.(dlg#show_await >>= (fun res -> (match res with
                                                                  | `Cancel -> on_cancel ()
                                                                  | `Accept -> ());
                                                                 Lwt.return_unit)
                                  |> ignore)) close#e_click
  in
  Dom.appendChild w.ig#root dlg#root;
  w

let create ~(init:     Wm.t)
           ~(post:     Wm.t -> unit)
           ~(widgets:  (string * Wm.widget) list React.signal)
           ~(s_conf:   editor_config React.signal)
           ~(conf_dlg: Dialog.t)
           () =
  let resolution           = init.resolution in
  let s_state,s_state_push = React.S.create `Container in
  let positions            = List.map (fun (_,x) -> Container_item.pos_of_item x) init.layout in
  let cols,rows            = get_grid ~resolution ~positions in
  let title                = "Контейнеры" in

  let conf = Left_toolbar.make_action { icon = "settings"; name = "Настройки" } in
  let edit = Left_toolbar.make_action { icon = "edit";     name = "Редактировать" } in
  let save = Left_toolbar.make_action { icon = "save";     name = "Сохранить" } in

  let cont = Cont.make ~title ~init:init.layout ~resolution ~cols ~rows ~widgets ~s_conf ~actions:[edit] () in
  let _ = React.E.map (fun _ -> conf_dlg#show) conf#e_click in
  let _ = React.E.map (fun _ ->
              let selected   = React.S.value cont.ig#s_selected |> Option.get_exn in
              let container  = selected#get_value in
              let on_apply w = let (s,v)   = container in
                               let nv      = { v with widgets = w } in
                               selected#set_value (s,nv);
                               (* TODO Update min/max container w and h here *)
                               s_state_push `Container
              in
              let on_cancel  = fun () -> s_state_push `Container in
              let w = create_widgets_grid ~container ~widgets ~s_conf ~on_apply ~on_cancel () in
              s_state_push (`Widget w)) edit#e_click
  in
  let _ = React.S.map (function
                       | None   -> edit#set_disabled true
                       | Some _ -> edit#set_disabled false)
                      cont.ig#s_selected
  in
  let _ = React.E.map (fun _ -> let layout = List.map (fun x -> x#get_value) cont.ig#items in
                                post @@ { init with layout };
                                let j = Wm.to_yojson { init with layout } in
                                Printf.printf "%s\n" @@ Yojson.Safe.pretty_to_string j) save#e_click in

  let lc = new Layout_grid.Cell.t ~widgets:[] () in
  let mc = new Layout_grid.Cell.t ~widgets:[] () in
  let rc = new Layout_grid.Cell.t ~widgets:[] () in
  let w  = new Layout_grid.t ~cells:[lc; mc; rc] () in

  let add_to_view lt ig rt =
    rm_children lc#root; Dom.appendChild lc#root lt#root;
    rm_children mc#root; Dom.appendChild mc#root ig#root;
    rm_children rc#root; Dom.appendChild rc#root rt#root;
    Dom.appendChild lt#root save#root;
    Dom.appendChild lt#root conf#root;
  in
  let _ = React.S.map (function
                       | `Widget (w:Widg.t) -> add_to_view w.lt w.ig w.rt
                       | `Container         -> add_to_view cont.lt cont.ig cont.rt)
                      s_state
  in
  lc#set_span_desktop 1; lc#set_span_tablet 1; lc#set_span_phone 4;
  mc#set_span_desktop 8; mc#set_span_tablet 7; mc#set_span_phone 4;
  rc#set_span_desktop 3; rc#set_span_tablet 8; rc#set_span_phone 4;
  w

class t () =

  let elt = Dom_html.createDiv Dom_html.document in

  object(self)

    val mutable sock : WebSockets.webSocket Js.t option = None

    inherit Widget.widget elt () as super

    method private on_load =
      Requests.get_wm ()
      >>= (fun wm ->
        let config : editor_config = { show_grid_lines = true } in
        let conf_dlg,s_conf = make_settings_dialog config in
        let e_wm,wm_sock = Requests.get_wm_socket () in
        let s_wm = React.S.hold wm e_wm in
        let open Lwt.Infix in
        let post  = (fun w -> Requests.post_wm w
                              >|= (function
                                   | Ok () -> ()
                                   | Error e -> print_endline @@ "error post wm" ^ e)
                              |> Lwt.ignore_result)
        in
        let id    = "wm-editor" in
        let s_widgets = React.S.map (fun (x:Wm.t) -> x.widgets) s_wm in
        let s_layout  = React.S.map ~eq:(fun (x:Wm.t) y -> (fst x.resolution) = (fst y.resolution)
                                                           && (snd x.resolution) = (snd y.resolution)
                                                           && (Equal.poly x.layout y.layout))
                                    (fun x -> x)
                                    s_wm
        in
        Dom.appendChild self#root conf_dlg#root;
        let _     = React.S.map (fun (s:Wm.t) ->
                        (try Dom.removeChild self#root (Dom_html.getElementById id)
                         with _ -> print_endline "No el");
                        let wm_el = create ~init:s
                                           ~post
                                           ~widgets:s_widgets
                                           ~s_conf
                                           ~conf_dlg
                                           ()
                        in
                        let ()    = wm_el#set_id id in
                        Dom.appendChild self#root wm_el#root)
                                s_layout
        in
        sock <- Some wm_sock;
        Lwt_result.return ())
      |> ignore

    initializer
      self#add_class "wm";
      super#set_on_unload @@ Some (fun () -> Option.iter (fun x -> x##close; sock <- None) sock);
      super#set_on_load   @@ Some (fun () -> self#on_load);

  end

let page () = new t ()
