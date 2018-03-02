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
  }

module type Item = sig

  type item

  val create_item     : add_candidate -> item

  (* Right toolbar properties *)
  (* Layers properties *)
  val max_layers      : int
  (* Add properties *)
  val add_candidates  : add_candidate list
  (* Gen properties widget from selected item *)
  val make_item_props : (string * item) Dynamic_grid.Item.t -> Widget.widget

  (* Grid properties *)
  val grid_title      : string

end

module Container_item : Item with type item = Wm.container = struct

  type item = Wm.container

  let max_layers     = 1
  let add_candidates = [ { icon = "crop_16_9"; name = "Контейнер"; typ = "container" } ]
  let grid_title     = "Раскладка окон"

  let create_item _ : item = { position = { left=0;top=0;right=0;bottom=0 }
                             ; widgets  = []
                             }
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
  let grid_title     = "Раскладка виджетов"

  let create_item ac : item = { type_       = ac.typ
                              ; domain      = ""
                              ; position    = { left=0;top=0;right=0;bottom=0 }
                              ; layer       = 1
                              ; aspect      = (1,1)
                              ; description = ""
                              }
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

  let make_layer_item items =
    let _class            = "wm-layer-item" in
    let drag_handle_class = Markup.CSS.add_element _class "drag-handle" in

    let open Dynamic_grid.Position in
    let y = match List.rev items with
      | []    -> 0
      | hd::_ -> hd#pos.y + 1
    in
    let drag  = new Icon.Font.t ~icon:"drag_handle" () in
    let text  = new Typography.Text.t ~text:(Printf.sprintf "Слой %d" (y + 1)) () in
    let box   = new Box.t ~vertical:false ~widgets:[text#widget; drag#widget] () in
    let pos   = { x = 0; y; w = 1; h = 1 } in
    let item  = Dynamic_grid.Item.to_item ~pos ~move_widget:drag#widget ~widget:box#widget
                                          ~resizable:false ~selectable:true ~value:() ()
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

  let make_layers_grid () =
    let _class = "wm-layers-grid" in
    let props = Dynamic_grid.to_grid ~cols:1 ~min_col_width:20 ~row_height:50 ~vertical_compact:true
                                     ~restrict_move:true ()
    in
    let grid = new Dynamic_grid.t ~grid:props ~items:[make_layer_item []] () in
    let ()   = grid#set_on_load @@ Some (fun () -> grid#layout) in
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
                                             add#set_disabled (len >= max);
                                             up#set_disabled ((len <= 1) || sel);
                                             down#set_disabled ((len <= 1) || sel);
                                             rm#set_disabled ((len <= 1) || not sel))
                                 s_sel layers_grid#s_items in
    let _           = React.E.map (fun _ -> layers_grid#add @@ make_layer_item layers_grid#items) add#e_click in
    let l           = [ rm,   (fun w -> w#remove)
                      ; up,   (fun w -> move_layer_up layers_grid#items w)
                      ; down, (fun w -> move_layer_down layers_grid#items w)
                      ]
    in
    let _           = List.map a_map l in
    icons

  let make max =
    let _class        = "wm-layers-card"  in
    let wrapper_class = "wm-layers-grid-wrapper" in

    let layers  = Dom_html.createDiv Dom_html.document |> Widget.create in
    let grid    = make_layers_grid () in
    let actions = new Card.Actions.t ~widgets:[(make_layers_actions max grid)#widget] () in
    let card    = new Card.t ~widgets:[layers#widget;actions#widget] () in
    let ()      = Option.iter (fun x -> x#set_selected true) @@ List.head_opt grid#items in
    let ()      = layers#add_class wrapper_class in
    let ()      = Dom.appendChild layers#root grid#root in
    let ()      = card#add_class _class in
    let title   = Selectable_title.make ["Слои",card] in
    let box     = new Box.t ~widgets:[title#widget; card#widget] () in
    box

end

module Items(I : Item) = struct

  let base_class = "wm-items"

  module Add = struct

    let base_class = Markup.CSS.add_element base_class "add"

    let make_item (item : add_candidate) =
      let _class = Markup.CSS.add_element base_class "item" in

      let icon = new Icon.Font.t ~icon:item.icon () in
      let text = new Typography.Text.t ~adjust_margin:false ~text:item.name () in
      let box  = new Box.t ~vertical:false ~widgets:[icon#widget;text#widget] () in
      let ()   = box#add_class _class in
      box

    let make () =
      let items  = List.map (fun x -> x,make_item x) I.add_candidates in
      let box    = new Box.t ~widgets:(List.map snd items) () in
      (* let ()     = List.iter (fun (ac,w) ->
       *                  Dom_events.listen w#root Dom_events.Typ.click
       *                                    (fun _ _ -> grid#add_free ~value:("",I.create_item ac) ()
       *                                                |> ignore;
       *                                                false)
       *                  |> ignore) items
       * in *)
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
    let add     = Add.make () in
    let props   = Properties.make selected in
    let title   = Selectable_title.make [ "Добавить", add
                                        ; "Свойства", props ] in
    let card    = new Card.t ~widgets:[add#widget; props#widget] () in
    let ()      = card#add_class base_class in
    let box     = new Box.t ~vertical:true ~widgets:[title#widget; card#widget] () in
    box

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

module Items_grid(I : Item) = struct

  let base_class = "wm-grid"

  let make_grid ~(init: (string * I.item) list) (* Initial layout *)
                ~(cols: int)
                ~(rows: int)
                () =
    let _class = Markup.CSS.add_element base_class "grid" in
    let (grid : (string * I.item) Dynamic_grid.t) =
      new Dynamic_grid.t
          ~grid:(Dynamic_grid.to_grid ~cols ~rows ~items_margin:(2,2) ())
          ~items:[Dynamic_grid.Item.to_item ~pos:{x=0;y=0;w=2;h=2}
                                            ~value:("Первопроходец",(I.create_item { icon = ""
                                                                                   ; typ  = ""
                                                                                   ; name = ""
                                                   }))
                                            ()] () in
    let () = grid#add_class _class in
    grid

  class t ~cols ~rows ~(init: (string * I.item) list) () =
    let grid    = make_grid ~init ~cols ~rows () in
    let wrapper = Dom_html.createDiv Dom_html.document |> Widget.create in
    let title   = new Typography.Text.t ~font:Subheading_2 ~text:I.grid_title () in
    let ph      = make_placeholder ~text:"Добавьте элементы в раскладку" ~icon:"add_box" () in
    let ()      = Dom.appendChild wrapper#root grid#root in
    object(self)

      inherit Box.t ~vertical:true ~widgets:[title#widget;wrapper#widget] ()
      val s_sel = React.S.map (function [x] -> Some x | _ -> None) grid#s_selected

      method grid = grid
      method s_selected = s_sel

      initializer
        self#set_on_load @@ Some (fun () -> grid#layout);
        self#add_class base_class;
        self#set_justify_content `Center;
        wrapper#add_class @@ Markup.CSS.add_element base_class "wrapper";
        React.S.map (function
                     | [] -> Dom.appendChild grid#root ph#root
                     | _  -> try Dom.removeChild grid#root ph#root with _ -> ())
                    grid#s_items |> ignore

    end

  let make ~(init: (string * I.item) list) () = new t ~init ()

end

module Right_toolbar(I : Item) = struct

  module It = Items(I)

  let base_class = "wm-right-toolbar"

  let make selected =
    let items  = It.make selected in
    let layers = Layers.make I.max_layers in
    let box    = new Box.t ~widgets:[items#widget; layers#widget] () in
    let ()     = box#add_class base_class in
    box

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
    ; rt : Box.t
    }

  let make ~(init:     (string * I.item) list)
           ~(widgets:  (string * Wm.widget) list React.signal)
           ~(actions:  Fab.t list)
           ~(cols:     int)
           ~(rows:     int)
           ~(s_conf:   editor_config React.signal)
           () =
    (* grid *)
    let ig = IG.make ~cols ~rows ~init () in
    (* left toolbar *)
    let rm = Left_toolbar.make_action { icon   = "delete"
                                      ; name   = "Удалить"
                                      } in
    let _  = React.E.map (fun _ -> Option.iter (fun x -> x#remove) @@ React.S.value ig#s_selected) rm#e_click in
    let _  = React.S.map (fun x -> rm#set_disabled @@ Option.is_none x) ig#s_selected in
    let lt = Left_toolbar.make (rm :: actions) in
    (* right toolbar *)
    let rt = RT.make ig#s_selected in

    (* config *)
    let _ = React.S.map (fun x -> if x.show_grid_lines then ig#grid#overlay_grid#show
                                  else ig#grid#overlay_grid#hide)
                        s_conf in

    (* main *)
    { ig; lt; rt}

end

module Cont = Make(Container_item)
module Widg = Make(Widget_item)

let create ~(init:     Wm.t)
           ~(post:     Wm.t -> unit)
           ~(widgets:  (string * Wm.widget) list React.signal)
           ~(rows:     int)
           ~(cols:     int)
           ~(s_conf:   editor_config React.signal)
           ~(conf_dlg: Dialog.t)
           () =
  let s_state,s_state_push = React.S.create `Container in
  let conf = Left_toolbar.make_action { icon   = "settings"
                                      ; name   = "Настройки"
                                      } in
  let edit = Left_toolbar.make_action { icon   = "edit"
                                      ; name   = "Редактировать"
                                      } in
  let back = Left_toolbar.make_action { icon   = "arrow_back"
                                      ; name   = "Назад"
                                      } in
  let save = Left_toolbar.make_action { icon   = "save"
                                      ; name   = "Сохранить"
                                      } in
  let cont = Cont.make ~init:init.layout ~cols ~rows ~widgets ~s_conf ~actions:[edit;save;conf] () in
  let s = React.S.map (fun l -> List.map (fun x -> x#get_value) l) cont.ig#grid#s_items in
  let _ = React.E.map (fun _ -> conf_dlg#show) conf#e_click in
  let _ = React.E.map (fun _ ->
              let sel = React.S.value cont.ig#s_selected |> Option.get_exn in
              let w = Widg.make ~init:(snd sel#get_value).widgets
                                ~cols:24
                                ~rows:16
                                ~widgets
                                ~s_conf
                                ~actions:[back;save;conf]
                                () in
              let _ = React.E.map (fun _ -> s_state_push `Container) back#e_click in
              s_state_push (`Widget w)) edit#e_click
  in
  let _ = React.S.l2 (fun state sel ->
              match state,sel with
              | `Container, None -> edit#set_disabled true
              | _                -> edit#set_disabled false)
                     s_state cont.ig#s_selected in
  let _ = React.E.map (fun _ -> post @@ { init with layout = React.S.value s }) save#e_click in

  let lc = new Layout_grid.Cell.t ~widgets:[] () in
  let mc = new Layout_grid.Cell.t ~widgets:[] () in
  let rc = new Layout_grid.Cell.t ~widgets:[] () in
  let w  = new Layout_grid.t ~cells:[lc; mc; rc] () in

  let rm_children c = Dom.list_of_nodeList @@ c##.childNodes
                      |> List.iter (fun x -> Dom.removeChild c x)
  in
  let _ = React.S.map (function
                       | `Widget (w:Widg.t) ->
                          print_endline "switching to widget editing";
                          (try
                             rm_children lc#root;
                             rm_children mc#root;
                             rm_children rc#root;
                             Dom.appendChild lc#root w.lt#root;
                             Dom.appendChild mc#root w.ig#root;
                             Dom.appendChild rc#root w.rt#root;
                           with _ -> ())
                       | `Container ->
                          print_endline "switching to container editing";
                          (try
                             rm_children lc#root;
                             rm_children mc#root;
                             rm_children rc#root;
                             Dom.appendChild cont.lt#root save#root;
                             Dom.appendChild cont.lt#root conf#root;
                             Dom.appendChild lc#root cont.lt#root;
                             Dom.appendChild mc#root cont.ig#root;
                             Dom.appendChild rc#root cont.rt#root;
                           with _ -> ())) s_state
  in

  let () = lc#set_span_desktop 1 in
  let () = mc#set_span_desktop 8 in
  let () = rc#set_span_desktop 3 in
  let () = lc#set_span_tablet 1 in
  let () = mc#set_span_tablet 7 in
  let () = rc#set_span_tablet 8 in
  let () = lc#set_span_phone 4 in
  let () = mc#set_span_phone 4 in
  let () = rc#set_span_phone 4 in
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
                                           ~rows:16
                                           ~cols:24
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
