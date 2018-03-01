open Containers
open Components
open Requests
open Lwt_result.Infix

type 'a action =
  { icon   : string
  ; name   : string
  ; action : 'a -> unit
  }

type add_candidate =
  { icon : string
  ; name : string
  }

module type Item = sig

  type item

  type item_action = (string * item) Dynamic_grid.Item.t option React.signal action
  type grid_action = (string * item) Dynamic_grid.t action

  val create_item    : add_candidate -> item

  (* Left toolbar properties *)
  val item_actions   : (int * item_action) list
  val grid_actions   : (int * grid_action) list

  (* Right toolbar properties *)
  (* Layers properties *)
  val max_layers     : int
  (* Add properties *)
  val add_candidates : add_candidate list

  (* Grid properties *)
  val grid_title     : string

end

module Container_item : Item = struct

  type item = Wm.container
  type item_action = (string * item) Dynamic_grid.Item.t option React.signal action
  type grid_action = (string * item) Dynamic_grid.t action

  let create_item _ : item = { position = { left=0;top=0;right=0;bottom=0 }
                             ; widgets  = []
                             }

  let item_actions   = [ 2, { icon   = "delete"
                            ; name   = "Удалить"
                            ; action = (fun x -> Option.iter (fun x -> x#remove) @@ React.S.value x |> ignore)
                            }
                       ; 3, { icon   = "edit"
                            ; name   = "Редактировать"
                            ; action = (fun _ -> print_endline "edit!") }
                       ]
  let grid_actions   = [ 5, { icon   = "save"
                            ; name   = "Сохранить"
                            ; action = (fun _ -> print_endline "save!") } ]

  let max_layers     = 1

  let add_candidates = [ { icon = "crop_16_9"; name = "Контейнер" } ]

  let grid_title     = "Раскладка окон"

end

(* module Widget_item : Item = struct
 * 
 *   type item = Wm.widget
 *   type item_action = (string * item) Dynamic_grid.Item.t option action
 *   type grid_action = (string * item) Dynamic_grid.t action
 * 
 *   let item_actions = [ 3, { icon   = "delete"
 *                           ; name   = "Удалить"
 *                           ; action = (fun x -> Option.iter (fun x -> x#remove) x |> ignore)
 *                           }
 *                      ]
 *   let grid_actions = [ 5, { icon   = "save"
 *                           ; name   = "Сохранить"
 *                           ; action = (fun _ -> print_endline "save!") }
 *                      ; 2, { icon   = "arrow_back"
 *                           ; name   = "Назад"
 *                           ; action = (fun _ -> print_endline "back!") }
 *                      ]
 * 
 *   let max_layers   = 10
 * 
 *   let add_candidates = [ { icon = "tv"; name = "Видео" }
 *                        ; { icon = "audiotrack"; name = "Аудио" }
 *                        ; { icon = "font_download"; name = "Текст" }
 *                        ]
 * 
 * end *)

module Wm(I : Item) = struct

  type editor_config =
    { show_grid_lines : bool
    }

  module Grid = struct

    let base_class        = "wm-grid"

    let make_placeholder ?action ~text ~icon ~s_conf () =
      let _class          = Markup.CSS.add_element  base_class "placeholder" in
      let container_class = Markup.CSS.add_element  _class     "container"   in
      let bordered_class  = Markup.CSS.add_modifier _class     "bordered"    in

      let ph  = Dom_html.createDiv Dom_html.document |> Widget.create in
      let txt = new Typography.Text.t ~adjust_margin:false ~text () in
      let ico = new Icon.Button.Font.t ~icon () in
      let box = new Box.t ~widgets:[txt#widget;ico#widget] () in

      let ()  = box#add_class _class in
      let ()  = ph#add_class container_class in
      let ()  = box#set_align_items `Center in
      let ()  = box#set_justify_content `Center in
      let ()  = Option.iter (fun f -> f ico#e_click |> ignore) action in
      let ()  = Dom.appendChild ph#root box#root in
      let _   = React.S.map (fun x -> box#add_or_remove_class x.show_grid_lines bordered_class) s_conf in
      ph

    let make (wm: Wm.t) =
      let ((grid,x1,x2) as res) = Layout.initialize wm in
      let (grid : (string * I.item) Dynamic_grid.t) = new Dynamic_grid.t
                                                          ~grid:(Dynamic_grid.to_grid ~cols:24 ~rows:16 ~items_margin:(2,2) ())
                                                          ~items:[] () in
      let () = grid#add_class base_class in
      (grid,x1,x2)

  end

  module Left_toolbar = struct

    let base_class   = "wm-left-toolbar"
    let action_class = Markup.CSS.add_element base_class "action"

    let make_action (action : 'a action) (target : 'a)=
      let w  = new Fab.t ~mini:true ~icon:action.icon () in
      let () = w#add_class action_class in
      let () = w#set_attribute "title" action.name in
      let _  = React.E.map (fun _ -> action.action target) w#e_click in
      w

    let make widgets =
      let box = new Box.t ~widgets () in
      let ()  = box#add_class base_class in
      box

  end

  module Right_toolbar = struct

    let base_class = "wm-right-toolbar"


    class active_title ~title ~widget () =
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

    class active_title_bar titles () =
      let _class  = Markup.CSS.add_element base_class "title-bar" in
      let (titles : active_title list) =
        List.map (fun (title,widget) -> new active_title ~title ~widget ()) titles in
      object(self)

        inherit Box.t ~vertical:false ~widgets:titles ()

        method titles : active_title list = titles
        method select (w:active_title) =
          List.iter (fun x -> if not @@ Equal.physical x#root w#root then x#set_active false) titles;
          w#set_active true
        method select_by_name n =
          match List.find_opt (fun x -> String.equal x#get_title n) self#titles with
          | None   -> ()
          | Some x -> self#select x

        initializer
          self#add_class _class;
          let open Dom_events in
          let _ = List.map (fun w -> listen w#root Typ.click (fun _ _ -> self#select w; false)) self#titles in
          match self#titles with
          | []           -> failwith "Titles must not be empty"
          | [x] | x :: _ -> self#select x

      end

    let make_title_bar titles = new active_title_bar titles ()

    let make_add_item (item : add_candidate) =
      let _class = Markup.CSS.add_element base_class "add-candidate" in

      let icon = new Icon.Font.t ~icon:item.icon () in
      let text = new Typography.Text.t ~adjust_margin:false ~text:item.name () in
      let box  = new Box.t ~vertical:false ~widgets:[icon#widget;text#widget] () in
      let ()   = box#add_class _class in
      box

    let make_add (grid : (string * I.item) Dynamic_grid.t) =
      let _class = Markup.CSS.add_element base_class "add" in
      let items  = List.map (fun x -> x,make_add_item x) I.add_candidates in
      let box    = new Box.t ~widgets:(List.map snd items) () in
      let ()     = List.iter (fun (ac,w) ->
                       Dom_events.listen w#root Dom_events.Typ.click
                                         (fun _ _ -> grid#add_free ~value:("",I.create_item ac) ()
                                                     |> ignore;
                                                     false)
                       |> ignore) items
      in
      let ()     = box#add_class _class in
      box

    let make_properties (s : (string * I.item) Dynamic_grid.Item.t option React.signal) =
      let _class = Markup.CSS.add_element base_class "properties" in
      let box    = new Box.t ~widgets:[] () in
      let ()     = box#add_class _class in
      box

    let make_item_card grid selected =
      let _class  = Markup.CSS.add_element base_class "item-card" in
      let add     = make_add grid in
      let props   = make_properties selected in
      let title   = make_title_bar [ "Добавить", add
                                   ; "Свойства", props ] in
      let card    = new Card.t ~widgets:[add#widget; props#widget] () in
      let ()      = card#add_class _class in
      title,card

    let make_layer_item items =
      let _class            = Markup.CSS.add_element base_class "layer"       in
      let drag_handle_class = Markup.CSS.add_element _class     "drag-handle" in

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

    let make_layers () =
      let _class          = Markup.CSS.add_element base_class "layers"    in
      let actions_class   = Markup.CSS.add_element _class     "actions"   in
      let container_class = Markup.CSS.add_element _class     "container" in

      let (grid:Dynamic_grid.grid) = { rows             = None
                                     ; cols             = 1
                                     ; min_col_width    = 20
                                     ; max_col_width    = None
                                     ; row_height       = Some 50
                                     ; vertical_compact = true
                                     ; items_margin     = None
                                     ; multi_select     = false
                                     ; restrict_move    = true
                                     }
      in
      let grid    = new Dynamic_grid.t ~grid ~items:[make_layer_item []] () in
      let layers  = Dom_html.createDiv Dom_html.document |> Widget.create in
      let add     = new Icon.Button.Font.t ~icon:"add_box" () in
      let rm      = new Icon.Button.Font.t ~icon:"delete" () in
      let up      = new Icon.Button.Font.t ~icon:"arrow_upward" () in
      let down    = new Icon.Button.Font.t ~icon:"arrow_downward" () in
      let icons   = new Card.Actions.Icons.t ~widgets:[down#widget;up#widget;add#widget;rm#widget] () in
      let actions = new Card.Actions.t ~widgets:[icons#widget] () in
      let card    = new Card.t ~widgets:[layers#widget;actions#widget] () in
      let sel     = React.S.map (fun l -> let sel,state = match l with
                                            | [x] -> Some x, false
                                            | _   -> None, true in
                                          up#set_disabled state;
                                          down#set_disabled state;
                                          rm#set_disabled state;
                                          sel)
                                grid#s_selected
      in

      let ()      = actions#add_class actions_class in
      let ()      = Option.iter (fun x -> x#set_selected true) @@ List.head_opt grid#items in
      let ()      = layers#add_class container_class in
      let ()      = Dom.appendChild layers#root grid#root in
      let ()      = grid#set_on_load @@ Some (fun () -> grid#layout) in
      let ()      = card#add_class _class in
      (* Actions with layers *)
      let a_map (a,f) = React.E.map (fun _ -> Option.iter f @@ React.S.value sel) a#e_click in
      let _           = React.S.map (fun l -> let len = List.length l in
                                              add#set_disabled (len >= I.max_layers);
                                              up#set_disabled (len <= 1);
                                              down#set_disabled (len <= 1);
                                              rm#set_disabled (len <= 1)) grid#s_items in
      let _           = React.E.map (fun _ -> grid#add @@ make_layer_item grid#items) add#e_click in
      let l = [ rm,   (fun w -> w#remove)
              ; up,   (fun w -> move_layer_up grid#items w)
              ; down, (fun w -> move_layer_down grid#items w)
              ]
      in
      let _ = List.map a_map l in
      let title = make_title_bar ["Слои",card] in
      title,card

    let make widgets =
      let box = new Box.t ~widgets () in
      let ()  = box#add_class base_class in
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

  let create ~(wm:       Wm.t)
             ~(post:     Wm.t -> unit)
             ~(s_conf:   editor_config React.signal)
             ~(conf_dlg: Dialog.t)=
    let open Layout in
    (* grid *)
    let grid,layout,f_add = Grid.make wm in
    let grid_wp  = Dom_html.createDiv Dom_html.document |> Widget.create in
    let grid_title = new Typography.Text.t ~font:Subheading_2 ~text:I.grid_title () in
    let ()       = Dom.appendChild grid_wp#root grid#root in
    let grid_box = new Box.t ~vertical:true ~widgets:[grid_title#widget;grid_wp#widget] () in
    let ph = Grid.make_placeholder ~text:"Добавьте элементы в раскладку"
                                   ~icon:"add_box"
                                   (* ~action:f_add *)
                                   ~s_conf
                                   ()
    in
    let sel = React.S.map (function [x] -> Some x | _ -> None) grid#s_selected in
    let () = grid_wp#add_class "wm-grid-wrapper" in
    let () = grid_box#add_class "wm-grid-container" in
    let () = grid_box#set_justify_content `Center in
    let () = grid#set_on_load @@ Some (fun () -> grid#layout) in
    let _  = React.S.map (function
                          | [] -> Dom.appendChild grid#root ph#root
                          | _  -> try Dom.removeChild grid#root ph#root with _ -> ())
                         grid#s_items
    in
    let conf  = Left_toolbar.make_action { icon   = "settings"
                                         ; name   = "Настройки"
                                         ; action = (fun _ -> conf_dlg#show)
                                         } grid in
    let grid_actions = List.map (fun (i,x) -> i,Left_toolbar.make_action x grid) I.grid_actions in
    let item_actions = List.map (fun (i,x) -> i,Left_toolbar.make_action x sel)  I.item_actions in
    let actions      = [10,conf] @ grid_actions @ item_actions
                       |> List.sort (fun (i1,_) (i2,_) -> compare i1 i2)
                       |> List.map (fun (_,x) -> x)
    in
    let left_toolbar = Left_toolbar.make actions in
    let _ = List.map (fun (_,a) -> React.S.map (fun x -> a#set_disabled @@ Option.is_none x) sel)
                     item_actions
    in
    (* right toolbar *)
    let layers_title,layers = Right_toolbar.make_layers () in
    let add_title,add_card  = Right_toolbar.make_item_card grid sel in
    let right_toolbar       = Right_toolbar.make [ add_title#widget
                                                 ; add_card#widget
                                                 ; layers_title#widget
                                                 ; layers#widget
                                                 ]
    in

    (* connect to signals *)
    let _ = React.S.map (fun x -> if x.show_grid_lines
                                  then grid#overlay_grid#show
                                  else grid#overlay_grid#hide)
                        s_conf
    in

    (* main *)

    let lc = new Layout_grid.Cell.t ~widgets:[left_toolbar] () in
    let mc = new Layout_grid.Cell.t ~widgets:[grid_box] () in
    let rc = new Layout_grid.Cell.t ~widgets:[right_toolbar] () in
    let w  = new Layout_grid.t ~cells:[lc; mc; rc] () in

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

end

module Wm_containers = Wm(Container_item)

class t () =

  let elt = Dom_html.createDiv Dom_html.document in

  object(self)

    val mutable sock : WebSockets.webSocket Js.t option = None

    inherit Widget.widget elt () as super

    method private on_load =
      Requests.get_wm ()
      >>= (fun wm ->
        let config : Wm_containers.editor_config = { show_grid_lines = true } in
        let conf_dlg,s_conf = Wm_containers.make_settings_dialog config in
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
        Dom.appendChild self#root conf_dlg#root;
        let _     = React.S.map (fun s -> (try Dom.removeChild self#root (Dom_html.getElementById id)
                                           with _ -> print_endline "No el");
                                          let wm_el = Wm_containers.create ~wm:s ~post ~s_conf ~conf_dlg in
                                          let ()    = wm_el#set_id id in
                                          Dom.appendChild self#root wm_el#root)
                                s_wm
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
