open Containers
open Components
open Requests
open Lwt_result.Infix

module Wm = struct

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
      let ((grid,_,_) as res) = Layout.initialize wm in
      let () = grid#add_class base_class in
      res

  end

  module Left_toolbar = struct

    let base_class = "wm-left-toolbar"

    let make widgets =
      let box = new Box.t ~widgets () in
      let ()  = List.iter (fun x -> x#add_class @@ Markup.CSS.add_element base_class "action") widgets in
      let ()  = box#add_class base_class in
      box

  end

  module Right_toolbar = struct

    let base_class = "wm-right-toolbar"

    let make_title titles =
      let _class       = Markup.CSS.add_element  base_class  "title-bar" in
      let title_class  = Markup.CSS.add_element  base_class  "title"     in
      let active_class = Markup.CSS.add_modifier title_class "active"    in

      let ws = List.map (fun x -> let w = new Typography.Text.t ~adjust_margin:false ~text:x ~font:Subheading_2 () in
                                  let () = w#add_class title_class in
                                  w)
                        titles
      in
      let box    = new Box.t ~vertical:false ~widgets:ws () in
      let s,push = React.S.create (match ws with [] -> None | hd::_ -> Some hd) in

      let () = box#add_class _class in
      let () = List.iter (fun w -> Dom_events.listen w#root Dom_events.Typ.click
                                                     (fun _ _ -> push @@ Some w; false) |> ignore) ws
      in
      let _  = React.S.map (fun active -> List.iter (fun x -> x#remove_class active_class) ws;
                                          Option.iter (fun x -> x#add_class active_class) active) s
      in
      box,s

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
      let _       = React.E.map (fun _ -> grid#add @@ make_layer_item grid#items) add#e_click in
      let _       = React.E.map (fun _ -> Option.iter (fun w -> w#remove) @@ React.S.value sel) rm#e_click in
      card

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

  let make_add _ =
    let card = new Card.t ~widgets:[] () in
    let ()   = card#style##.minHeight := Js.string "100px" in
    let ()   = card#add_class "wm-add-card" in
    card

  let create ~(wm:       Wm.t)
             ~(post:     Wm.t -> unit)
             ~(s_conf:   editor_config React.signal)
             ~(conf_dlg: Dialog.t)=
    let open Layout in
    (* grid *)
    let grid,layout,f_add = Grid.make wm in
    let grid_wp  = Dom_html.createDiv Dom_html.document |> Widget.create in
    let grid_title = new Typography.Text.t ~font:Subheading_2 ~text:"Раскладка окон" () in
    let ()       = Dom.appendChild grid_wp#root grid#root in
    let grid_box = new Box.t ~vertical:true ~widgets:[grid_title#widget;grid_wp#widget] () in
    let ph = Grid.make_placeholder ~text:"Добавьте элементы в раскладку"
                                   ~icon:"add_box"
                                   ~action:f_add
                                   ~s_conf
                                   ()
    in
    let () = grid_wp#add_class "wm-grid-wrapper" in
    let () = grid_box#add_class "wm-grid-container" in
    let () = grid_box#set_justify_content `Center in
    let () = grid#set_on_load @@ Some (fun () -> grid#layout) in
    let _  = React.S.map (function
                          | [] -> Dom.appendChild grid#root ph#root
                          | _  -> try Dom.removeChild grid#root ph#root with _ -> ())
                         grid#s_items
    in
    (* left toolbar *)
    let add   = new Fab.t ~mini:true ~icon:"add" () in
    let rm    = new Fab.t ~mini:true ~icon:"delete" () in
    let edit  = new Fab.t ~mini:true ~icon:"edit" () in
    let conf  = new Fab.t ~mini:true ~icon:"settings" () in
    let apply = new Button.t ~label:"применить" () in
    let sel   = React.S.map (function
                             | [x] -> rm#set_disabled false;
                                      edit#set_disabled false;
                                      Some x
                             | _   -> rm#set_disabled true;
                                      edit#set_disabled true;
                                      None)
                            grid#s_selected
    in
    let ()    = add#set_attribute  "title" "Добавить" in
    let ()    = rm#set_attribute   "title" "Удалить" in
    let ()    = edit#set_attribute "title" "Редактировать" in
    let _     = React.E.map (fun _ -> Option.iter (fun w -> w#remove) @@ React.S.value sel) rm#e_click in
    let _     = React.E.map (fun _ -> post { wm with layout = React.S.value layout }) apply#e_click in
    let _     = React.E.map (fun _ -> conf_dlg#show) conf#e_click in
    let _     = f_add add#e_click in
    let _     = React.E.map (fun _ -> Option.iter (fun x -> x#remove) @@ React.S.value sel) rm#e_click in
    let left_toolbar   = Left_toolbar.make [add#widget; rm#widget; edit#widget; conf#widget] in
    (* right toolbar *)
    let layers_title,_ = Right_toolbar.make_title ["Слои"] in
    let layers         = Right_toolbar.make_layers () in
    let add_title,_    = Right_toolbar.make_title ["Добавить";"Свойства"] in
    let add_card       = make_add grid in
    let right_toolbar  = Right_toolbar.make [ add_title#widget
                                            ; add_card#widget
                                            ; layers_title#widget
                                            ; layers#widget
                                            ]
    in

    (* connect to conf signal *)
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

class t () =

  let elt = Dom_html.createDiv Dom_html.document in

  object(self)

    val mutable sock : WebSockets.webSocket Js.t option = None

    inherit Widget.widget elt () as super

    method private on_load =
      Requests.get_wm ()
      >>= (fun wm ->
        let config : Wm.editor_config = { show_grid_lines = true } in
        let conf_dlg,s_conf = Wm.make_settings_dialog config in
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
                                          let wm_el = Wm.create ~wm:s ~post ~s_conf ~conf_dlg in
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
