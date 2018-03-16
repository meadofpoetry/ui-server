open Containers
open Components

type action = [ `Added of int | `Removed of int | `Changed of (int * int) list | `Selected of int ]

let set_data_layer_attr w l =
  w#set_attribute "data-layer" @@ string_of_int l

let emit_new_pos (s_layers:int Dynamic_grid.Item.t list React.signal) push =
  let open Dynamic_grid.Position in
  let changed = List.fold_left (fun acc x ->
                    let op,np = x#get_value,x#pos.y in
                    if op <> np
                    then (x#set_value np; set_data_layer_attr x np; (op,np) :: acc)
                    else acc) [] @@ React.S.value s_layers in
  match changed with
  | [] -> ()
  | l  -> push (`Changed l)

let make_show_toggle () =
  let on_data  = Markup.Icon_toggle.({ icon = "visibility";     label = None; css_class = None }) in
  let off_data = Markup.Icon_toggle.({ icon = "visibility_off"; label = None; css_class = None }) in
  new Icon_toggle.t ~on_data ~off_data ()

let make_layer_item s_layers push =
  let _class            = "wm-layer-item" in
  let drag_handle_class = Markup.CSS.add_element _class "drag-handle" in
  let show_icon_class   = Markup.CSS.add_element _class "visibility" in
  let color_class       = Markup.CSS.add_element _class "color-indicator" in

  let open Dynamic_grid.Position in
  let positions = List.map (fun x -> x#pos) @@ React.S.value s_layers
                  |> List.sort (fun p1 p2 -> compare p1.y p2.y) in
  let y = match List.rev positions with
    | []    -> 0
    | hd::_ -> hd.y + 1
  in
  let drag  = new Icon.Font.t ~icon:"drag_handle" () in
  let text  = new Typography.Text.t ~text:(Printf.sprintf "Слой %d" (y + 1)) () in
  let vis   = make_show_toggle () in
  let color = Tyxml_js.Html.(span ~a:[a_class [color_class]] []) |> Tyxml_js.To_dom.of_element |> Widget.create in
  let left  = new Box.t ~vertical:false ~widgets:[vis#widget; color#widget; text#widget ] () in
  let box   = new Box.t ~vertical:false ~widgets:[left#widget; drag#widget] () in
  let pos   = { x = 0; y; w = 1; h = 1 } in
  let item  = Dynamic_grid.Item.to_item ~pos ~move_widget:drag#widget ~widget:box#widget
                                        ~on_drag:(fun _ _ _ _ -> emit_new_pos s_layers push)
                                        ~resizable:false ~selectable:true ~value:y ()
  in
  let ()    = vis#add_class show_icon_class in
  let ()    = left#set_align_items `Center in
  let ()    = drag#add_class drag_handle_class in
  let ()    = box#set_justify_content `Space_between in
  let ()    = box#add_class _class in
  item

let remove_layer s_layers push layer =
  let open Dynamic_grid.Position in
  let layers = List.filter (fun x -> not @@ Equal.physical x#root layer#root) @@ React.S.value s_layers in
  let y      = layer#pos.y in
  push (`Removed y);
  layer#remove;
  emit_new_pos s_layers push;
  match List.find_pred (fun w -> w#pos.y = y) layers with
  | Some w -> w#set_selected true
  | None   -> (match List.find_pred (fun w -> w#pos.y = y - 1) layers with
               | Some w -> w#set_selected true
               | None   -> (match layers with
                            | hd::_ -> hd#set_selected true
                            | _     -> ()))

let move_layer_up s_layers push layer =
  let open Dynamic_grid.Position in
  let pos = layer#pos in
  if pos.y <> 0
  then (let upper = List.find_opt (fun x -> x#pos.y = pos.y - 1) @@ React.S.value s_layers in
        Option.iter (fun x -> let new_pos = x#pos in
                              x#set_pos pos;
                              layer#set_pos new_pos;
                              emit_new_pos s_layers push) upper)

let move_layer_down s_layers push layer =
  let open Dynamic_grid.Position in
  let pos    = layer#pos in
  let layers = React.S.value s_layers in
  if pos.y <> List.length layers - 1
  then (let lower = List.find_opt (fun x -> x#pos.y = pos.y + 1) layers in
        Option.iter (fun x -> let new_pos = x#pos in
                              x#set_pos pos;
                              layer#set_pos new_pos;
                              emit_new_pos s_layers push) lower)

let make_layers_grid ~init =
  let e,push = React.E.create () in
  let init   = List.length init in
  let _class = "wm-layers-grid" in
  let props  = Dynamic_grid.to_grid ~cols:1 ~min_col_width:20 ~row_height:50 ~vertical_compact:true
                                    ~restrict_move:true ()
  in
  let grid   = new Dynamic_grid.t ~grid:props ~items:[] () in
  let e_sel : action React.event =
    React.S.diff (fun n o -> let open Dynamic_grid.Position in
                             match n with
                             | [x] -> `Selected x#pos.y
                             | _   -> (match o with
                                       | [x] -> `Selected x#pos.y
                                       | _   -> `Selected init)) grid#s_selected
  in
  let e      = React.E.select [e;e_sel] in
  let ()     = List.iter (fun _ -> grid#add @@ make_layer_item grid#s_items push |> ignore) @@ List.range' 0 init
  in
  let ()     = grid#set_on_load @@ Some (fun () -> grid#layout) in
  let ()     = List.iter (fun i -> set_data_layer_attr i (i#pos:Dynamic_grid.Position.t).y) grid#items in
  grid,e,push

let make_layers_actions max layers_grid push =
  let open Dynamic_grid.Position in
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
  let _           = React.E.map (fun _ -> layers_grid#add @@ make_layer_item layers_grid#s_items push
                                          |> (function
                                              | Ok i    -> set_data_layer_attr i i#pos.y;
                                                           push (`Added i#pos.y)
                                              | Error _ -> print_endline "error adding";())) add#e_click in
  let l           = [ rm,   (fun w -> remove_layer    layers_grid#s_items push w)
                    ; up,   (fun w -> move_layer_up   layers_grid#s_items push w)
                    ; down, (fun w -> move_layer_down layers_grid#s_items push w)
                    ]
  in
  let _           = List.map a_map l in
  icons

let make ~init ~max =
  let _class        = "wm-layers-card"  in
  let wrapper_class = "wm-layers-grid-wrapper" in

  let open Dynamic_grid.Position in
  let layers      = Dom_html.createDiv Dom_html.document |> Widget.create in
  let grid,e,push = make_layers_grid ~init in
  let actions     = new Card.Actions.t ~widgets:[(make_layers_actions max grid push)#widget] () in
  let card        = new Card.t ~widgets:[layers#widget;actions#widget] () in
  let ()          = Option.iter (fun x -> x#set_selected true) @@ List.head_opt grid#items in
  let ()          = layers#add_class wrapper_class in
  let ()          = Dom.appendChild layers#root grid#root in
  let ()          = card#add_class _class in
  let title       = Wm_selectable_title.make ["Слои",card] in
  let box         = new Box.t ~widgets:[title#widget; card#widget] () in
  box,e
