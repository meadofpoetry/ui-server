open Containers
open Components

type value  =
  { original : int
  ; actual   : int
  }

type action = [ `Added of int
              | `Removed of int
              | `Changed of (int * int) list
              | `Visibility of (int * bool)
              | `Selected of int
              ]

let set_data_layer_attr w l =
  w#set_attribute "data-layer" @@ string_of_int l

let emit_new_pos (s_layers:value Dynamic_grid.Item.t list React.signal) push =
  let open Dynamic_grid.Position in
  let layers  = React.S.value s_layers in
  let changed = List.fold_left (fun acc x ->
                    let op,np = x#get_value.actual,(List.length layers - 1) - x#pos.y in
                    if op <> np
                    then (x#set_value { x#get_value with actual = np }; set_data_layer_attr x np; (op,np) :: acc)
                    else acc) [] layers in
  match changed with
  | [] -> ()
  | l  -> push (`Changed l)

let make_show_toggle () =
  let on_data  = Markup.Icon_toggle.({ icon = "visibility";     label = None; css_class = None }) in
  let off_data = Markup.Icon_toggle.({ icon = "visibility_off"; label = None; css_class = None }) in
  let toggle   = new Icon_toggle.t ~propagate:false ~on_data ~off_data () in
  let ()       = toggle#set_on true in
  toggle

let make_layer_item s_layers push layer =
  let open Dynamic_grid.Position in
  let _class            = "wm-layer-item" in
  let drag_handle_class = Markup.CSS.add_element _class "drag-handle" in
  let show_icon_class   = Markup.CSS.add_element _class "visibility" in
  let color_class       = Markup.CSS.add_element _class "color-indicator" in
  let layers   = React.S.value s_layers in
  let drag     = new Icon.Font.t ~icon:"drag_handle" () in
  let original = List.fold_left (fun acc x -> max (succ x#get_value.original) acc) 0 layers in
  let text     = new Typography.Text.t ~text:(Printf.sprintf "Слой %d" (original + 1)) () in
  let vis      = make_show_toggle () in
  let color    = Tyxml_js.Html.(span ~a:[a_class [color_class]] [])
                 |> Tyxml_js.To_dom.of_element |> Widget.create in
  let left     = new Box.t ~vertical:false ~widgets:[vis#widget; color#widget; text#widget ] () in
  let box      = new Box.t ~vertical:false ~widgets:[left#widget; drag#widget] () in
  let y        = List.length layers - layer in
  let pos      = { x = 0; y; w = 1; h = 1 } in
  let value    = { original; actual = layer } in
  let item     = Dynamic_grid.Item.to_item ~pos ~move_widget:drag#widget ~widget:box#widget
                                           ~on_drag:(fun _ _ _ _ -> emit_new_pos s_layers push)
                                           ~resizable:false ~selectable:true ~value ()
  in
  let ()       = List.iter (fun i -> if i#pos.y >= y then i#set_pos { i#pos with y = i#pos.y + 1 }) layers in
  let ()       = vis#add_class show_icon_class in
  let ()       = left#set_align_items `Center in
  let ()       = drag#add_class drag_handle_class in
  let ()       = box#set_justify_content `Space_between in
  let ()       = box#add_class _class in
  item,vis#s_state

let on_add grid push =
  let open Dynamic_grid.Position in
  let f layer =
    let i,s = make_layer_item grid#s_items push layer in
    (match grid#add i with
     | Ok item -> let _ = React.S.map  (fun x -> push @@ `Visibility (item#get_value.actual,x)) s in
                  set_data_layer_attr item item#get_value.actual;
                  emit_new_pos grid#s_items push;
                  item#set_selected true;
                  push (`Added item#get_value.actual);
                  push (`Selected item#get_value.actual);
     | Error _ -> ())
  in
  let selected = React.S.value grid#s_selected in
  match selected with
  | []    -> f 0
  | [sel] -> f (sel#get_value.actual + 1)
  | _     -> ()


let remove_layer s_layers push layer =
  let open Dynamic_grid.Position in
  let layers = List.filter (fun x -> not @@ Equal.physical x#root layer#root) @@ React.S.value s_layers in
  let y      = layer#pos.y in
  push (`Removed layer#get_value.actual);
  layer#remove;
  emit_new_pos s_layers push;
  match List.find_pred (fun w -> w#pos.y = y) layers with
  | Some w -> w#set_selected true;
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

class t ~init () =
  let _class = "wm-layers-grid" in
  let grid   = Dynamic_grid.to_grid ~cols:1 ~min_col_width:20 ~row_height:50 ~vertical_compact:true
                                    ~restrict_move:true ()
  in
  let e_layer,push = React.E.create () in

  object(self)

    inherit [value] Dynamic_grid.t ~grid ~items:[] ()

    method e_layer      : action React.event = e_layer
    method e_layer_push : ?step:React.step -> action -> unit = push
    method clear () = self#remove_all ()
    method initialize (init:int list) =
      let init = List.length init in
      self#clear ();
      List.iter (fun _ -> on_add self push) @@ List.range' init 0;
      Option.iter (fun x -> x#set_selected true) @@ List.head_opt self#items

    initializer
      self#initialize init;
      self#add_class _class;
      self#set_on_load @@ Some (fun _ -> self#layout);
      React.S.diff (fun n o -> let open Dynamic_grid.Position in
                               match n with
                               | [x] -> push @@ `Selected x#get_value.actual
                               | _   -> (match o with
                                         | [x] -> push @@ `Selected x#pos.y
                                         | _   -> ())) self#s_selected
      |> ignore

  end

let make_layers_grid ~init =
  new t ~init ()

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
                                           let is_first = Option.map (fun x -> x#pos.y = 0) s
                                                          |> Option.get_or ~default:false in
                                           let is_last  = Option.map (fun x -> x#pos.y = pred len) s
                                                          |> Option.get_or ~default:false in
                                           add#set_disabled  (len >= max);
                                           up#set_disabled   ((len <= 1) || not sel || is_first);
                                           down#set_disabled ((len <= 1) || not sel || is_last);
                                           rm#set_disabled   ((len <= 1) || not sel))
                               s_sel layers_grid#s_change in
  let _           = React.E.map (fun _ -> on_add layers_grid push) add#e_click in
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
  let grid        = make_layers_grid ~init in
  let actions     = new Card.Actions.t ~widgets:[(make_layers_actions max grid grid#e_layer_push)#widget] () in
  let card        = new Card.t ~widgets:[layers#widget;actions#widget] () in
  let ()          = layers#add_class wrapper_class in
  let ()          = Dom.appendChild layers#root grid#root in
  let ()          = card#add_class _class in
  let title       = Wm_selectable_title.make ["Слои",card] in
  let box         = new Box.t ~widgets:[title#widget; card#widget] () in
  box,grid
