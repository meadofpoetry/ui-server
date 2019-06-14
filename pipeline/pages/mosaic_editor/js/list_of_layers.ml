open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

let ( % ) f g x = f (g x)

type value  =
  { original : int
  ; actual : int
  }

type action =
  [ `Added of int
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
  let changed =
    List.fold_left (fun acc x ->
        let op, np = x#value.actual,(List.length layers - 1) - x#pos.y in
        if op = np then acc else
          (x#set_value { x#value with actual = np };
           set_data_layer_attr x np; (op,np) :: acc)) [] layers
  in
  match changed with
  | [] -> ()
  | l -> push (`Changed l)

let make_show_toggle () =
  let state, push_state = React.S.create true in
  let on = Icon.SVG.(make_simple Path.eye) in
  let off = Icon.SVG.(make_simple Path.eye_off) in
  state, Icon_button.make
    ~on:true
    ~on_icon:on
    ~icon:off
    ~on_change:push_state
    ()

let make_layer_item s_layers push layer =
  let open Dynamic_grid.Position in
  let _class = "wm-layer-item" in
  let drag_handle_class = Components_tyxml.BEM.add_element _class "drag-handle" in
  let show_icon_class = Components_tyxml.BEM.add_element _class "visibility" in
  let color_class = Components_tyxml.BEM.add_element _class "color-indicator" in

  let layers = React.S.value s_layers in
  let drag = Icon.SVG.(make_simple Path.drag_horizontal)  in
  let original = List.fold_left (fun acc x -> max (succ x#value.original) acc) 0 layers in
  let text = Typography.Text.make (Printf.sprintf "Слой %d" (original + 1)) in
  let state, vis = make_show_toggle () in
  let color = Tyxml_js.Html.(span ~a:[a_class [color_class]] [])
              |> Tyxml_js.To_dom.of_element |> Widget.create in
  let left = Box.make ~dir:`Row ~align_items:`Center
      [vis#widget; color#widget; text#widget ] in
  let box = Box.make ~dir:`Row ~justify_content:`Space_between
      [left#widget; drag#widget] in
  let y = List.length layers - layer in
  let pos = { x = 0; y; w = 1; h = 1 } in
  let value = { original; actual = layer } in
  let item =
    Dynamic_grid.Item.to_item ~pos ~move_widget:drag#widget ~widget:box#widget
      ~on_drag:(fun _ _ _ _ -> emit_new_pos s_layers push)
      ~resizable:false ~selectable:true ~value ()
  in
  let () = List.iter (fun i ->
      if i#pos.y >= y
      then i#set_pos { i#pos with y = i#pos.y + 1 }) layers in
  vis#add_class show_icon_class;
  drag#add_class drag_handle_class;
  box#add_class _class;
  item, state

let on_add grid push =
  let f layer =
    let i, s = make_layer_item grid#s_items push layer in
    (match grid#add i with
     | Ok item ->
       let _ = React.S.map (fun x -> push @@ `Visibility (item#value.actual, x)) s in
       set_data_layer_attr item item#value.actual;
       emit_new_pos grid#s_items push;
       item#set_selected true;
       push (`Added item#value.actual);
       push (`Selected item#value.actual);
     | Error _ -> ())
  in
  match React.S.value grid#s_selected with
  | [ ] -> f 0
  | [x] -> f (x#value.actual + 1)
  | _ -> ()

let remove_layer grid push layer =
  let open Dynamic_grid.Position in
  let layers =
    List.filter (not % Widget.equal layer)
    @@ React.S.value grid#s_items in
  let y = layer#pos.y in
  push (`Removed layer#value.actual);
  grid#remove layer;
  emit_new_pos grid#s_items push;
  match List.find_opt (fun w -> w#pos.y = y) layers with
  | Some w -> w#set_selected true;
  | None ->
    match List.find_opt (fun w -> w#pos.y = y - 1) layers with
    | Some w -> w#set_selected true
    | None ->
      match layers with
      | hd :: _ -> hd#set_selected true
      | _ -> ()

let move_layer_up s_layers push layer =
  let open Dynamic_grid.Position in
  let pos = layer#pos in
  if pos.y <> 0
  then
    let upper =
      List.find_opt (fun x -> x#pos.y = pos.y - 1)
      @@ React.S.value s_layers in
    match upper with
    | None -> ()
    | Some x ->
      let new_pos = x#pos in
      x#set_pos pos;
      layer#set_pos new_pos;
      emit_new_pos s_layers push

let move_layer_down s_layers push layer =
  let open Dynamic_grid.Position in
  let pos = layer#pos in
  let layers = React.S.value s_layers in
  if pos.y <> List.length layers - 1
  then
    let lower = List.find_opt (fun x -> x#pos.y = pos.y + 1) layers in
    match lower with
    | None -> ()
    | Some x ->
      let new_pos = x#pos in
      x#set_pos pos;
      layer#set_pos new_pos;
      emit_new_pos s_layers push

class grid ~init () =
  let _class = "wm-layers-grid" in
  let grid =
    Dynamic_grid.to_grid
      ~cols:1
      ~min_col_width:20
      ~row_height:50
      ~vertical_compact:true
      ~restrict_move:true
      () in
  let e_layer, push = React.E.create () in

  object(self)

    inherit [value] Dynamic_grid.t ~grid ~items:[] () as super

    method! init () : unit =
      super#init ();
      self#initialize init;
      self#add_class _class;
      (* self#set_on_load @@ Some self#layout; *) (* FIXME *)
      React.S.diff (fun n o ->
          let open Dynamic_grid.Position in
          match n with
          | [x] -> push @@ `Selected x#value.actual
          | _   ->
            begin match o with
              | [x] -> push @@ `Selected x#pos.y
              | _   -> ()
            end) self#s_selected
      |> ignore

    method e_layer : action React.event = e_layer

    method e_layer_push : ?step:React.step -> action -> unit = push

    method clear () = self#remove_all ()

    method initialize (init : int list) =
      self#clear ();
      List.iter (fun _ -> on_add self push) init;
      match self#items with
      | hd :: _ -> hd#set_selected true
      | _ -> ()

  end

let make_layers_grid ~init =
  new grid ~init ()

let make_layers_actions max layers_grid push =
  let open Dynamic_grid.Position in
  let open Icon.SVG in
  let _class = "wm-layers-actions" in
  let add = Icon_button.make
      ~icon:(make_simple Path.plus_box)
      ~on_click:(fun _ _ ->
          on_add layers_grid push;
          Lwt.return_unit)
      () in
  let rm = Icon_button.make ~icon:(make_simple Path.delete) () in
  let up = Icon_button.make ~icon:(make_simple Path.arrow_up) () in
  let down = Icon_button.make ~icon:(make_simple Path.arrow_down) () in
  let icons =
    Card.Actions.make_icons
      [ down#widget
      ; up#widget
      ; add#widget
      ; rm#widget ] in
  icons#add_class _class;
  (* Actions with layers *)
  let s_sel =
    React.S.map (function
        | [x] -> Some x
        | _ -> None) layers_grid#s_selected in
  let a_map ((a : #Widget.t), f) =
    Lwt.async (fun () ->
        Events.clicks a#root (fun _ _ ->
            (match React.S.value s_sel with
             | Some x -> f x
             | None -> ());
            Lwt.return_unit)) in
  let _ =
    React.S.l2 (fun s l ->
        let len = List.length l in
        let sel = match s with None -> false | Some _ -> true in
        let is_first = match s with
          | None -> false
          | Some x -> x#pos.y = 0 in
        let is_last = match s with
          | None -> false
          | Some x -> x#pos.y = pred len in
        add#set_disabled (len >= max);
        up#set_disabled ((len <= 1) || not sel || is_first);
        down#set_disabled ((len <= 1) || not sel || is_last);
        rm#set_disabled ((len <= 1) || not sel))
      s_sel layers_grid#s_change in
  let l =
    [ rm, (fun w -> remove_layer layers_grid push w)
    ; up, (fun w -> move_layer_up layers_grid#s_items push w)
    ; down, (fun w -> move_layer_down layers_grid#s_items push w)
    ]
  in
  List.iter a_map l;
  icons

class t ~init ~max () =
  let grid = make_layers_grid ~init in
  let wrapper_class = "wm-layers-grid-wrapper" in
  let layers = Widget.create_div () in
  let actions = Card.Actions.make
      [(make_layers_actions max grid grid#e_layer_push)#widget] in
  object
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#add_class "wm-layers-card";
      super#add_class Box.CSS.root;
      super#add_class Box.CSS.vertical;
      super#append_child layers;
      super#append_child actions;

      layers#add_class wrapper_class;
      layers#append_child grid

    method! destroy () : unit =
      grid#destroy ();
      layers#destroy ();
      actions#destroy ();
      super#destroy ()

    method! layout () : unit =
      layers#layout ();
      grid#layout ();
      actions#layout ();
      super#layout ()

    method grid = grid
  end

let make ~init ~max =
  new t ~init ~max ()
