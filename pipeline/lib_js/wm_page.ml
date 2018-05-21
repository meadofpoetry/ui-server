open Containers
open Components
open Requests
open Lwt_result.Infix
open Wm_types
open Wm_components

type container_grids =
  { rect  : Wm.position
  ; grids : (int * int) list
  }

let pos_absolute_to_relative (pos:Wm.position) (cont_pos:Wm.position) : Wm.position =
  { left   = pos.left - cont_pos.left
  ; right  = pos.right - cont_pos.left
  ; top    = pos.top - cont_pos.top
  ; bottom = pos.bottom - cont_pos.top
  }

let pos_relative_to_absolute (pos:Wm.position) (cont_pos:Wm.position) : Wm.position =
  { left   = pos.left + cont_pos.left
  ; right  = pos.right + cont_pos.left
  ; top    = pos.top + cont_pos.top
  ; bottom = pos.bottom + cont_pos.top
  }

let get_bounding_rect (positions:Wm.position list) : Wm.position =
  let open Wm in
  match positions with
  | []     -> { left=0;right=0;top=0;bottom=0}
  | hd::tl -> List.fold_left (fun acc (x:Wm.position) -> let {left;top;bottom;right} = x in
                                                         let left   = min acc.left left in
                                                         let top    = min acc.top top in
                                                         let bottom = max acc.bottom bottom in
                                                         let right  = max acc.right right in
                                                         { left;top;right;bottom})
                             hd tl

let get_bounding_rect_and_grids (positions:Wm.position list) =
  let rect       = get_bounding_rect positions in
  let resolution = rect.right - rect.left, rect.bottom - rect.top in
  let positions  = List.map (fun x -> pos_absolute_to_relative x rect) positions in
  { rect; grids = Utils.get_grids ~resolution ~positions () }

let resize ~(resolution:int*int) ~(to_position:'a -> Wm.position) ~(f:Wm.position -> 'a -> 'a) = function
  | [] -> []
  | l  ->
     let grids = get_bounding_rect_and_grids @@ List.map to_position l in
     let rect  = grids.rect |> Utils.to_grid_position in
     let nw,nh = Utils.resolution_to_aspect (rect.w,rect.h)
                 |> Dynamic_grid.Position.correct_aspect {x=0;y=0;w=fst resolution;h=snd resolution}
                 |> (fun p -> p.w,p.h)
     in
     let w,h   = if nw > rect.w
                 then List.hd grids.grids
                 else List.fold_left (fun acc (w,h) -> if w > (fst acc) && w <= nw
                                                       then (w,h) else acc) (0,0) grids.grids
     in
     let cw,rh = nw / w, nh / h in
     let dx    = (fst resolution - (w * cw)) / 2 in
     let dy    = (snd resolution - (h * rh)) / 2 in
     let apply (item:'a) : 'a =
       Utils.of_grid_position rect
       |> pos_absolute_to_relative (to_position item)
       |> Wm_items_layer.grid_pos_of_layout_pos ~resolution:(rect.w,rect.h) ~cols:w ~rows:h
       |> (fun pos -> Dynamic_grid.Position.({ x = (pos.x * cw) + dx
                                             ; y = (pos.y * rh) + dy
                                             ; w = pos.w * cw
                                             ; h = pos.h * rh }))
       |> Utils.of_grid_position
       |> (fun x -> f x item)
     in
     List.map apply l

let resize_container (p:Wm.position) (t:Wm.container wm_item) =
  let resolution = p.right - p.left, p.bottom - p.top in
  let widgets    = resize ~resolution
                          ~to_position:(fun (_,(x:Wm.widget)) -> x.position)
                          ~f:(fun pos (s,(x:Wm.widget)) -> s,{ x with position = pos })
                          t.item.widgets
  in
  { t with item = { t.item with position = p; widgets }}

let resize_layout ~(resolution:int*int) (l:Wm.container wm_item list) =
  let containers = resize ~resolution
                          ~to_position:(fun (t:Wm.container wm_item) -> t.item.position)
                          ~f:resize_container
                          l
  in
  containers

module Container_item : Item with type item = Wm.container = struct

  type item        = Wm.container  [@@deriving yojson,eq]
  type layout_item = string * item
  type t           = item wm_item  [@@deriving yojson,eq]

  let max_layers = 1

  let update_min_size (t : t) =
    let min_size = match t.item.widgets with
      | [] -> None
      | _  -> let positions = List.map (fun (_,(x:Wm.widget)) -> x.position) t.item.widgets in
              let w,h = List.hd (get_bounding_rect_and_grids positions).grids in
              Some (w,h)
    in
    { t with min_size }
  let t_to_layout_item (t:t) = t.name,t.item
  let t_of_layout_item (k,(v:item)) =
    let icon = "crop_16_9" in
    let t    = { icon; name = k; unique = false; min_size = None; item = v } in
    update_min_size t
  let to_grid_item (t:t) (pos:Dynamic_grid.Position.t) =
    let widget = Item_info.make_container_info t in
    Dynamic_grid.Item.to_item ~value:t ~widget ~pos ()
  let position_of_t (t:t) = t.item.position
  let layer_of_t _        = 0
  let size_of_t (_ : t)   = None,None
  let layers_of_t_list _  = [0]
  let update_position (t : t) (p : Wm.position) =
    let op    = t.item.position in
    let nw,nh = p.right - p.left, p.bottom - p.top in
    let ow,oh = op.right - op.left, op.bottom - op.top in
    match (ow <> nw || oh <> nh) && not (List.is_empty t.item.widgets) with
    | true  -> resize_container p t
    | false -> { t with item = { t.item with position = p }}
  let update_layer (t : t) _ = t
  let make_item_name (t : t) (other : t list) =
    let rec aux idx other =
      let name = Printf.sprintf "%s #%d" t.name idx in
      match List.partition (fun (x:t) -> String.equal x.name name) other with
      | [],_    -> name
      | _,other -> aux (succ idx) other

    in
    aux 1 other
  let make_item_properties t _ _ = Item_properties.make_container_props t

end

module Widget_item : Item with type item = Wm.widget = struct
  type item        = Wm.widget     [@@deriving yojson,eq]
  type layout_item = string * item
  type t           = item wm_item  [@@deriving yojson,eq]

  let max_layers = 10

  let update_min_size (t:t) = t
  let t_to_layout_item (t:t) = t.name,t.item
  let t_of_layout_item (k,(v:item)) =
    let icon = match v.type_ with
      | "video" -> "tv"
      | "audio" -> "audiotrack"
      | _       -> "help"
    in
    let t = { icon; name = k; unique = true; min_size = None; item = v } in
    update_min_size t
  let to_grid_item (t:t) (pos:Dynamic_grid.Position.t) =
    let widget = Item_info.make_widget_info t in
    Dynamic_grid.Item.to_item ~keep_ar:true ~widget ~value:t ~pos ()
  let layer_of_t (t:t)   = t.item.layer
  let size_of_t (t:t)    = Option.(Pair.map return return t.item.aspect)
  let layers_of_t_list l = List.fold_left (fun acc x -> if List.mem ~eq:(=) (layer_of_t x) acc
                                                        then acc else layer_of_t x :: acc) [] l
                           |> List.sort compare
                           |> (fun l -> if List.is_empty l then [0] else l)
  let position_of_t (t:t) = t.item.position
  let update_layer (t:t) (layer:int)              = { t with item = { t.item with layer } }
  let update_position (t:t) (p:Wm.position)       = { t with item = { t.item with position = p }}
  let make_item_name (t:t) _                      = t.name
  let make_item_properties (t:t React.signal) _ _ = Item_properties.make_widget_props t

end

module Cont = Wm_editor.Make(Container_item)
module Widg = Wm_editor.Make(Widget_item)

let serialize ~(cont:Cont.t) () : (string * Wm.container) list =
  List.map (fun (n,(v:Wm.container)) ->
      let widgets = List.map (fun (s,(w:Wm.widget)) ->
                        let position = pos_relative_to_absolute w.position v.position in
                        let nw  = { w with position } in
                        s,nw) v.widgets in
      n,{ v with widgets })
           cont.ig#layout_items

let get_free_widgets containers widgets =
  let used = List.fold_left (fun acc (_,(x:Wm.container)) -> x.widgets @ acc) [] containers in
  List.filter (fun (k,(v:Wm.widget)) ->
      let eq (k1,_) (k2,_) = String.equal k1 k2 in
      not @@ List.mem ~eq (k,v) used)
              widgets

let create_widgets_grid ~(container:      Wm.container wm_item)
                        ~(candidates:     Widget_item.t list React.signal)
                        ~(set_candidates: Widget_item.t list -> unit)
                        ~(on_apply:       (string * Wm.widget) list -> unit)
                        ~(on_cancel:      unit -> unit)
                        () =
  let init_cand  = React.S.value candidates in
  let cont_name  = container.name in
  let cont_pos   = Container_item.position_of_t container in
  let resolution = cont_pos.right - cont_pos.left, cont_pos.bottom - cont_pos.top in
  let init       = List.map Widget_item.t_of_layout_item container.item.widgets in
  let apply      = Wm_left_toolbar.make_action { icon = "check"; name = "Применить" } in
  let back       = Wm_left_toolbar.make_action { icon = "arrow_back"; name = "Назад" } in
  let dlg        = new Dialog.t
                       ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Отмена" ()
                                ; new Dialog.Action.t ~typ:`Accept ~label:"Ok" ()
                                ]
                       ~title:"Сохранить изменения?"
                       ~content:(`Widgets [])
                       ()
  in
  let ()         = dlg#add_class "wm-confirmation-dialog" in
  let title      = Printf.sprintf "%s. Виджеты" cont_name in
  let w = Widg.make ~title ~init ~candidates ~set_candidates ~resolution ~actions:[back;apply] () in
  let _ = React.E.map (fun _ -> on_apply w.ig#layout_items) apply#e_click in
  let _ = React.E.map (fun _ ->
              let added   = List.filter (fun x -> not @@ List.mem ~eq:Widget_item.equal x init) w.ig#items in
              let removed = List.filter (fun x -> not @@ List.mem ~eq:Widget_item.equal x w.ig#items) init in
              match added,removed with
              | [],[] -> on_cancel ()
              | _ -> let open Lwt.Infix in
                     dlg#show_await ()
                     >>= (fun res -> (match res with
                                      | `Accept -> on_apply w.ig#layout_items
                                      | `Cancel -> set_candidates init_cand; on_cancel ());
                                     Lwt.return_unit)
                     |> ignore) back#e_click
  in
  Dom.appendChild w.ig#root dlg#root;
  w

let switch ~grid ~(selected:Container_item.t Dynamic_grid.Item.t) ~s_state_push ~candidates ~set_candidates () =
  let on_apply widgets =
    let t = selected#value in
    let t = Container_item.update_min_size { t with item = { t.item with widgets }} in
    selected#set_value t;
    grid#update_item_min_size selected;
    s_state_push `Container
  in
  let on_cancel  = fun () -> s_state_push `Container in
  let w = create_widgets_grid ~container:selected#value ~candidates ~set_candidates ~on_apply ~on_cancel () in
  s_state_push (`Widget w)

let create ~(init: Wm.t)
           ~(post: Wm.t -> unit)
           () =
  (* Convert widgets positions to relative *)
  let conv p = List.map (fun (n,(v:Wm.widget)) -> n,{ v with position = pos_absolute_to_relative v.position p }) in
  let layout = List.map (fun (n,(v:Wm.container)) -> let widgets = conv v.position v.widgets in
                                                     n, { v with widgets })
                        init.layout
  in
  let wc = List.map Widget_item.t_of_layout_item @@ get_free_widgets init.layout init.widgets in
  let s_wc,s_wc_push = React.S.create wc in
  let s_cc,s_cc_push = React.S.create [({ icon     = "crop_16_9"
                                        ; name     = "Контейнер"
                                        ; unique   = false
                                        ; min_size = None
                                        ; item     = { position = {left=0;right=0;top=0;bottom=0}
                                                     ; widgets  = []
                                                     }
                                        } : Container_item.t)
                                      ]
  in
  let wz_dlg,wz_e,wz_show  = Wm_wizard.to_dialog init in
  let resolution           = init.resolution in
  let s_state,s_state_push = React.S.create `Container in
  let title                = "Контейнеры" in
  let edit   = Wm_left_toolbar.make_action { icon = "edit"; name = "Редактировать" } in
  let save   = Wm_left_toolbar.make_action { icon = "save"; name = "Сохранить" } in
  let wizard = Wm_left_toolbar.make_action { icon = "brightness_auto"; name = "Авто" } in
  let size   = Wm_left_toolbar.make_action { icon = "aspect_ratio"; name = "Разрешение" } in
  let size_dlg  = Wm_resolution_dialog.make () in
  let on_remove = fun (t:Wm.container wm_item) ->
    let ws = List.map Widget_item.t_of_layout_item t.item.widgets in
    List.iter (fun x -> Wm_editor.remove ~eq:Widget_item.equal s_wc s_wc_push x) ws
  in
  let cont = Cont.make ~title ~init:(List.map Container_item.t_of_layout_item layout)
                       ~candidates:s_cc ~set_candidates:s_cc_push
                       ~resolution ~on_remove ~actions:[save;wizard;size;edit] ()
  in
  let _ = React.E.map (fun _ -> wz_show ()) wizard#e_click in
  let _ = React.S.map (fun x -> edit#set_disabled @@ Option.is_none x) cont.ig#s_selected in
  let _ = React.E.map (fun selected -> switch ~grid:cont.ig
                                              ~s_state_push
                                              ~candidates:s_wc
                                              ~set_candidates:s_wc_push
                                              ~selected
                                              ())
          @@ React.E.select [ cont.ig#e_item_dblclick
                            ; React.E.map (fun _ -> React.S.value cont.ig#s_selected |> Option.get_exn)
                                          edit#e_click
                            ]
  in
  let _ = React.E.map (fun _ -> post { resolution = cont.ig#resolution
                                     ; widgets=init.widgets
                                     ; layout = serialize ~cont () }) save#e_click in
  let _ = React.E.map (fun l ->
              let layout = List.map (fun (n,(v:Wm.container)) ->
                               let widgets = conv v.position v.widgets in
                               n, { v with widgets }) l
              in
              let layers = Container_item.layers_of_t_list @@ List.map Container_item.t_of_layout_item layout in
              cont.rt#initialize_layers layers;
              s_wc_push @@ List.map Widget_item.t_of_layout_item @@ get_free_widgets layout init.widgets;
              cont.ig#initialize init.resolution @@ List.map Container_item.t_of_layout_item layout) wz_e
  in
  let _ = React.E.map (fun _ -> let open Lwt.Infix in
                                size_dlg#show_await_resolution cont.ig#resolution
                                >|= (function
                                     | Some r ->
                                        let new_layout = resize_layout ~resolution:r cont.ig#items in
                                        cont.ig#initialize r new_layout
                                     | None   -> ())
                                |> Lwt.ignore_result) size#e_click
  in
  let lc = new Layout_grid.Cell.t ~widgets:[] () in
  let mc = new Layout_grid.Cell.t ~widgets:[] () in
  let rc = new Layout_grid.Cell.t ~widgets:[] () in
  lc#set_span_desktop @@ Some 1; lc#set_span_tablet @@ Some 1; lc#set_span_phone @@ Some 4;
  mc#set_span_desktop @@ Some 8; mc#set_span_tablet @@ Some 7; mc#set_span_phone @@ Some 4;
  rc#set_span_desktop @@ Some 3; rc#set_span_tablet @@ Some 8; rc#set_span_phone @@ Some 4;
  let add_to_view lt ig rt =
    Utils.rm_children lc#root; Dom.appendChild lc#root lt#root;
    Utils.rm_children mc#root; Dom.appendChild mc#root ig#root;
    Utils.rm_children rc#root; Dom.appendChild rc#root rt#root;
  in
  let _ = React.S.map (function `Widget (w:Widg.t) -> add_to_view w.lt w.ig w.rt
                              | `Container         -> add_to_view cont.lt cont.ig cont.rt)
                      s_state
  in
  Dom.appendChild cont.ig#root wz_dlg#root;
  Dom.appendChild cont.ig#root size_dlg#root;
  [lc;mc;rc]

class t () = object(self)
  val mutable sock : WebSockets.webSocket Js.t option = None
  inherit Layout_grid.t ~cells:[] () as super

  method private on_load () =
    Requests.get_wm ()
    >>= (fun wm ->
      let e_wm,wm_sock = Requests.get_wm_socket () in
      let post         = (fun w -> Lwt.Infix.(Requests.post_wm w
                                              >|= (function
                                                   | Ok () -> ()
                                                   | Error _ -> print_endline @@ "error post wm")
                                              |> Lwt.ignore_result))
      in
      let _ = React.S.map (fun (s:Wm.t) -> Dom.list_of_nodeList @@ self#inner#root##.childNodes
                                           |> List.iter (fun x -> Dom.removeChild self#inner#root x);
                                           let cells = create ~init:s ~post () in
                                           List.iter (fun x -> Dom.appendChild self#inner#root x#root) cells)
                          (React.S.hold wm e_wm)
      in
      sock <- Some wm_sock;
      Lwt_result.return ())
    |> ignore
  method private on_unload () =
    Option.iter (fun x -> x##close; sock <- None) sock

  initializer
    self#set_on_load @@ Some self#on_load;
    self#set_on_unload @@ Some self#on_unload;
    self#add_class "wm";
end

let page () = new t ()