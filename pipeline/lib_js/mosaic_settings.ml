open Containers
open Components
open Requests
open Lwt_result.Infix
open Wm_types
open Wm_components

let rec gcd a b =
  if a != 0 && b != 0
  then let a, b = if a > b then a mod b, b else a, b mod a in gcd a b
  else a + b

let resolution_to_aspect (w,h) =
  let d = gcd w h in w / d, h / d

let get_possible_grid ~(resolution:int * int) ~(positions:Wm.position list) () =
  let w,h = resolution in
  let c,r = List.fold_left (fun (c,r) (x:Wm.position) -> gcd c (x.right - x.left),
                                                         gcd r (x.bottom - x.top)) resolution positions
            |> fun (c,r) -> let d = gcd c r in  w / d, h / d
  in
  c,r

let get_factors i =
  let rec aux acc cnt =
    if cnt = 0 then acc
    else (if i mod cnt = 0 then aux (cnt :: acc) (pred cnt) else aux acc (pred cnt))
  in
  aux [] i

let get_preferred_grid ?(cols=30) ~resolution () =
  let (w,h)   = resolution in
  let (x,y)   = resolution_to_aspect resolution in
  if cols >= w      then resolution,[]
  else if x >= cols then (x,y),[]
  else (let grids = List.map (fun factor -> let c = w / factor in c, c * y / x) @@ get_factors (gcd w h) in
        let best  = List.fold_left (fun acc (c,r) -> if (c - cols) < (fst acc - cols) && c - cols > 0
                                                     then (c,r) else acc) resolution grids
        in
        best,(List.filter (fun (c,r) -> c <> (fst best) || r <> (snd best)) grids))

let get_grid ~resolution ~positions () =
  let possible   = get_possible_grid ~resolution ~positions () in
  let best,other = get_preferred_grid ~resolution () in
  List.iter (fun (x:Wm.position) -> Printf.printf "left: %d, right: %d, top: %d, bottom: %d\n"
                                                  x.left x.right x.top x.bottom) positions;
  Printf.printf "resolution: %dx%d, possible: %dx%d, preffered: %dx%d\n"
                (fst resolution) (snd resolution)
                (fst possible) (snd possible)
                (fst best) (snd best);
  match Pair.compare compare compare best possible with
  | 1 | 0 -> best
  | _     -> possible

let get_widgets_bounding_rect (container:Wm.container) =
  let open Wm in
  match container.widgets with
  | [] -> { left=0;right=0;top=0;bottom=0}
  | hd::tl -> List.fold_left (fun acc (_,(x:Wm.widget)) -> let {left;top;bottom;right} = x.position in
                                                           let left   = min acc.left left in
                                                           let top    = min acc.top top in
                                                           let bottom = max acc.bottom bottom in
                                                           let right  = max acc.right right in
                                                           { left;top;right;bottom})
                             (snd hd).position tl

module Container_item : Item with type item = Wm.container = struct

  type item = Wm.container
  type t    = string * item

  let max_layers     = 1
  let add_candidates = [ { icon = "crop_16_9"; name = "Контейнер"; typ = "container" } ]

  let create_item _ : item = { position = { left=0;top=0;right=0;bottom=0 }
                             ; widgets  = []
                             }
  let pos_of_item (item : item) = item.position
  let layer_of_item _           = 0
  let layers_of_t_list _        = [0]
  let update_pos  (item : item) (p : Wm.position) =
    let op    = item.position in
    let nw,nh = p.right - p.left, p.bottom - p.top in
    let ow,oh = op.right - op.left, op.bottom - op.top in
    if ow <> nw || oh <> nh
    then
      let open Wm in
      (* bounding widgets rect *)
      match item.widgets with
      | [] -> { item with position = p }
      | _  ->
         let positions = List.map (fun (_,(x:Wm.widget)) -> x.position) item.widgets in
         let rect = get_widgets_bounding_rect item in
         let rect_w,rect_h = rect.right - rect.left, rect.bottom - rect.top in
         let rs = (float_of_int nw) /. (float_of_int nh) in
         let ri = (float_of_int rect_w) /. (float_of_int rect_h) in
         let ar = resolution_to_aspect (rect_w, rect_h) in
         let og = get_grid ~resolution:(ow,oh) ~positions () in
         let cw,ch = ow / (fst og), oh / (snd og) in
         Printf.printf "cw: %d, ch: %d\n" cw ch;
         (* let w,h =
          *   if Float.(rs > ri)
          *   then
          *     let () = print_endline "rs > ri" in
          *     let w = int_of_float @@ (float_of_int rect_w) *. ((float_of_int nh) /. (float_of_int rect_h)) in
          *     let w = (w / (fst ar)) * (fst ar)  in
          *     let h = (snd ar) * w / (fst ar) in
          *     w,h
          *   else
          *     let () = print_endline "ri >= rs" in
          *     let h = int_of_float @@ (float_of_int rect_h) *. ((float_of_int nw) /. (float_of_int rect_w)) in
          *     let h = (h / (snd ar)) * (snd ar) in
          *     let w = (fst ar) * h / (snd ar) in
          *     w,h
          * in
          * let w,h = List.fold_left (fun (c,r) (x:Wm.position) -> gcd c (x.right - x.left),
          *                                                        gcd r (x.bottom - x.top)) (w,h) positions
          * in
          * let sf = (float_of_int w) /. (float_of_int rect_w) in
          * Printf.printf "res: %dx%d, ar: %dx%d, old wh: %dx%d, new wh: %dx%d\n"
          *               nw nh (fst ar) (snd ar) rect_w rect_h w h; *)
         let widgets = item.widgets in
         (* let widgets = List.map (fun (n,(v:Wm.widget)) ->
          *                   let open Wm in
          *                   let pos = v.position in
          *                   let pos = { left   = int_of_float @@ (float_of_int pos.left) *. sf
          *                             ; right  = int_of_float @@ (float_of_int pos.right) *. sf
          *                             ; top    = int_of_float @@ (float_of_int pos.top) *. sf
          *                             ; bottom = int_of_float @@ (float_of_int pos.bottom) *. sf
          *                             }
          *                   in n,{ v with position = pos }) item.widgets
          * in *)
         { item with position = p; widgets }
    else { item with position = p }
  let update_layer (item : item) _ = item
  let make_item_name (ac : add_candidate) (index : int) =
    Printf.sprintf "%s #%d" ac.name index
  let make_item_props = Item_properties.make_container_props

end

module Widget_item : Item with type item = Wm.widget = struct

  type item = Wm.widget
  type t    = string * item
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
  let layers_of_t_list l =
    List.fold_left (fun acc (_,x) -> if List.mem ~eq:(=) (layer_of_item x) acc
                                     then acc else layer_of_item x :: acc) [] l
    |> List.sort compare
    |> (fun l -> if List.is_empty l then [0] else l)
  let pos_of_item (item : item) = item.position
  let update_layer (item : item) (layer : int) = { item with layer }
  let update_pos  (item : item) (pos : Wm.position) = { item with position = pos }
  let make_item_name (ac : add_candidate) (index : int) =
    Printf.sprintf "%s #%d" ac.name index
  let make_item_props _ =
    let box         = new Box.t ~vertical:true ~widgets:[] () in
    { widget  = box#widget
    ; actions = []
    }

end

module Cont = Wm_editor.Make(Container_item)
module Widg = Wm_editor.Make(Widget_item)

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

let create_widgets_grid ~(container: string * Wm.container)
                        ~widgets
                        ~s_conf
                        ~on_apply
                        ~on_cancel
                        () =
  let cont_name  = fst container in
  let cont_item  = snd container in
  let cont_pos   = Container_item.pos_of_item cont_item in
  let resolution = cont_pos.right - cont_pos.left, cont_pos.bottom - cont_pos.top in
  let init       = cont_item.widgets in
  let back       = Wm_left_toolbar.make_action { icon = "arrow_back"; name = "Применить и выйти" } in
  let close      = Wm_left_toolbar.make_action { icon = "close"; name = "Отменить и выйти" } in
  let cols,rows  = get_grid ~resolution ~positions:(List.map (fun (_,x) -> Widget_item.pos_of_item x) init) () in
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
  let w = Widg.make ~title ~init ~resolution ~cols ~rows ~widgets ~s_conf ~actions:[back;close] () in
  let _ = React.E.map (fun _ -> let widgets = List.map (fun x -> x#get_value) w.ig#items in
                                on_apply widgets)
                      back#e_click
  in
  let _ = React.E.map (fun _ -> Lwt.Infix.(dlg#show_await >>= (fun res -> (match res with
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
  (* Convert widgets positions to relative *)
  let conv p = List.map (fun (n,v) -> let pos = Widget_item.pos_of_item v in
                                      n,{ v with position = pos_absolute_to_relative pos p })
  in
  let layout = List.map (fun (n,v) -> let open Wm in
                                      let widgets = conv v.position v.widgets in
                                      n, { v with widgets })
                        init.layout
  in
  let init                 = { init with layout } in
  let resolution           = init.resolution in
  let s_state,s_state_push = React.S.create `Container in
  let positions            = List.map (fun (_,x) -> Container_item.pos_of_item x) init.layout in
  let cols,rows            = get_grid ~resolution ~positions () in
  let title                = "Контейнеры" in

  let conf = Wm_left_toolbar.make_action { icon = "settings"; name = "Настройки" } in
  let edit = Wm_left_toolbar.make_action { icon = "edit";     name = "Редактировать" } in
  let save = Wm_left_toolbar.make_action { icon = "save";     name = "Сохранить" } in

  let cont = Cont.make ~title ~init:init.layout ~resolution ~cols ~rows ~widgets ~s_conf ~actions:[edit] () in
  let _ = React.E.map (fun _ -> conf_dlg#show) conf#e_click in
  let _ = React.E.map (fun _ ->
              let selected   = React.S.value cont.ig#s_selected |> Option.get_exn in
              let container  = selected#get_value in
              let on_apply w = let (s,v)   = container in
                               let nv  = { v with widgets = w } in
                               (match w with
                                | [] -> selected#set_min_h None;
                                        selected#set_min_w None;
                                | _  -> let r   = get_widgets_bounding_rect nv in
                                        let cw  = (fst resolution) / cols in
                                        let rh  = (snd resolution) / rows in
                                        let mw  = ceil ((float_of_int (r.right - r.left)) /. (float_of_int cw))
                                                  |> int_of_float in
                                        let mh  = ceil ((float_of_int (r.bottom - r.top)) /. (float_of_int rh))
                                                  |> int_of_float in
                                        selected#set_min_h (Some mh);
                                        selected#set_min_w (Some mw));
                               (* TODO Update min/max container w and h here *)
                               selected#set_value (s,nv);
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
  let _ = React.E.map (fun _ ->
              (* Convert widgets positions to absolute *)
              let conv p = List.map (fun (n,v) -> let pos = Widget_item.pos_of_item v in
                                                  n,{ v with position = pos_relative_to_absolute pos p })
              in
              let layout = List.map (fun x -> let open Wm in
                                              let n,v = x#get_value in
                                              let widgets = conv v.position v.widgets in
                                              n,{ v with widgets })
                                    cont.ig#items in
              post @@ { init with layout };
              let j = Wm.to_yojson { init with layout } in
              Printf.printf "%s\n" @@ Yojson.Safe.pretty_to_string j) save#e_click in

  let lc = new Layout_grid.Cell.t ~widgets:[] () in
  let mc = new Layout_grid.Cell.t ~widgets:[] () in
  let rc = new Layout_grid.Cell.t ~widgets:[] () in
  let w  = new Layout_grid.t ~cells:[lc; mc; rc] () in

  let add_to_view lt ig rt =
    Utils.rm_children lc#root; Dom.appendChild lc#root lt#root;
    Utils.rm_children mc#root; Dom.appendChild mc#root ig#root;
    Utils.rm_children rc#root; Dom.appendChild rc#root rt#root;
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
        let conf_dlg,s_conf = Settings_dialog.make config in
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
