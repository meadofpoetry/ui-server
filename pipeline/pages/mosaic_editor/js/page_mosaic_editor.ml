open Js_of_ocaml
open Netlib
open Components
open Wm_types
open Basic_widgets
open Container
open Pipeline_types
open Pipeline_http_js

let ( >>= ) = Lwt.( >>= )

module Selector = struct
  let video = "." ^ Page_mosaic_editor_tyxml.CSS.video
end

type container_grids =
  { rect : Wm.position
  ; grids : (int * int) list
  }
(*
let pos_absolute_to_relative
      (pos : Wm.position)
      (cont_pos : Wm.position) : Wm.position =
  { left = pos.left - cont_pos.left
  ; right = pos.right - cont_pos.left
  ; top = pos.top - cont_pos.top
  ; bottom = pos.bottom - cont_pos.top
  }

let pos_relative_to_absolute
      (pos : Wm.position)
      (cont_pos : Wm.position) : Wm.position =
  { left = pos.left + cont_pos.left
  ; right = pos.right + cont_pos.left
  ; top = pos.top + cont_pos.top
  ; bottom = pos.bottom + cont_pos.top
  }
 *)
let get_bounding_rect (positions : Wm.position list) : Wm.position =
  let open Wm in
  match positions with
  | [] -> { left = 0; right = 0; top = 0; bottom = 0 }
  | hd :: tl ->
    List.fold_left (fun acc (x : Wm.position) ->
        let { left; top; bottom; right } = x in
        let left = min acc.left left in
        let top = min acc.top top in
        let bottom = max acc.bottom bottom in
        let right = max acc.right right in
        { left; top; right; bottom}) hd tl

let get_bounding_rect_and_grids (positions : Wm.position list) =
  let rect = get_bounding_rect positions in
  let resolution = rect.right - rect.left, rect.bottom - rect.top in
  let positions  =
    List.map (fun x -> pos_absolute_to_relative x rect) positions in
  { rect
  ; grids = Utils.get_grids ~resolution ~positions ()
  }

let resize ~(resolution : int * int)
    ~(to_position : 'a -> Wm.position)
    ~(f : Wm.position -> 'a -> 'a) = function
  | [] -> []
  | l ->
    let grids = get_bounding_rect_and_grids @@ List.map to_position l in
    let rect = grids.rect |> Utils.to_grid_position in
    let nw, nh =
      Utils.resolution_to_aspect (rect.w, rect.h)
      |> Dynamic_grid.Position.correct_aspect
        { x = 0
        ; y = 0
        ; w = fst resolution
        ; h = snd resolution }
      |> (fun p -> p.w, p.h) in
    let w, h =
      if nw > rect.w
      then List.hd grids.grids
      else List.fold_left (fun acc (w, h) ->
          if w > (fst acc) && w <= nw
          then (w,h) else acc) (0,0) grids.grids in
    let cw, rh = nw / w, nh / h in
    let dx = (fst resolution - (w * cw)) / 2 in
    let dy = (snd resolution - (h * rh)) / 2 in
    let apply (item : 'a) : 'a =
      Utils.of_grid_position rect
      |> pos_absolute_to_relative (to_position item)
      |> Layer.grid_pos_of_layout_pos
        ~resolution:(rect.w, rect.h) ~cols:w ~rows:h
      |> (fun pos -> Dynamic_grid.Position.(
          { x = (pos.x * cw) + dx
          ; y = (pos.y * rh) + dy
          ; w = pos.w * cw
          ; h = pos.h * rh }))
      |> Utils.of_grid_position
      |> (fun x -> f x item)
    in
    List.map apply l

let resize_container (p : Wm.position) (t : Wm.container wm_item) =
  let resolution = p.right - p.left, p.bottom - p.top in
  let widgets =
    resize ~resolution
      ~to_position:(fun (_, (x : Wm.widget)) -> Utils.Option.get x.position)
      ~f:(fun pos (s, (x : Wm.widget)) -> s, { x with position = Some pos })
      (List.filter (fun (_, (w : Wm.widget)) -> Option.is_some w.position) t.item.widgets)
      (* TODO cleanup the the mess induced by relative widget position *)
  in
  { t with item = Wm.{ position = p; widgets }}

let resize_layout ~(resolution : int * int) (l : Wm.container wm_item list) =
  let containers =
    resize ~resolution
      ~to_position:(fun (t : Wm.container wm_item) -> t.item.position)
      ~f:resize_container
      l
  in
  containers

module Widget_item : Item with type item = Wm.widget = struct

  type item = Wm.widget [@@deriving yojson, eq]

  type layout_item = string * item

  type t = item wm_item  [@@deriving yojson, eq]

  let max_layers = 10

  let update_min_size (t : t) = t

  let t_to_layout_item (t : t) = t.name, t.item

  let t_of_layout_item (k, (v : item)) =
    let path =
      let open Icon.SVG.Path in
      match v.type_ with
      | Video -> video
      | Audio -> music in
    let t =
      { icon = Icon.SVG.(make_simple path)#widget
      ; name = k
      ; unique = true
      ; min_size = None
      ; item = v } in
    update_min_size t

  let to_grid_item (t : t) (pos : Dynamic_grid.Position.t) =
    let widget = Item_info.make_widget_info t in
    let keep_ar   =
      match t.item.aspect with
      | Some _ -> true
      | None   -> false in
    Dynamic_grid.Item.to_item ~keep_ar ~widget ~value:t ~pos ()

  let layer_of_t (t : t) = t.item.layer

  let size_of_t (t : t) = match t.item.aspect with
    | Some (x, y) -> Some x, Some y
    | None -> None, None

  let layers_of_t_list l =
    List.fold_left (fun acc x ->
        if List.mem (layer_of_t x) acc
        then acc else layer_of_t x :: acc) [] l
    |> List.sort compare
    |> function [] -> [0] | l -> l

  (* TODO check if this invarian always holds *)
  let position_of_t (t : t) = Utils.Option.get t.item.position

  let update_layer (t : t) (layer : int) =
    { t with item = { t.item with layer } }

  let update_position (t : t) (p : Wm.position) =
    { t with item = { t.item with position = Some p }}

  let make_item_name (t : t) _ =
    let typ =
      match t.item.type_ with
      | Video -> "Видео "
      | Audio -> "Аудио " in
    match t.item.pid with
    | Some pid -> typ ^ "PID:" ^ string_of_int pid
    | None     -> t.name

  let make_item_properties (t : t React.signal) _ _ =
    Item_properties.make_widget_props t

end

module Cont = Editor.Make(Container.Container_item)
module Widg = Editor.Make(Widget_item)
(*
let serialize ~(cont : Cont.t) () : (string * Wm.container) list =
  List.map (fun (n, (v : Wm.container)) ->
      let widgets =
        List.map (fun (s,(w:Wm.widget)) ->
            let position = pos_relative_to_absolute w.position v.position in
            let nw = { w with position = Some position } in
            s, nw) v.widgets in
      n, { v with widgets })
    cont.ig#layout_items
 *)
let get_free_widgets containers widgets =
  let used = List.fold_left (fun acc (_, (x : Wm.container)) ->
      x.widgets @ acc) [] containers in
  List.filter (fun (k, (v : Wm.widget)) ->
      let eq (k1, _) (k2, _) = String.equal k1 k2 in
      not @@ List.exists (eq (k,v)) used)
    widgets

let create_widgets_grid
    ~(container : Wm.container wm_item)
    ~(candidates : Widget_item.t list React.signal)
    ~(set_candidates : Widget_item.t list -> unit)
    ~(on_apply : (string * Wm.widget) list -> unit)
    ~(on_cancel : unit -> unit)
    () =
  let init_cand = React.S.value candidates in
  let cont_name = container.name in
  let cont_pos = Container_item.position_of_t container in
  let resolution = cont_pos.right - cont_pos.left,
                   cont_pos.bottom - cont_pos.top in
  let init = List.map Widget_item.t_of_layout_item container.item.widgets in
  let apply =
    Actions.make_action
      { icon = Icon.SVG.(make_simple Path.check)#widget
      ; name = "Применить" } in
  let back =
    Actions.make_action
      { icon = Icon.SVG.(make_simple Path.arrow_left)#widget
      ; name = "Назад" } in
  let dlg =
    Dialog.make
      ~actions:[ Dialog.Markup.create_action ~label:"Отмена" ~action:Close ()
               ; Dialog.Markup.create_action ~label:"ОК" ~action:Accept () ]
      ~title:(Dialog.Markup.create_title_simple "Сохранить изменения?" ())
      () in
  dlg#add_class "wm-confirmation-dialog";
  let title = Printf.sprintf "%s. Виджеты" cont_name in
  let w = Widg.make ~title
      ~init
      ~candidates
      ~set_candidates
      ~resolution
      ~actions:[back; apply]
      () in
  Lwt.async (fun () ->
      Events.clicks apply#root (fun _ _ ->
          on_apply w.ig#layout_items;
          Lwt.return_unit));
  Lwt.async (fun () ->
      Events.clicks back#root (fun _ _ ->
          let eq = Widget_item.equal in
          let filter, mem = List.(filter, fun x -> exists (eq x)) in
          let found = filter (fun x -> not @@ mem x init) w.ig#items in
          let lost = filter (fun x -> not @@ mem x w.ig#items) init in
          match found, lost with
          | [], [] ->
            on_cancel ();
            Lwt.return_unit
          | _ ->
            let open Lwt.Infix in
            dlg#open_await ()
            >|= function
            | Accept -> on_apply w.ig#layout_items
            | _ -> set_candidates init_cand; on_cancel ()));
  w.ig#append_child dlg;
  w

let switch ~grid
    ~(selected : Container_item.t Dynamic_grid.Item.t)
    ~s_state_push
    ~candidates
    ~set_candidates () =
  let on_apply widgets =
    let t = selected#value in
    let t = Container_item.update_min_size
        { t with item = { t.item with widgets }} in
    selected#set_value t;
    grid#update_item_min_size selected;
    s_state_push `Container
  in
  let on_cancel  = fun () -> s_state_push `Container in
  let w = create_widgets_grid
      ~container:selected#value
      ~candidates
      ~set_candidates
      ~on_apply
      ~on_cancel () in
  s_state_push (`Widget w)

(* let make_containers (widgets : (string * Wm.widget) list) =
 *   let open Wm_container.Container_item in
 *   let open Wm_wizard in
 *   let domains = Find.channels widgets in
 *   List.map (fun domain ->
 *       let widgets =
 *         List.filter (fun (_, (widget : Wm.widget)) ->
 *             String.equal widget.domain domain
 *           ) widgets in
 *       let name, _ = "channel", "provider" in
 *       ({ icon = Icon.SVG.(create_simple Path.contain)#widget
 *        ; name
 *        ; unique = true
 *        ; min_size = None
 *        ; item =
 *            { position = { left   = 0
 *                         ; right  = 0
 *                         ; top    = 0
 *                         ; bottom = 0 }
 *            ; widgets
 *            }
 *        } : t)
 *     ) domains
*)

let create_cells () =
  let lc =
    Layout_grid.Cell.make
      ~span_desktop:1
      ~span_tablet:1
      ~span_phone:4
      [] in
  let mc =
    Layout_grid.Cell.make
      ~span_desktop:8
      ~span_tablet:7
      ~span_phone:4
      [] in
  let rc =
    Layout_grid.Cell.make
      ~span_desktop:3
      ~span_tablet:8
      ~span_phone:4
      [] in
  lc, mc, rc

let create
    ~(init : Wm.t)
    ~(post : Wm.t -> unit Lwt.t)
    socket =
  (* Convert widgets positions to relative *)
  let wc =
    List.map Widget_item.t_of_layout_item
    @@ get_free_widgets init.layout init.widgets in
  let s_wc, s_wc_push = React.S.create wc in
  (* FIXME icon shoud be common *)
  let new_cont =
    ({ icon = Icon.SVG.(make_simple Path.contain)#widget
     ; name = "Новый контейнер"
     ; unique = false
     ; min_size = None
     ; item =
         { position = { left = 0; right = 0; top = 0; bottom = 0 }
         ; widgets = []
         }
     } : Container_item.t) in
  let containers = [new_cont] in
  let s_cc, s_cc_push = React.S.create containers in
  let wz_e, wz_push = React.E.create () in
  let wz_dlg, wz_show = Wizard.to_dialog socket init wz_push in
  let resolution = init.resolution in
  let s_state, s_state_push = React.S.create `Container in
  let title = "Контейнеры" in
  let on_remove = fun (t : Wm.container wm_item) ->
    print_endline "remove";
    let eq = Widget_item.equal in
    let ws = List.map Widget_item.t_of_layout_item t.item.widgets in
    List.iter (fun x -> Editor.remove ~eq s_wc s_wc_push x) ws in
  let wizard =
    Actions.make_action
      { icon = Icon.SVG.(make_simple Path.auto_fix)#widget
      ; name = "Авто"
      } in
  let edit =
    Actions.make_action
      { icon = Icon.SVG.(make_simple Path.pencil)#widget
      ; name = "Редактировать"
      } in
  let save =
    Actions.make_action
      { icon = Icon.SVG.(make_simple Path.content_save)#widget
      ; name = "Сохранить"
      } in
  let size =
    Actions.make_action
      { icon = Icon.SVG.(make_simple Path.aspect_ratio)#widget
      ; name = "Разрешение"
      } in
  let resolution_dlg = Resolution_dialog.make () in
  let wz = Events.clicks wizard#root (fun _ _ -> wz_show ()) in
  wizard#set_on_destroy (fun () -> Lwt.cancel wz);
  let cont =
    Cont.make ~title
      ~init:(List.map Container_item.t_of_layout_item init.layout)
      ~candidates:s_cc
      ~set_candidates:s_cc_push
      ~resolution
      ~on_remove
      ~actions:[save; wizard; size; edit]
      () in
  (* FIXME store events and signals *)
  (* let _ =
   *   React.S.map (fun x -> edit#set_disabled @@ Option.is_none x)
   *     cont.ig#s_selected in *)
  let e_edit, set_edit = React.E.create () in
  let _ =
    React.(
      E.map (fun selected ->
          switch ~grid:cont.ig
            ~s_state_push
            ~candidates:s_wc
            ~set_candidates:s_wc_push
            ~selected
            ())
      @@ E.select [ cont.ig#e_item_dblclick; e_edit ]) in
  Lwt.async (fun () ->
      Events.clicks size#root (fun _ _ ->
          resolution_dlg#show_await_resolution cont.ig#resolution
          >>= function
          | None -> Lwt.return_unit
          | Some resolution ->
            let new_layout = resize_layout ~resolution cont.ig#items in
            cont.ig#initialize resolution new_layout;
            Lwt.return_unit));
  Lwt.async (fun () ->
      Events.clicks edit#root (fun _ _ ->
          React.S.value cont.ig#s_selected |> Utils.Option.get |> set_edit;
          Lwt.return_unit));
  Lwt.async (fun () ->
      Events.clicks save#root (fun _ _ ->
          post { resolution = cont.ig#resolution
               ; widgets = init.widgets
               ; layout = cont.ig#layout_items (*serialize ~cont ()*)
               }));
  let _ =
    React.E.map (fun l ->
        let layers = Container_item.layers_of_t_list
          @@ List.map Container_item.t_of_layout_item l in
        cont.rt#initialize_layers layers;
        s_wc_push
        @@ List.map Widget_item.t_of_layout_item
        @@ get_free_widgets l init.widgets;
        cont.ig#initialize init.resolution
        @@ List.map Container_item.t_of_layout_item l;
        cont.rt#layout ();
        cont.ig#layout ()) wz_e in
  let lc, mc, rc = create_cells () in
  let add_to_view lt ig rt =
    lc#remove_children (); lc#append_child lt;
    mc#remove_children (); mc#append_child ig;
    rc#remove_children (); rc#append_child rt;
    lc#set_on_layout lt#layout;
    mc#set_on_layout ig#layout;
    rc#set_on_layout rt#layout;
    lc#layout ();
    rc#layout ();
    mc#layout ()
  in
  let _ =
    React.S.map (function
        | `Widget (w : Widg.t) -> add_to_view w.lt w.ig w.rt
        | `Container -> add_to_view cont.lt cont.ig cont.rt)
      s_state in
  cont.ig#append_child wz_dlg;
  cont.ig#append_child resolution_dlg;
  [lc; mc; rc]

let post = fun w ->
  Http_wm.set_layout w
  >>= function
  | Ok () -> Lwt.return ()
  | Error e ->
    print_endline @@ Api_js.Http.error_to_string e;
    Lwt.return ()

let on_data socket (grid : Layout_grid.t) wm =
  grid#remove_cells ();
  let cells =
    try create ~init:wm ~post socket
    with e -> Printf.printf "error: %s\n" @@ Printexc.to_string e; [] in
  List.iter grid#append_cell cells

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let () =
  let grid = Layout_grid.make [] in
  let thread =
    Http_wm.get_layout ()
    >>= fun wm ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, event) ->
    on_data socket grid wm;
    grid#add_class "wm";
    let e = React.E.map (on_data socket grid) event in
    grid#set_on_destroy (fun () ->
        React.E.stop ~strong:true e;
        React.E.stop ~strong:true event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok grid in
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let body = Ui_templates.Loader.create_widget_loader
      ~parent:scaffold#app_content_inner
      thread in
  scaffold#set_body body
