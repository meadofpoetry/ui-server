open Containers
open Components
open Pipeline_types

module Utils = struct

  let gcd = Components.Utils.gcd
  let resolution_to_aspect = Components.Utils.resolution_to_aspect

  let get_factors i =
    let rec aux acc cnt =
      if cnt = 0 then acc
      else (if i mod cnt = 0 then aux (cnt :: acc) (pred cnt)
            else aux acc (pred cnt))
    in
    aux [] i

  let get_grids ~resolution ~positions () =
    let cmp = Pair.compare compare compare in
    let (w, h) = resolution in
    let (ax, ay) = resolution_to_aspect resolution in
    let grids =
      List.map (fun factor -> let c = w / factor in c, c * ay / ax)
      @@ get_factors (gcd w h)
      |> List.filter (fun (c,r) ->
             let cw,rh = w / c, h / r in
             List.fold_while (fun _ (x:Wm.position) ->
                 if x.left mod cw = 0 && x.right mod cw = 0
                    && x.top mod rh = 0 && x.bottom mod rh = 0
                 then true,`Continue
                 else false,`Stop) true positions)
    in
    List.sort cmp grids

  let get_best_grid ?(cols = 90) ~resolution grids =
    let cmp = Pair.compare compare compare in
    let grids = List.sort cmp grids in
    List.fold_left (fun acc (c,r) ->
        if (c - cols) < (fst acc - cols) && c - cols > 0
        then (c, r) else acc) resolution grids

  let to_grid_position (pos : Wm.position) : Dynamic_grid.Position.t =
    { x = pos.left
    ; y = pos.top
    ; w = pos.right - pos.left
    ; h = pos.bottom - pos.top }
  let of_grid_position (pos : Dynamic_grid.Position.t) : Wm.position =
    { left = pos.x
    ; right = pos.w + pos.x
    ; top = pos.y
    ; bottom = pos.y + pos.h }

  let center ~(parent : Wm.position) ~(pos : Wm.position) () : Wm.position =
    let parent = to_grid_position parent in
    let child = to_grid_position pos in
    let x = parent.x + ((parent.w - child.w) / 2) in
    let y = parent.y + ((parent.h - child.h) / 2) in
    of_grid_position { child with x; y }

end

module Text_row = struct

  class t ?icon ?text ?s ~label () =
    let _class = "wm-property-row" in
    let nw = new Typography.Text.t ~text:label () in
    let vw = match icon with
      | Some icon -> (new Icon.Font.t ~icon ())#widget
      | None -> let text = Option.get_or ~default:"-" text in
                (new Typography.Text.t ~text ())#widget
    in
    object
      inherit Hbox.t ~halign:`Space_between
                ~widgets:[Widget.coerce nw; Widget.coerce vw]
                () as super
      val mutable _s = None
      method! init () : unit =
        super#init ();
        super#add_class _class;
        vw#add_class @@ Markup.CSS.add_element _class "value";
        nw#add_class @@ Markup.CSS.add_element _class "label";
        match s with
        | None -> ()
        | Some s -> _s <- Some (React.S.map (fun x -> vw#set_text_content x) s)
      method! destroy () : unit =
        super#destroy ();
        Option.iter (React.S.stop ~strong:true) _s;
        _s <- None
      method has_icon = icon
      method get_value_widget = vw
      method get_label_widget = nw
    end

end

module Item_info = struct

  (* FIXME use bem *)
  let _class = "wm-grid-item__info"
  let line_class = Markup.CSS.add_element _class "line"

  let make_info icon info =
    let icon = new Icon.Font.t ~icon () in
    let info = List.map (fun (label, text) ->
                   new Text_row.t ~label ~text () |> Widget.coerce) info in
    let box = new Vbox.t ~widgets:([icon#widget] @ info) () in
    box#add_class _class;
    box#widget

  let make_container_info (item : Wm.container Wm_types.wm_item) =
    let text = new Typography.Text.t ~text:item.name () in
    let line_1 = new Hbox.t ~valign:`Center
                   ~widgets:[item.icon; text#widget] () in
    let lines = [line_1#widget] in
    let box = new Vbox.t ~widgets:lines () in
    line_1#add_class
    @@ Markup.CSS.add_modifier line_class "with-icon";
    List.iter (fun x -> x#add_class line_class) lines;
    box#add_class _class;
    box#widget

  let make_widget_info (item:Wm.widget Wm_types.wm_item) =
    let text =
      let typ = match item.item.type_ with
        | Video -> "Видео"
        | Audio -> "Аудио" in
      let pid = match item.item.pid with
        | None -> ""
        | Some pid -> string_of_int pid in
      Printf.sprintf "%s PID:%s" typ pid in
    let line_2 = new Typography.Text.t ~text () in
    (* let line_3 = new Typography.Text.t ~text:item.item.description () in *)
    let lines = [line_2#widget] in
    let box = new Vbox.t ~widgets:lines () in
    line_2#add_class
    @@ Markup.CSS.add_modifier line_class "with-icon";
    List.iter (fun x -> x#add_class line_class) lines;
    box#add_class _class;
    box#widget


end

module Item_properties = struct

  type t_cont = Wm.container Wm_types.wm_item
  type t_widg = Wm.widget Wm_types.wm_item

  let make_container_props (t : t_cont React.signal) =
    let s_name = React.S.map (fun (x : t_cont) -> x.name) t in
    let name = new Text_row.t ~label:"Имя" ~s:s_name () in
    let s_num = React.S.map (fun (x:t_cont) ->
                    string_of_int @@ List.length x.item.widgets) t in
    let num  = new Text_row.t ~label:"Количество виджетов" ~s:s_num () in
    let box  = new Vbox.t ~widgets:[name#widget; num#widget] () in
    Wm_types.{ widget = box#widget; actions = [] }

  let make_video_props (t : t_widg React.signal) =
    let v = React.S.value t in
    let typ = new Text_row.t ~label:"Тип" ~text:"Видео" () in
    let aspect = new Text_row.t ~label:"Аспект"
                   ~text:(Wm.aspect_to_string v.item.aspect) () in
    let descr = new Text_row.t ~label:"Описание"
                  ~text:v.item.description () in
    let box = new Vbox.t ~widgets:[typ;aspect;descr] () in
    Wm_types.{ widget = box#widget; actions = [] }

  let make_audio_props (t : t_widg React.signal) =
    let v = React.S.value t in
    let typ = new Text_row.t ~label:"Тип" ~text:"Аудио" () in
    let aspect = new Text_row.t ~label:"Аспект"
                   ~text:(Wm.aspect_to_string v.item.aspect) () in
    let descr = new Text_row.t ~label:"Описание"
                  ~text:v.item.description () in
    let box = new Vbox.t ~widgets:[typ;aspect;descr] () in
    Wm_types.{ widget = box#widget; actions = [] }

  let make_widget_props (t : t_widg React.signal) =
    match (React.S.value t).item.type_ with
    | Video -> make_video_props t
    | Audio -> make_audio_props t

end

module Settings_dialog = struct

  let make config =
    let open Wm_types in
    let show_grid_switch = new Switch.t () in
    let show_grid = new Form_field.t ~align_end:true
                      ~input:show_grid_switch
                      ~label:"Показывать сетку" () in
    let box = new Vbox.t ~widgets:[show_grid#widget] () in
    let accept = new Button.t ~label:"Применить" () in
    let d =
      new Dialog.t
        ~title:"Настройки редактора мозаики"
        ~actions:[Dialog.Action.make ~typ:`Accept accept]
        ~content:(`Widgets [ box#widget ])
        () in
    let s, push = React.S.create config in
    let _ =
      React.E.map (function
          | `Accept -> let v = React.S.value show_grid_switch#s_state in
                       push { show_grid_lines = v }
          | `Cancel -> ()) d#e_action in
    show_grid_switch#set_checked config.show_grid_lines;
    d#add_class "wm-editor-config-dialog";
    d, s

end

module Placeholder = struct
  let make ~text ~icon () =
    let ph = Ui_templates.Placeholder.create_with_icon ~text ~icon () in
    ph#add_class "wm-placeholder";
    ph
end
