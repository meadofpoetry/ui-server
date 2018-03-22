open Containers
open Components

module Utils = struct

  let gcd                  = Components.Utils.gcd
  let resolution_to_aspect = Components.Utils.resolution_to_aspect

  let rm_children container =
    Dom.list_of_nodeList @@ container##.childNodes
    |> List.iter (fun x -> Dom.removeChild container x)

  let get_factors i =
    let rec aux acc cnt =
      if cnt = 0 then acc
      else (if i mod cnt = 0 then aux (cnt :: acc) (pred cnt) else aux acc (pred cnt))
    in
    aux [] i

  let get_grids ~resolution ~positions () =
    let cmp      = Pair.compare compare compare in
    let (w,h)    = resolution in
    let (ax,ay)  = resolution_to_aspect resolution in
    let grids    = List.map (fun factor -> let c = w / factor in c, c * ay / ax) @@ get_factors (gcd w h)
                   |> List.filter (fun (c,r) ->
                          let cw,rh = w / c, h / r in
                          List.fold_while (fun _ (x:Wm.position) ->
                              if x.left mod cw = 0 && x.right mod cw = 0 && x.top mod rh = 0 && x.bottom mod rh = 0
                              then true,`Continue
                              else false,`Stop) true positions)
    in
    List.sort cmp grids

  let get_best_grid ?(cols=90) ~resolution grids =
    let cmp   = Pair.compare compare compare in
    let grids = List.sort cmp grids in
    List.fold_left (fun acc (c,r) -> if (c - cols) < (fst acc - cols) && c - cols > 0
                                     then (c,r) else acc) resolution grids

  let to_grid_position (pos:Wm.position) : Dynamic_grid.Position.t =
    {x = pos.left; y = pos.top; w = pos.right - pos.left; h = pos.bottom - pos.top }
  let of_grid_position (pos:Dynamic_grid.Position.t) : Wm.position =
    { left = pos.x; right = pos.w + pos.x; top = pos.y; bottom = pos.y + pos.h }

  let center ~(parent:Wm.position) ~(pos:Wm.position) () : Wm.position =
    let parent = to_grid_position parent in
    let child  = to_grid_position pos in
    let x      = parent.x   + ((parent.w - child.w) / 2) in
    let y      = parent.y   + ((parent.h - child.h) / 2) in
    let pos    = {child with x;y} |> of_grid_position in
    pos

end

module Text_row = struct

  class t ?icon ?text ?s ~label () =
    let _class = "wm-property-row" in
    let nw = new Typography.Text.t ~text:label () in
    let vw = match icon with
      | Some icon -> (new Icon.Font.t ~icon ())#widget
      | None      -> let text = Option.get_or ~default:"-" text in
                     (new Typography.Text.t ~text ())#widget
    in
    object(self)
      inherit Box.t ~vertical:false ~widgets:[Widget.coerce nw; Widget.coerce vw] () as super
      method has_icon = icon
      method get_value_widget = vw
      method get_label_widget = nw
      initializer
        self#add_class _class;
        vw#add_class @@ Markup.CSS.add_element _class "value";
        nw#add_class @@ Markup.CSS.add_element _class "label";
        Option.iter (fun s -> React.S.map (fun x -> vw#set_text_content x) s |> ignore) s;
        super#set_justify_content `Space_between
    end

end

module Item_info = struct

  let _class     = "wm-grid-item__info"
  let line_class = Markup.CSS.add_element _class "line"

  let make_info icon info =
    let icon = new Icon.Font.t ~icon () in
    let info = List.map (fun (label,text) -> new Text_row.t ~label ~text () |> Widget.coerce) info in
    let box  = new Box.t ~vertical:true ~widgets:([icon#widget] @ info) () in
    let ()   = box#add_class _class in
    box#widget

  let make_container_info (item:Wm.container Wm_types.wm_item) =
    let icon   = new Icon.Font.t ~icon:item.icon () in
    let text   = new Typography.Text.t ~text:item.name () in
    let line_1 = new Box.t ~vertical:false ~widgets:[icon#widget;text#widget] () in
    let lines  = [line_1#widget] in
    let box    = new Box.t ~vertical:true ~widgets:lines () in
    let ()     = line_1#add_class @@ Markup.CSS.add_modifier line_class "with-icon" in
    let ()     = line_1#set_align_items `Center in
    let ()     = List.iter (fun x -> x#add_class line_class) lines in
    let ()     = box#add_class _class in
    box#widget

  let make_widget_info (item:Wm.widget Wm_types.wm_item) =
    let icon = new Icon.Font.t ~icon:item.icon () in
    let text = new Typography.Text.t ~text:item.name () in
    let line_1 = new Box.t ~vertical:false ~widgets:[icon#widget;text#widget] () in
    let line_2 = new Typography.Text.t ~text:item.item.domain () in
    let line_3 = new Typography.Text.t ~text:item.item.description () in
    let lines  = [line_1#widget;line_2#widget;line_3#widget] in
    let box    = new Box.t ~vertical:true ~widgets:lines () in
    let ()     = line_1#add_class @@ Markup.CSS.add_modifier line_class "with-icon" in
    let ()     = line_1#set_align_items `Center in
    let ()     = List.iter (fun x -> x#add_class line_class) lines in
    let ()     = box#add_class _class in
    box#widget


end

module Item_properties = struct

  type t_cont = Wm.container Wm_types.wm_item
  type t_widg = Wm.widget Wm_types.wm_item

  let make_container_props (t:t_cont React.signal) =
    let s_name = React.S.map (fun (x:t_cont) -> x.name) t in
    let name   = new Text_row.t ~label:"Имя" ~s:s_name () in
    let s_num  = React.S.map (fun (x:t_cont) -> string_of_int @@ List.length x.item.widgets) t in
    let num  = new Text_row.t ~label:"Количество виджетов" ~s:s_num () in
    let box  = new Box.t ~vertical:true ~widgets:[ name#widget; num#widget ] () in
    Wm_types.({ widget = box#widget; actions = [ ] })

  let make_video_props (t:t_widg React.signal) =
    let v      = React.S.value t in
    let typ    = new Text_row.t ~label:"Тип" ~text:"Видео" () in
    let aspect = new Text_row.t ~label:"Аспект" ~text:(Printf.sprintf "%dx%d"
                                                                      (fst v.item.aspect)
                                                                      (snd v.item.aspect)) () in
    let descr  = new Text_row.t ~label:"Описание" ~text:v.item.description () in
    let box    = new Box.t ~widgets:[typ;aspect;descr] () in
    Wm_types.({ widget = box#widget; actions = [] })

  let make_audio_props (t:t_widg React.signal) =
    let v      = React.S.value t in
    let typ    = new Text_row.t ~label:"Тип" ~text:"Аудио" () in
    let aspect = new Text_row.t ~label:"Аспект" ~text:(Printf.sprintf "%dx%d"
                                                                      (fst v.item.aspect)
                                                                      (snd v.item.aspect)) () in
    let descr  = new Text_row.t ~label:"Описание" ~text:v.item.description () in
    let box    = new Box.t ~widgets:[typ;aspect;descr] () in
    Wm_types.({ widget = box#widget; actions = [] })

  let make_widget_props (t:t_widg React.signal) =
    match (React.S.value t).item.type_ with
    | "video" -> make_video_props t
    | "audio" -> make_audio_props t
    | _       -> let widget = new Typography.Text.t ~text:"Unknown" () in
                 Wm_types.({ widget = widget#widget; actions = [] })

end

module Settings_dialog = struct

  let make config =
    let open Wm_types in
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

end

module Placeholder = struct

  let make ?action ~text ~icon () =
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

end
