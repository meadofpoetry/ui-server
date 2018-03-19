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
    let w,h   = resolution in
    let x,y   = resolution_to_aspect (w,h) in
    if cols >= w      then resolution
    else if x >= cols then (x,y)
    else (List.fold_left (fun acc (c,r) -> if (c - cols) < (fst acc - cols) && c - cols > 0
                                           then (c,r) else acc) resolution grids)

end

module Text_row = struct

  class t ?icon ?text ?e ~label () =
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
        Option.iter (fun e -> React.E.map (fun s -> vw#set_text_content s) e |> ignore) e;
        super#set_justify_content `Space_between
    end

end

module Item_properties = struct

  type t_cont = Wm.container Wm_types.wm_item
  type t_widg = Wm.widget Wm_types.wm_item

  let make_container_props (t:Wm.container Wm_types.wm_item) =
    let name = new Text_row.t ~label:"Имя" ~text:t.name () in
    let num  = new Text_row.t
                   ~label:"Количество виджетов"
                   ~text:(string_of_int @@ List.length t.item.widgets)
                   ()
    in
    let box  = new Box.t ~vertical:true ~widgets:[ name#widget; num#widget ] () in
    Wm_types.({ widget = box#widget; actions = [ ] })

  let make_video_props (t:t_widg) _ =
    let typ    = new Text_row.t ~label:"Тип" ~text:"Видео" () in
    let aspect = new Text_row.t ~label:"Аспект" ~text:(Printf.sprintf "%dx%d"
                                                                      (fst t.item.aspect)
                                                                      (snd t.item.aspect)) () in
    let descr  = new Text_row.t ~label:"Описание" ~text:t.item.description () in
    let box    = new Box.t ~widgets:[typ;aspect;descr] () in
    Wm_types.({ widget = box#widget; actions = [] })

  let make_audio_props (t:t_widg) _ =
    let typ    = new Text_row.t ~label:"Тип" ~text:"Аудио" () in
    let aspect = new Text_row.t ~label:"Аспект" ~text:(Printf.sprintf "%dx%d"
                                                                      (fst t.item.aspect)
                                                                      (snd t.item.aspect)) () in
    let descr  = new Text_row.t ~label:"Описание" ~text:t.item.description () in
    let box    = new Box.t ~widgets:[typ;aspect;descr] () in
    Wm_types.({ widget = box#widget; actions = [] })

  let make_widget_props (t:t_widg) (other:t_widg list) =
    let other = List.filter (fun (x:t_widg) -> String.equal x.item.type_ t.item.type_) other in
    match t.item.type_ with
    | "video" -> make_video_props t other
    | "audio" -> make_audio_props t other
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
