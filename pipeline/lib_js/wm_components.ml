open Containers
open Components

module Utils = struct

  let rm_children container =
    Dom.list_of_nodeList @@ container##.childNodes
    |> List.iter (fun x -> Dom.removeChild container x)

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

  let make_container_props (item : (string * Wm.container) Dynamic_grid.Item.t) =
    let (name,cont) = item#get_value in
    let name = new Text_row.t ~label:"Имя" ~text:name () in
    let num  = new Text_row.t
                   ~label:"Количество виджетов"
                   ~text:(string_of_int @@ List.length cont.widgets)
                   ()
    in
    let box  = new Box.t ~vertical:true ~widgets:[ name#widget; num#widget ] () in
    Wm_types.({ widget = box#widget; actions = [ ] })

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
