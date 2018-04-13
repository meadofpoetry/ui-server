open Containers
open Components

let create ?action ~text ~icon () =
  let _class        = "ats-placeholder" in
  let content_class = Markup.CSS.add_element _class "content"   in

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
