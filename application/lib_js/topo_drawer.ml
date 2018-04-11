open Containers
open Components

let base_class = "topology__drawer"

let make_header ~title () =
  let close = new Icon.Button.Font.t ~icon:"close" () in
  let title = new Typography.Text.t ~adjust_margin:false ~font:Typography.Headline ~text:title () in
  let box   = new Box.t ~vertical:false ~widgets:[title#widget;close#widget] () in
  let ()    = box#add_class @@ Markup.CSS.add_element base_class "header" in
  let ()    = title#add_class @@ Markup.CSS.add_element base_class "title" in
  let ()    = close#add_class @@ Markup.CSS.add_element base_class "close" in
  box,close#e_click,title#set_text

let make_error e =
  let _class = Markup.CSS.add_element base_class "error" in
  let t  = new Typography.Text.t ~adjust_margin:false ~split:true ~text:e () in
  let i  = new Icon.Font.t ~icon:"error" () in
  let b  = new Box.t ~vertical:true ~widgets:[i#widget;t#widget] () in
  let () = b#add_class _class in
  b#widget

let make_progress () =
  let _class = Markup.CSS.add_element base_class "progress" in
  let w  = new Circular_progress.t ~indeterminate:true () in
  let t  = new Typography.Text.t ~adjust_margin:false ~text:"Загрузка..." () in
  let b  = new Box.t ~vertical:true ~widgets:[w#widget;t#widget] () in
  let () = b#add_class _class in
  b#widget

let make ?(anchor=`Right) ~title () =
  let header,e_close,set_title = make_header ~title () in
  let divider = new Divider.t () in
  let box     = new Box.t
                    ~vertical:(match anchor with
                               | `Bottom | `Top   -> false
                               | `Left   | `Right -> true)
                    ~widgets:[]
                    ()
  in
  let content = [header#widget;divider#widget;box#widget] in
  let drawer  = new Drawer.t ~anchor ~content () in
  let _       = React.E.map (fun _ -> drawer#hide) e_close in
  let ()      = drawer#add_class base_class in
  let ()      = box#add_class @@ Markup.CSS.add_element base_class "body" in
  drawer,box,set_title
