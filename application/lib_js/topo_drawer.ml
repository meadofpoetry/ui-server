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

let make_tabs (l:(string*Widget.widget) list) =
  let hide   = fun w -> w#style##.display := Js.string "none" in
  let show   = fun w -> w#style##.display := Js.string "" in
  let _class = Markup.CSS.add_element base_class "dynamic-content" in
  let tabs = List.map (fun (n,w) -> ({ href     = None
                                     ; content  = `Text n
                                     ; disabled = false
                                     ; value    = w } : Widget.widget Tabs.tab))
                      l
  in
  let bar  = new Tabs.Tab_bar.t ~tabs () in
  let ()   = List.iter (fun t -> hide t#get_value) bar#tabs in
  let body = Dom_html.createDiv Dom_html.document |> Widget.create in
  let _    = React.S.diff (fun n o -> Option.iter (fun o -> hide o#get_value) o;
                                      Option.iter (fun n -> show n#get_value) n)
                          bar#s_active
  in
  let ()   = Option.iter (fun t -> show t#get_value) @@ React.S.value bar#s_active in
  let ()   = List.iter (fun t -> Dom.appendChild body#root t#get_value#root) bar#tabs in
  let box  = Dom_html.createDiv Dom_html.document |> Widget.create in
  let ()   = box#add_class _class in
  let ()   = body#add_class @@ Markup.CSS.add_element _class "body" in
  let ()   = bar#add_class @@ Markup.CSS.add_element _class "tabs" in
  Dom.appendChild box#root bar#root;
  Dom.appendChild box#root body#root;
  box

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
