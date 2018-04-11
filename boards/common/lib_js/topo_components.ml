open Containers
open Components

let base_class = "topology__drawer"

let make_apply : type a b. (a option React.signal) -> (a -> (b,string) Lwt_result.t) -> Widget.widget =
  fun s f ->
  let b = new Button.t ~label:"Применить" () in
  let _ = React.S.map (function Some _ -> b#set_disabled false
                              | None   -> b#set_disabled true) s
  in
  let _ = React.E.map (fun _ -> Option.iter (fun s -> f s |> ignore) @@ React.S.value s) b#e_click in
  b#widget

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
  let scrl = new Tabs.Scroller.t ~tabs () in
  let bar  = scrl#tab_bar in
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
  let ()   = scrl#add_class @@ Markup.CSS.add_element _class "tabs" in
  Dom.appendChild box#root scrl#root;
  Dom.appendChild box#root body#root;
  box
