open Containers
open Components

type simple_tab = Tabs.content * Widget.t

let create_simple_tabs (l:simple_tab list) =
  let hide   = fun w -> w#style##.display := Js.string "none" in
  let show   = fun w -> w#style##.display := Js.string "" in
  let _class = "mdc-simple-tabs" in
  let tabs = List.map (fun (c,w) -> ({ href     = None
                                     ; content  = c
                                     ; disabled = false
                                     ; value    = w } : Widget.t Tabs.tab))
                      l
  in
  let scrl = new Tabs.Scroller.t ~tabs () in
  let bar  = scrl#tab_bar in
  let ()   = List.iter (fun t -> hide t#value) bar#tabs in
  let body = Dom_html.createDiv Dom_html.document |> Widget.create in
  let _    = React.S.diff (fun n o -> Option.iter (fun o -> hide o#value) o;
                                      Option.iter (fun n -> show n#value) n)
                          bar#s_active
  in
  let ()   = Option.iter (fun t -> show t#value) @@ React.S.value bar#s_active in
  let ()   = List.iter (fun t -> Dom.appendChild body#root t#value#root) bar#tabs in
  let ()   = body#add_class @@ Components_markup.CSS.add_element _class "body" in
  let ()   = scrl#add_class @@ Components_markup.CSS.add_element _class "tabs" in
  object(self)
    inherit Widget.t (Dom_html.createDiv Dom_html.document) () as super
    method! layout () = super#layout (); scrl#layout (); body#layout ()
    initializer
      Dom.appendChild self#root scrl#root;
      Dom.appendChild self#root body#root;
      self#add_class _class
  end
