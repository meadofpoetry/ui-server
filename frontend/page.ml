open Containers
open Components

let main_class    = "main-content"
let toolbar_class = "main-toolbar"

type ('a,'b) page_content = [ `Static  of (#Widget.widget as 'a) list
                            | `Dynamic of (unit -> (#Widget.widget as 'b)) Tabs.tab list
                            ]

let remove_children container =
  Dom.list_of_nodeList @@ container##.childNodes
  |> List.iter (fun x -> Dom.removeChild container x)

let switch_tab (container:#Dom.node Js.t) (s:#Widget.widget option React.signal) =
  let init   = React.S.value s in
  let load   = Option.iter (fun n -> n#layout (); Dom.appendChild container n#root) in
  let unload = Option.iter (fun o -> o#destroy (); (try Dom.removeChild container o#root with _ -> ())) in
  load init;
  React.S.diff (fun n o -> unload o; load n) s

let create_toolbar_tabs_row (tabs:(unit -> #Widget.widget) Tabs.tab list) =
  let open Tabs in
  let bar  = new Tabs.Scroller.t ~tabs () in
  let s    = React.S.map (function
                          | Some x -> Some (x#value ())
                          | None   -> None) bar#tab_bar#s_active in
  let section = new Toolbar.Row.Section.t ~align:`Start ~widgets:[bar] () in
  let row     = new Toolbar.Row.t ~sections:[section] () in
  (* FIXME move to css *)
  let ()      = (Js.Unsafe.coerce row#style)##.alignItems := Js.string "flex-end" in
  let ()      = (Js.Unsafe.coerce section#style)##.alignItems := Js.string "flex-end" in
  let ()      = (Js.Unsafe.coerce bar#style)##.flexGrow := 1 in
  let ()      = bar#set_on_load @@ Some bar#layout in
  row,s

class t (content:('a,'b) page_content) () =
  let main      = Dom_html.getElementById "main-content" in
  let arbitrary = Dom_html.getElementById "arbitrary-content" |> Widget.create in
  let toolbar   = Dom_html.getElementById "main-toolbar" |> Widget.create in
  object(self)
    inherit Widget.widget main ()
    initializer
      self#add_class main_class;
      toolbar#add_class toolbar_class;
      match content with
      | `Static widgets -> List.iter (fun x -> Dom.appendChild arbitrary#root x#root) widgets
      | `Dynamic tabs   -> let row,s = create_toolbar_tabs_row tabs in
                           self#add_class @@ Markup.CSS.add_modifier main_class "dynamic";
                           let _     = switch_tab arbitrary#root s in
                           Dom.appendChild toolbar#root row#root;
  end
