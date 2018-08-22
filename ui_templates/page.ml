open Containers
open Components
open Tabs

let main_class    = "main-content"
let toolbar_class = "main-toolbar"
let row_id        = "main-toolbar__tabs"

type ('a,'b) page_content =
  [ `Static  of (#Widget.t as 'b) list
  | `Dynamic of ('a, (unit -> (#Widget.t as 'b))) Tab.t list
  ]

let switch_tab (container:#Widget.t)
      (s:#Widget.t option React.signal) =
  let init   = React.S.value s in
  let load   = Option.iter (fun n -> n#layout (); container#append_child n) in
  let unload = Option.iter (fun o -> o#destroy (); container#remove_child o) in
  load init;
  React.S.diff (fun n o -> unload o; load n) s

let create_toolbar_tabs_row (tabs:('a, (unit -> #Widget.t)) Tab.t list) =
  let open Tabs in
  let bar = new Tab_bar.t ~align:Start ~tabs () in
  let section = new Toolbar.Row.Section.t ~align:`Start ~widgets:[bar] () in
  let row = new Toolbar.Row.t ~sections:[section] () in
  let () = row#set_id row_id in
  let s  = React.S.map (function
               | Some x -> Some (x#value ())
               | None   -> None) bar#scroller#s_active_tab in
  row, s

let get_arbitrary () =
  Dom_html.getElementById "arbitrary-content"
  |> Widget.create

let get_toolbar () =
  Dom_html.getElementById "main-toolbar"
  |> Widget.create

class t (content:('a,'b) page_content) () =
  let main = Dom_html.getElementById "main-content" in
  let arbitrary = get_arbitrary () in
  let toolbar = get_toolbar () in
  let title = toolbar#get_child_element_by_id "page-title"
              |> Option.get_exn
              |> Widget.create in
  object(self)

    val mutable _previous_toolbar = None
    val mutable _previous_content = None

    inherit Widget.t main ()

    method set_title x =
      Dom_html.document##.title := Js.string x;
      title#set_text_content x

    method set () =
      _previous_content <- Some arbitrary#root##.childNodes;
      arbitrary#set_empty ();
      match content with
      | `Static widgets -> List.iter arbitrary#append_child widgets
      | `Dynamic tabs   ->
         let row, s = create_toolbar_tabs_row tabs in
         (try
            let elt = Dom_html.getElementById row_id in
            _previous_toolbar <- Some elt;
            Dom.removeChild toolbar#root elt
          with _ -> ());
         switch_tab arbitrary s |> self#_keep_e;
         self#add_class
         @@ Components_markup.CSS.add_modifier main_class "dynamic";
         toolbar#append_child row

    initializer
      self#add_class main_class;
      toolbar#add_class toolbar_class;
      self#set ()
  end
