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

let switch_tab (container:#Dom.node Js.t) (s:#Widget.t option React.signal) =
  let init   = React.S.value s in
  let load   = Option.iter (fun n ->
                   n#layout ();
                   Dom.appendChild container n#root) in
  let unload = Option.iter (fun o ->
                   o#destroy ();
                   (try Dom.removeChild container o#root with _ -> ())) in
  load init;
  React.S.diff (fun n o -> unload o; load n) s

let create_toolbar_tabs_row (tabs:('a, (unit -> #Widget.t)) Tab.t list) =
  let open Tabs in
  let bar  = new Tab_bar.t ~align:Start ~tabs () in
  (* let s    = React.S.map (function
   *                | Some x -> Some (x#value ())
   *                | None   -> None) bar#tab_bar#s_active in *)
  let section = new Toolbar.Row.Section.t ~align:`Start ~widgets:[bar] () in
  let row     = new Toolbar.Row.t ~sections:[section] () in
  (* FIXME move to css *)
  let () = (Js.Unsafe.coerce row#style)##.alignItems := Js.string "flex-end" in
  let () = (Js.Unsafe.coerce section#style)##.alignItems := Js.string "flex-end" in
  (* let () = (Js.Unsafe.coerce bar#style)##.flexGrow := 1 in *)
  (* let () = bar#set_on_load @@ Some bar#layout in *)
  let () = row#set_id row_id in
  row, (* s *) React.S.const None

class t (content:('a,'b) page_content) () =
  let main      = Dom_html.getElementById "main-content" in
  let arbitrary = Dom_html.getElementById "arbitrary-content" |> Widget.create in
  let toolbar   = Dom_html.getElementById "main-toolbar" |> Widget.create in
  let row, s    = match content with
    | `Static _     -> None, React.S.const None
    | `Dynamic tabs ->
       let w, s = create_toolbar_tabs_row tabs in
       Some w, s in
  object(self)

    val mutable _previous_toolbar = None
    val mutable _previous_content = None

    inherit Widget.t main ()

    method set () =
      _previous_content <- Some arbitrary#root##.childNodes;
      arbitrary#set_empty ();
      match content with
      | `Static widgets ->
         List.iter (fun x -> Dom.appendChild arbitrary#root x#root) widgets
      | `Dynamic _ ->
         (try
            let elt = Dom_html.getElementById row_id in
            _previous_toolbar <- Some elt;
            Dom.removeChild toolbar#root elt
          with _ -> ());
         let _ = switch_tab arbitrary#root s in
         self#add_class
         @@ Components_markup.CSS.add_modifier main_class "dynamic";
         Dom.appendChild toolbar#root (Option.get_exn row)#root;

    initializer
      self#add_class main_class;
      toolbar#add_class toolbar_class;
      self#set ()
  end
