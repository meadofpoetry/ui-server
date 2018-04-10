open Containers

type anchor = [ `Left | `Right | `Top | `Bottom ]

let anchor_left_class   = Markup.CSS.add_modifier Markup.Drawer.drawer_class "anchor-left"
let anchor_right_class  = Markup.CSS.add_modifier Markup.Drawer.drawer_class "anchor-right"
let anchor_top_class    = Markup.CSS.add_modifier Markup.Drawer.drawer_class "anchor-top"
let anchor_bottom_class = Markup.CSS.add_modifier Markup.Drawer.drawer_class "anchor-bottom"

class type mdc =
  object
    method open_ : bool Js.t Js.prop
  end

let open_event : Dom_html.event Js.t Dom.Event.typ  = Dom_events.Typ.make "MDCTemporaryDrawer:open"
let close_event : Dom_html.event Js.t Dom.Event.typ = Dom_events.Typ.make "MDCTemporaryDrawer:close"

class t ?(animating=true) ~(anchor:anchor) ~(content:#Widget.widget list) () =
  let drawer = Markup.Drawer.create_drawer ~content:(List.map Widget.widget_to_markup content) () in
  let elt    = Markup.Drawer.create ~drawer () |> Tyxml_js.To_dom.of_element in
  let e,push = React.E.create () in
  object(self)
    val mutable _anchor = anchor
    val _s = React.S.hold false e
    val _mdc : mdc Js.t = Js.Unsafe.global##.mdc##.drawer##.MDCTemporaryDrawer##attachTo elt
    val _drawer = Widget.create @@ Tyxml_js.To_dom.of_element drawer
    inherit Widget.widget elt ()

    method anchor = _anchor
    method set_anchor (x:anchor) =
      _anchor <- x;
      match x with
      | `Left   -> _drawer#add_class anchor_left_class;
                   List.iter _drawer#remove_class [anchor_top_class; anchor_right_class; anchor_bottom_class]
      | `Top    -> _drawer#add_class anchor_top_class;
                   List.iter _drawer#remove_class [anchor_left_class; anchor_right_class; anchor_bottom_class]
      | `Right  -> _drawer#add_class anchor_right_class;
                   List.iter _drawer#remove_class [anchor_left_class; anchor_top_class; anchor_bottom_class]
      | `Bottom -> _drawer#add_class anchor_bottom_class;
                   List.iter _drawer#remove_class [anchor_left_class; anchor_top_class; anchor_right_class]

    method drawer     = _drawer
    method show       = _mdc##.open_ := Js._true
    method show_await =
      let t,w = Lwt.wait () in
      self#show;
      let l = ref None in
      l := Some (Dom_events.listen self#root close_event (fun _ _ ->
                                     Lwt.wakeup w ();
                                     Option.iter (fun x -> Dom_events.stop_listen x) !l;
                                     true));
      t
    method hide       = _mdc##.open_ := Js._false
    method s_state : bool React.signal = _s

    method private _disable_scroll =
      Dom_html.document##.body##.classList##add (Js.string Markup.Drawer.scroll_lock_class)
    method private _enable_scroll=
      Dom_html.document##.body##.classList##remove (Js.string Markup.Drawer.scroll_lock_class)

    initializer
      Dom_events.listen self#root open_event (fun _ _ -> push true; true) |> ignore;
      Dom_events.listen self#root close_event (fun _ _ -> push false; true) |> ignore;
      self#add_or_remove_class animating Markup.Drawer.animating_class;
      self#set_anchor anchor
  end
