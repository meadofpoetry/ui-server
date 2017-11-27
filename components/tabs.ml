module Tab = struct

  type tab_content = [ `Text of string | `Icon of (string * string option) | `Text_and_icon of (string * string)]

  class type mdc =
    object
      method computedWidth         : float Js.readonly_prop
      method computedLeft          : float Js.readonly_prop
      method isActive              : bool Js.t Js.prop
      method preventDefaultOnClick : bool Js.t Js.prop
    end

  class type selected_event =
    object
      inherit Dom_html.event
      method detail : < tab : mdc Js.t Js.readonly_prop > Js.t Js.readonly_prop
    end

  type events =
    { selected : selected_event Js.t Dom_events.Typ.typ
    }

  let events =
    { selected = Dom_events.Typ.make "MDCTab:selected"
    }

  class t ?href ?text ?icon () =

    let content = begin match text,icon with
                  | Some t, Some i -> `Text_and_icon (t,i)
                  | Some t, None   -> `Text t
                  | None,   Some i -> `Icon (i,None)
                  | _              -> failwith "at least icon or text must be provided for a tab widget"
                  end in

    let elt = Markup.Tabs.Tab.create ?href ~content () |> Tyxml_js.To_dom.of_a in

    object(self)

      inherit Widget.widget elt ()

      val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.tabs##.MDCTab##attachTo elt

      method anchor_element = elt

      method href       = Js.to_string self#anchor_element##.href
      method set_href s = self#anchor_element##.href := Js.string s

      method content : tab_content = content

      method active     = Js.to_bool mdc##.isActive
      method activate   = mdc##.isActive := Js._true
      method deactivate = mdc##.isActive := Js._false

      method width = mdc##.computedWidth
      method left  = mdc##.computedLeft

    end

end

module Tab_bar = struct

  class type mdc =
    object
      method tabs           : Tab.mdc Js.t Js.js_array Js.readonly_prop
      method activeTab      : Tab.mdc Js.t Js.prop
      method activeTabIndex : int Js.prop
      method layout         : unit -> unit Js.meth
    end

  class type change_event =
    object
      inherit Dom_html.event
      method detail : mdc Js.t Js.readonly_prop
    end

  type events =
    { change : change_event Js.t Dom_events.Typ.typ
    }

  let events =
    { change = Dom_events.Typ.make "MDCTabBar:change"
    }

  class t ~(tabs:Tab.t list) () =

    let typ = List.map (fun x -> match x#content with
                                 | `Text _          -> `Text
                                 | `Icon _          -> `Icon
                                 | `Text_and_icon _ -> `Text_and_icon) tabs
              |> CCList.sort_uniq
              |> (function
                  | [x] -> x
                  | _   -> failwith "All tabs must be of the same type: text, icon or text with icon") in

    let indicator = new Widget.widget (Markup.Tabs.Tab_bar.Indicator.create () |> Tyxml_js.To_dom.of_span) () in

    let elt = Markup.Tabs.Tab_bar.create ~typ
                                         ~indicator:(Widget.widget_to_markup indicator)
                                         ~tabs:(Widget.widgets_to_markup tabs) ()
              |> Tyxml_js.To_dom.of_nav in

    object(self)

      inherit Widget.widget elt ()

      val mutable tabs = tabs
      val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.tabs##.MDCTabBar##attachTo elt

      method typ : [ `Text | `Icon | `Text_and_icon ] = typ
      method indicator_widget = indicator
      method tabs             = tabs

      method active_tab_index       = mdc##.activeTabIndex
      method set_active_tab_index x = mdc##.activeTabIndex := x
      method active_tab             = CCList.get_at_idx self#active_tab_index tabs
      method set_active_tab tab     = CCList.find_idx (fun x -> x == tab) tabs
                                      |> (function
                                          | Some (idx,_) -> self#set_active_tab_index idx
                                          | None -> ())

      method get_tab_at_index i     = CCList.get_at_idx i tabs

      (* FIXME implement normally *)
      method remove_tab_at_index i  = (match self#get_tab_at_index i with
                                       | Some tab -> tabs <- CCList.remove_at_idx i tabs;
                                                     Dom.removeChild elt tab#root;
                                                     mdc##layout ()
                                       | None -> ())
      method add_tab (tab : Tab.t)  = tabs <- tabs @ [tab];
                                      Dom.appendChild elt tab#root;
                                      mdc##layout ()


    end

end

module Scroller = struct

  class type mdc =
    object
      method tabBar : Tab_bar.mdc Js.t Js.readonly_prop
    end

  class t ~(tabs:Tab.t list) () =

    let tab_bar = new Tab_bar.t ~tabs () in
    let elt = tab_bar#add_class Markup.Tabs.Scroller.scroll_frame_tabs_class;
              Markup.Tabs.Scroller.create ~tabs:(Widget.widget_to_markup tab_bar) ()
              |> Tyxml_js.To_dom.of_div in

    object
      inherit Widget.widget elt ()

      val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.tabs##.MDCTabBarScroller##attachTo elt

      method tab_bar = tab_bar
    end

end
