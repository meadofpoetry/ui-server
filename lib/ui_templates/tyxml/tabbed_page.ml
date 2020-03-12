open Components_tyxml

module CSS = struct
  let root = "mdc-tabbed-page"

  let tabpanel = BEM.add_element root "tabpanel"

  let tabpanel_content = BEM.add_element root "content"
end

let tab_id s = s

let tabpanel_id s = s ^ "-tabpanel"

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  module Tab_scroller_markup = Tab_scroller.Make (Xml) (Svg) (Html)
  module Tab_bar_markup = Tab_bar.Make (Xml) (Svg) (Html)
  module Tab_markup = Tab.Make (Xml) (Svg) (Html)
  module Glide_markup = Components_lab_tyxml.Glide.Make (Xml) (Svg) (Html)
  open Html

  type tab = { id : string; name : string; children : Xml.elt list }

  let create_tab ?classes ?a ~id ~name () =
    Tab_markup.tab
      ~a:[ a_id (tab_id id); a_aria "controls" [ tabpanel_id id ] ]
      ~text_label:name ()

  let create_tab_bar ?classes ?a ?(tabs = []) () =
    let scroll_content =
      Tab_scroller_markup.tab_scroller_scroll_content
        ~a:[ a_role [ "tablist" ] ]
        ~tabs ()
    in
    let scroll_area =
      Tab_scroller_markup.tab_scroller_scroll_area ~scroll_content ()
    in
    let scroller = Tab_scroller_markup.tab_scroller ~scroll_area () in
    Tab_bar_markup.tab_bar ?classes ?a ~scroller ()

  let create_tabpanel_content ?(classes = []) ?(a = []) ?(children = []) () =
    let classes = CSS.tabpanel_content :: Layout_grid.CSS.root :: classes in
    div ~a:(a_class classes :: a) (Html.totl children)

  let create_tabpanel ?(classes = []) ?(a = []) ~id ?children () =
    let classes = CSS.tabpanel :: classes in
    Glide_markup.glide_slide ~classes
      ~a:
        ( a_id (tabpanel_id id)
        :: a_role [ "tabpanel" ]
        :: a_aria "labelledby" [ tab_id id ]
        :: a )
      ?children ()

  let create ?(classes = []) ?(a = []) ?(children = []) () =
    let classes = CSS.root :: classes in
    let tabs, slides =
      List.split
      @@ List.map
           (fun { id; name; children } ->
             let tab = create_tab ~id ~name () in
             let tabpanel =
               create_tabpanel ~id
                 ~children:[ create_tabpanel_content ~children () ]
                 ()
             in
             (tab, tabpanel))
           children
    in
    let tab_bar = create_tab_bar ~tabs () in
    let glide = Glide_markup.glide ~classes ~slides () in
    (tab_bar, glide)
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
