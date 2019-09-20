open Components_tyxml

module CSS = struct
  let root = "page"

  let tabpanel = "tabpanel"

  let tabpanel_content = BEM.add_element tabpanel "content"
end

let tab_id s = s ^ "-tab"

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  module Tab_bar_markup = Tab_bar.Make (Xml) (Svg) (Html)
  module Tab_scroller_markup = Tab_scroller.Make (Xml) (Svg) (Html)
  module Glide_markup = Components_lab_tyxml.Glide.Make (Xml) (Svg) (Html)
  open Html

  let create_tab_bar ?classes ?a ?(tabs = []) () =
    let scroll_content =
      Tab_scroller_markup.tab_scroller_scroll_content ~a:[a_role ["tablist"]] ~tabs ()
    in
    let scroll_area = Tab_scroller_markup.tab_scroller_scroll_area ~scroll_content () in
    let scroller = Tab_scroller_markup.tab_scroller ~scroll_area () in
    Tab_bar_markup.tab_bar ?classes ?a ~scroller ()

  let create_tabpanel_content ?(classes = []) ?(a = []) ?(children = []) () =
    let classes = CSS.tabpanel_content :: Layout_grid.CSS.root :: classes in
    div ~a:(a_class classes :: a) children

  let create_tabpanel ?(classes = []) ?(a = []) ~id ?children () =
    let classes = CSS.tabpanel :: classes in
    Glide_markup.glide_slide
      ~classes
      ~a:(a_id id :: a_role ["tabpanel"] :: a_aria "labelledby" [tab_id id] :: a)
      ?children
      ()

  let create ?(classes = []) ?(a = []) ?(children = []) () =
    let classes = CSS.root :: classes in
    Glide_markup.glide
      ~a:(a_class classes :: a)
      ~slides:
        (List.map
           (fun (id, x) ->
             create_tabpanel ~id ~children:[create_tabpanel_content ~children:x ()] ())
           children)
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
