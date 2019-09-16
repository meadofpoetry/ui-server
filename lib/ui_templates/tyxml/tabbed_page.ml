open Components_tyxml

module CSS = struct
  let root = "page"

  let tabpanel = "tabpanel"

  let tabpanel_content = BEM.add_element tabpanel "content"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  module Components = Bundle.Make (Xml) (Svg) (Html)
  module Glide_markup = Components_lab_tyxml.Glide.Make (Xml) (Svg) (Html)
  open Html
  open Components

  let create_tab_bar ?classes ?attrs ?(tabs = []) () =
    let scroll_content =
      Tab_scroller.create_scroll_content ~attrs:[a_role ["tablist"]] ~tabs ()
    in
    let scroll_area = Tab_scroller.create_scroll_area ~scroll_content () in
    let scroller = Tab_scroller.create ~scroll_area () in
    Tab_bar.create ?classes ?attrs ~scroller ()

  let create_tabpanel_content ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.tabpanel_content :: Layout_grid.CSS.root :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create_tabpanel ?(classes = []) ?(attrs = []) ~id ?children () =
    let classes = CSS.tabpanel :: classes in
    Glide_markup.create_slide
      ~classes
      ~attrs:([a_id id; a_role ["tabpanel"]; a_aria "labelledby" [id]] @ attrs)
      ?children
      ()

  let create ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.root :: classes in
    Glide_markup.create
      ~attrs:([a_class classes] @ attrs)
      ~slides:
        (List.map
           (fun (id, x) ->
             create_tabpanel ~id ~children:[create_tabpanel_content ~children:x ()] ())
           children)
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
