open Components_tyxml

module CSS = struct
  let slides = "slides"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  module Components = Bundle.Make (Xml) (Svg) (Html)
  open Html
  open Components

  let make_tab ?active ~id ~controls label =
    Tab.create
      ?active
      ~text_label:(`Text label)
      ~attrs:[a_id id; a_aria "controls" [controls]]
      ()

  let make_tab_bar ?classes ?attrs tabs =
    let scroll_content =
      Tab_scroller.create_scroll_content ~attrs:[a_role ["tablist"]] ~tabs ()
    in
    let scroll_area = Tab_scroller.create_scroll_area ~scroll_content () in
    let scroller = Tab_scroller.create ~scroll_area () in
    Tab_bar.create ?classes ?attrs ~scroller ()

  let make_tabpanel ?(active = false) ~id ~labelledby content =
    div
      ~a:
        ([a_id id; a_role ["tabpanel"]; a_aria "labelledby" [labelledby]]
        |> fun l -> if active then l else a_hidden () :: l)
      content

  let make_content ?(classes = []) ?(attrs = []) panels =
    let classes = CSS.slides :: classes in
    div ~a:([a_class classes] @ attrs)
    @@ List.map (fun panel -> div ~a:[a_class [Layout_grid.CSS.root]] [panel]) panels
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
