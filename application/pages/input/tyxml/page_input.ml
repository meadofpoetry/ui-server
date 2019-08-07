open Components_tyxml

module CSS = struct
  let slides = "slides"
end

let tab_id id = id

let tabpanel_id id = id ^ "-tabpanel"

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap
     with module Xml := Xml
      and module Svg := Svg) = struct

  module Components = Bundle.Make(Xml)(Svg)(Html)

  open Html
  open Components

  let make_tab ?active ~id label =
    let indicator = Tab_indicator.(create ?active (create_content ()) ()) in
    let text_label = Tab.create_text_label label () in
    Tab.create ?active
      ~attrs:[ a_id (tab_id id)
             ; a_aria "controls" [tabpanel_id id] ]
      ~indicator
      (Tab.create_content ~text_label ()) ()

  let make_tab_bar ?classes ?attrs tabs =
    let scroll_content =
      Tab_scroller.create_scroll_content
        ~attrs:[a_role ["tablist"]]
        tabs
        () in
    let scroll_area =
      Tab_scroller.create_scroll_area
        ~content:scroll_content
        () in
    let scroller = Tab_scroller.create ~scroll_area () in
    Tab_bar.create ?classes ?attrs ~scroller ()

  let make_tabpanel ?(active = false) ~id content =
    div ~a:([ a_id (tabpanel_id id)
            ; a_role ["tabpanel"]
            ; a_aria "labelledby" [tab_id id] ]
            |> fun l -> if active then l else a_hidden () :: l)
      content

  let make_content ?(classes = []) ?(attrs = []) panels =
    let classes = CSS.slides :: classes in
    div ~a:([a_class classes] @ attrs)
    @@ List.map (fun panel ->
        div ~a:[a_class [Layout_grid.CSS.root]]
          [panel]) panels

end
