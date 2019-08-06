open Tyxml
open Application_types

module Components = Components_tyxml.Bundle.Make(Xml)(Svg)(Html)

open Components

let tab_id id = id

let tabpanel_id id = id ^ "-tabpanel"

let make_tab ?active ~id label =
  let indicator = Tab_indicator.(create ?active (create_content ()) ()) in
  let text_label = Tab.create_text_label label () in
  Tab.create ?active
    ~attrs:[ Html.a_id (tab_id id)
           ; Html.a_aria "controls" [tabpanel_id id] ]
    ~indicator
    (Tab.create_content ~text_label ()) ()

let make_tabpanel ?(active = false) ~id content =
  Html.(
    div ~a:([ a_id (tabpanel_id id)
            ; a_role ["tabpanel"]
            ; a_aria "labelledby" [tab_id id] ]
            |> fun l -> if active then l else a_hidden () :: l)
      content)

let common_tabs () =
  ["Потоки", "streams"]

let tabs_of_board ({ model; manufacturer; _ } : Topology.topo_board) =
  match manufacturer, model with
  | "DekTec", "DTM-3200" -> ["TSoIP", "tsoip"]
  | "NIITV", "DVB4CH" -> ["RF", "rf"]
  | "NIITV", "TSAN" -> ["QoS", "qos"]
  | _ -> []

let tabs_of_cpu = function
  | None -> []
  | Some _ -> ["QoE", "qoe"]

let make_tabs
    (cpu : Topology.topo_cpu option)
    (boards : Topology.topo_board list) =
  let tabs_content =
    common_tabs ()
    @ (List.flatten @@ List.map tabs_of_board boards)
    @ tabs_of_cpu cpu in
  let tabs = List.map (fun (label, id) -> make_tab ~id label) tabs_content in
  let scroll_content =
    Tab_scroller.create_scroll_content
      ~attrs:[Html.a_role ["tablist"]]
      tabs
      () in
  let scroll_area = Tab_scroller.create_scroll_area ~content:scroll_content () in
  let scroller = Tab_scroller.create ~scroll_area () in
  Tab_bar.create ~scroller (),
  Html.(
    [div ~a:[a_class ["slides"]]
       (List.map (fun (label, id) ->
            div ~a:[a_class [Layout_grid.CSS.root]]
              [make_tabpanel ~id [txt label]])
           tabs_content)])
