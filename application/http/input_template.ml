open Tyxml
open Application_types

module Components = Components_tyxml.Bundle.Make(Xml)(Svg)(Html)

open Components

let make_tab ?active label =
  let indicator = Tab_indicator.(create ?active (create_content ()) ()) in
  let text_label = Tab.create_text_label label () in
  Tab.create ?active ~indicator (Tab.create_content ~text_label ()) ()

let common_tabs () =
  ["Потоки"]

let tabs_of_board ({ model; manufacturer; _ } : Topology.topo_board) =
  match manufacturer, model with
  | "DekTec", "DTM-3200" -> ["TSoIP"]
  | "NIITV", "DVB4CH" -> ["RF"]
  | "NIITV", "TSAN" -> ["QoS"]
  | _ -> []

let tabs_of_cpu = function
  | None -> []
  | Some _ -> ["QoE"]

let make_tab_bar
    (cpu : Topology.topo_cpu option)
    (boards : Topology.topo_board list) =
  let tabs =
    List.mapi (fun i x ->
        let active = i = 0 in
        make_tab ~active x)
      (common_tabs ()
       @ (List.flatten @@ List.map tabs_of_board boards)
       @ tabs_of_cpu cpu) in
  let scroll_area =
    Tab_scroller.(
      create_scroll_area
        ~content:(create_scroll_content tabs ())
        ()) in
  let scroller = Tab_scroller.create ~scroll_area () in
  Tab_bar.create ~scroller ()
