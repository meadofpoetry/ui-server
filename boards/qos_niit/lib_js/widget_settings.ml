open Containers
open Components
open Board_types
open Lwt_result.Infix
open Common.Topology

type config = unit [@@deriving yojson]

let name     = "Настройки"
let settings = None

let make ~(state:   Common.Topology.state React.signal)
         ~(config:  Board_types.config React.signal)
         ~(streams: Common.Stream.t_list React.signal)
         (conf:     config option)
         control =
  let t2mi   = Widget_t2mi_settings.make ~state ~streams ~config None control in
  let jitter = Widget_jitter_settings.make ~state ~config None control in
  [ (`Text "T2-MI", t2mi); (`Text "Джиттер", jitter) ]
  |> Ui_templates.Tabs.create_simple_tabs
  |> Widget.coerce

