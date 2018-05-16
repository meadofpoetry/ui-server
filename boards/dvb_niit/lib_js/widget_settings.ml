open Containers
open Components
open Lwt_result.Infix

type config =
  { ids : int list option
  } [@@deriving yojson]

let default_config = { ids = None }

let name     = "Настройки"
let settings = None

let make ~(state:  Common.Topology.state React.signal)
         ~(config: Board_types.config React.signal)
         (conf:    config option)
         control =
  let conf = Option.get_or ~default:default_config conf in
  let ids  = match conf.ids with Some x -> x | None -> List.map fst @@ React.S.value config in
  List.map (fun id -> `Text (Printf.sprintf "Модуль %d" (succ id)),
                      (Widget_module_settings.make ~state ~config (Some {id}) control))
  @@ List.sort compare ids
  |> Ui_templates.Tabs.create_simple_tabs
  |> Widget.coerce
