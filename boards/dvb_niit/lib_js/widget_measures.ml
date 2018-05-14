open Containers
open Components
open Board_types
open Lwt_result.Infix

type config =
  { ids : int list option
  } [@@deriving yojson]

let default_config = { ids = None }

let name     = "Измерения"
let settings = None

let make ~(measures:measure_response React.event)
         ~(config:Board_types.config React.signal)
         (conf:config option) =
  let conf = Option.get_or ~default:default_config conf in
  let ids  = match conf.ids with Some x -> x | None -> List.map fst @@ React.S.value config in
  let open Widget_module_measures in
  List.map (fun x ->
      let config = Some { id = x } in
      let name   = Printf.sprintf "Модуль %d" @@ succ x in
      let w      = make ~measures config in
      `Text name, w) @@ List.sort compare ids
  |> Ui_templates.Tabs.create_simple_tabs
  |> Widget.coerce
