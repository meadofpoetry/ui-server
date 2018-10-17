open Containers
open Components
open Lwt_result.Infix

type config =
  { ids : int list option
  } [@@deriving yojson]

let default_config = { ids = None }

let name     = "Настройки"
let settings = None

let base_class = "dvb-niit-settings"

let make ~(state:  Common.Topology.state React.signal)
      ~(config: Board_types.config React.signal)
      (conf:    config option)
      control =
  let conf = Option.get_or ~default:default_config conf in
  let ids  = match conf.ids with Some x -> x | None -> List.map fst @@ React.S.value config in
  let tabs =
    List.map (fun id ->
        let open Widget_module_settings in
        let name  = Printf.sprintf "Модуль %d" (succ id) in
        let value = make ~state ~config (Some {id}) control in
        new Tab.t ~value ~content:(Text name) ())
    @@ List.sort compare ids in
  let bar, body = Ui_templates.Tabs.create_simple tabs in
  body#add_class @@ Markup.CSS.add_element base_class "body";
  let w = Ui_templates.Tabs.wrap_simple (bar, body) in
  w#add_class base_class;
  w
