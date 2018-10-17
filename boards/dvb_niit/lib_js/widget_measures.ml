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

let base_class = "dvb-niit-measures"

let make ~(measures:(int * measures) React.event)
      ~(config:Board_types.config React.signal)
      (conf:config option) =
  let conf = Option.get_or ~default:default_config conf in
  let ids  = match conf.ids with
    | Some x -> x
    | None -> List.map fst @@ React.S.value config in
  let tabs =
    List.map (fun x ->
        let open Widget_module_measures in
        let config = Some { id = x } in
        let name   = Printf.sprintf "Модуль %d" @@ succ x in
        let w      = make ~measures config in
        new Tab.t ~content:(Text name) ~value:w ())
    @@ List.sort compare ids in
  let bar, body = Ui_templates.Tabs.create_simple tabs in
  body#add_class @@ Markup.CSS.add_element base_class "body";
  let w = Ui_templates.Tabs.wrap_simple (bar, body) in
  w#add_class base_class;
  w
