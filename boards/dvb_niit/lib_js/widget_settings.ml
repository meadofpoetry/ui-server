open Containers
open Components
open Lwt_result.Infix

type config =
  { ids : int list option
  } [@@deriving yojson]

let default_config = { ids = None }

let make ~(state:(Common.Topology.state React.signal,string) Lwt_result.t)
         ~(config:(Board_types.config React.signal,string) Lwt_result.t)
         (conf:config option)
         control : Dashboard.Item.item =
  let conf = Option.get_or ~default:default_config conf in
  let t = match conf.ids with
    | Some l -> Lwt_result.return l
    | None   -> config >>= (fun c -> Lwt_result.return @@ List.map fst @@ React.S.value c)
  in
  let t = t >>= (fun ids -> List.map (fun id ->
                                `Text (Printf.sprintf "Модуль %d" (succ id)),
                                (Widget_module_settings.make ~state ~config (Some {id}) control).widget)
                            @@ List.sort compare ids
                            |> Ui_templates.Tabs.create_simple_tabs
                            |> Widget.coerce
                            |> Lwt_result.return)
  in
  let widget = object
      inherit Ui_templates.Loader.widget_loader t ()
      method! layout () = t >>= (fun w -> Lwt_result.return @@ w#layout ()) |> Lwt.ignore_result
    end
  in
  Dashboard.Item.to_item ~name:"Настройки" widget#widget
