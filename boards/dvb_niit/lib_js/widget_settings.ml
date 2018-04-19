open Containers
open Components
open Lwt_result.Infix

type config = { ids : int list option }

let make ~(state:(Common.Topology.state React.signal,string) Lwt_result.t)
         ~(config:(Board_types.config React.signal,string) Lwt_result.t)
         (conf:config)
         control =
  let t = match conf.ids with
    | Some l -> Lwt_result.return l
    | None   -> config >>= (fun c -> Lwt_result.return @@ List.map fst @@ React.S.value c)
  in
  let t = t >>= (fun ids -> List.map (fun id ->
                                `Text (Printf.sprintf "Модуль %d" (succ id)),
                                (Widget_module_settings.make ~state ~config {id} control)#widget)
                            @@ List.sort compare ids
                            |> Ui_templates.Tabs.create_simple_tabs
                            |> Widget.coerce
                            |> Lwt_result.return)
  in
  let o = object
      val mutable _name = "Настройки"
      inherit Ui_templates.Loader.widget_loader t ()
      method! layout    = t >>= (fun w -> Lwt_result.return w#layout) |> Lwt.ignore_result
      method name       = _name
      method set_name x = _name <- x
      method settings : unit Widget_grid.Item.settings option = None
    end
  in (o :> unit Widget_grid.Item.t)
