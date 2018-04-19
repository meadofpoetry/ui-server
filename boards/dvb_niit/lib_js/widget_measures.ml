open Containers
open Components
open Board_types
open Lwt_result.Infix

type config = { ids : int list option }

let make ~(measures:measure_response React.event)
         ~(config:(Board_types.config React.signal,string) Lwt_result.t)
         (conf:config) =
  let open Widget_module_measures in
  let t = match conf.ids with
    | Some x -> Lwt_result.return x
    | None   -> config >>= (fun c -> Lwt_result.return @@ List.map fst @@ React.S.value c)
  in
  let t = t >>= fun ids -> List.map (fun x ->
                               let config = { id = x } in
                               let name   = Printf.sprintf "Модуль %d" @@ succ x in
                               let w      = make ~measures config in
                               `Text name, w#widget) @@ List.sort compare ids
                           |> Ui_templates.Tabs.create_simple_tabs
                           |> Widget.coerce
                           |> Lwt_result.return
  in
  let o = object
      val mutable _name = "Измерения"
      inherit Ui_templates.Loader.widget_loader t ()
      method! layout    = t >>= (fun w -> Lwt_result.return w#layout) |> Lwt.ignore_result
      method name       = _name
      method set_name x = _name <- x
      method settings : unit Widget_grid.Item.settings option = None
    end
  in (o :> unit Widget_grid.Item.t)
