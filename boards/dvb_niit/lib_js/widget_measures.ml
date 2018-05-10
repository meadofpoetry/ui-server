open Containers
open Components
open Board_types
open Lwt_result.Infix

type config =
  { ids : int list option
  } [@@deriving yojson]

let default_config = { ids = None }

let make ~(measures:measure_response React.event)
         ~(config:(Board_types.config React.signal,string) Lwt_result.t)
         (conf:config option) : Dashboard.Item.item =
  let conf = Option.get_or ~default:default_config conf in
  let t = match conf.ids with
    | Some x -> Lwt_result.return x
    | None   -> config >>= (fun c -> Lwt_result.return @@ List.map fst @@ React.S.value c)
  in
  let open Widget_module_measures in
  let t = t >>= fun ids -> List.map (fun x ->
                               let config = Some { id = x } in
                               let name   = Printf.sprintf "Модуль %d" @@ succ x in
                               let w      = make ~measures config in
                               `Text name, w.widget) @@ List.sort compare ids
                           |> Ui_templates.Tabs.create_simple_tabs
                           |> Widget.coerce
                           |> Lwt_result.return
  in
  let widget = object
      inherit Ui_templates.Loader.widget_loader t ()
      method! layout () = t >>= (fun w -> Lwt_result.return @@ w#layout ()) |> Lwt.ignore_result
    end
  in
  Dashboard.Item.to_item ~name:"Измерения"
                         widget#widget
