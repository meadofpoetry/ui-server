open Containers
open Components
open Lwt_result.Infix

let make ~(state:(Common.Topology.state React.signal,string) Lwt_result.t)
         ~(config:(Board_types.config React.signal,string) Lwt_result.t)
         control =
  let t = state >>= (fun s -> config >>= (fun c -> Lwt_result.return (s,c))) in
  let t = t >>= (fun (s,c) ->
      let init = React.S.value c in
      List.map (fun (id,_) ->
          `Text (Printf.sprintf "Модуль %d" (succ id)),
          (Widget_module_settings.make ~state ~config {id} control))
      @@ (List.sort (fun (id1,_) (id2,_) -> compare id1 id2) init)
      |> Ui_templates.Tabs.create_simple_tabs
      |> Lwt_result.return)
  in
  (Ui_templates.Loader.create_widget_loader t)#widget
