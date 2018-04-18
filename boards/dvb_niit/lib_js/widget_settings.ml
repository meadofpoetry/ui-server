open Containers
open Components
open Lwt_result.Infix

let make ~(state:(Common.Topology.state React.signal,string) Lwt_result.t)
         ~(config:(Board_types.config React.signal,string) Lwt_result.t)
         control =
  let t = state >>= (fun s -> config >>= (fun c -> Lwt_result.return (s,c))) in
  let t = t >>= (fun (_,c) ->
      let init = React.S.value c in
      List.map (fun (id,_) ->
          `Text (Printf.sprintf "Модуль %d" (succ id)),
          (Widget_module_settings.make ~state ~config {id} control)#widget)
      @@ (List.sort (fun (id1,_) (id2,_) -> compare id1 id2) init)
      |> Ui_templates.Tabs.create_simple_tabs
      |> Widget.coerce
      |> Lwt_result.return)
  in
  let o = object
      val mutable _name = "Настройки"
      inherit Ui_templates.Loader.widget_loader t ()
      method name = _name
      method set_name x = _name <- x
      method settings : unit Widget_grid.Item.settings option = None
    end
  in (o :> unit Widget_grid.Item.t)
