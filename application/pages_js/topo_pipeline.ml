open Components
open Lwt_result.Infix
open Application_types

let base_class = "pipeline-settings"

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let make_streams (cpu : Topology.topo_cpu) socket =
  let open Application_http_js in
  get_streams ()
  >>= fun init -> Event.get_streams socket
  >>= fun (id, event) ->
  let box = Streams_selector.make ~init ~event cpu () in
  box#set_on_destroy (fun () ->
      React.E.stop ~strong:true event;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket id));
  Lwt_result.return box#widget

let make_structure socket =
  let open Pipeline_http_js.Http_structure in
  get_streams_with_source ()
  >>= fun actual -> get_streams_applied ()
  >>= fun applied -> Event.get_streams_with_source socket
  >>= fun (id_actual, e_actual) -> Event.get_streams_applied socket
  >>= fun (id_applied, e_applied) ->
  let w = Pipeline_widgets_js.Widget_structure.make ~actual ~applied () in
  let notif =
    React.E.merge (fun _ -> w#notify) ()
      [ React.E.map (fun x -> `Actual x) e_actual
      ; React.E.map (fun x -> `Applied x) e_applied ] in
  w#set_on_destroy (fun () ->
      React.E.stop ~strong:true notif;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket id_actual);
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket id_applied));
  Lwt.return_ok w#widget

let make (cpu : Topology.topo_cpu)
    (socket : Api_js.Websocket.JSON.t) : (#Widget.t,string) Lwt_result.t =
  let wrap f () = Ui_templates.Loader.create_widget_loader (f socket) in
  let tabs =
    [ (wrap (make_streams cpu)), Tab.make ~label:"Выбор потоков" ()
    ; (wrap (make_structure)), Tab.make ~label:"Выбор PID" ()
    ] in
  let bar, body = Ui_templates.Tabs.create_dynamic tabs in
  let box = Ui_templates.Tabs.wrap bar body in
  body#add_class @@ BEM.add_element base_class "body";
  box#add_class base_class;
  Lwt_result.return box
