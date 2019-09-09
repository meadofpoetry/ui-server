open Application_types

let base_class = "pipeline-settings"

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let make_streams (cpu : Topology.topo_cpu) socket =
  let open Application_http_js in
  get_streams ()
  >>= fun init ->
  Event.get_streams socket
  >>= fun (id, event) ->
  let box = Streams_selector.make ~init ~event cpu () in
  box#set_on_destroy (fun () ->
      React.E.stop ~strong:true event;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket id));
  Lwt_result.return box#widget

let make_structure socket =
  let open Pipeline_http_js.Http_structure in
  get_annotated ()
  >>= fun structure ->
  Event.get_annotated socket
  >>= fun (id, e) ->
  let w = Pipeline_widgets.Structure.make structure () in
  let notif =
    React.E.merge (fun _ -> w#notify) () [React.E.map (fun x -> `Structure x) e]
  in
  w#set_on_destroy (fun () ->
      React.E.stop ~strong:true notif;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket id));
  Lwt.return_ok w#widget

let make (cpu : Topology.topo_cpu) (socket : Api_js.Websocket.JSON.t) :
    (#Components.Widget.t, string) Lwt_result.t =
  let wrap f () =
    Components.Widget.create @@ Components_lab.Loader.make_widget_loader (f socket)
  in
  let tabs =
    [ ( `Fun (wrap (make_streams cpu))
      , Components.Tab.Markup_js.create
          ~text_label:(`Text "Выбор потоков")
          () )
    ; ( `Fun (wrap make_structure)
      , Components.Tab.Markup_js.create ~text_label:(`Text "Выбор PID") () ) ]
  in
  let bar, body = Components.Tab_bar.make_bind ~tabs () in
  body#add_class @@ Components.BEM.add_element base_class "body";
  let box = Components.Widget.create_div () in
  box#append_child bar;
  box#append_child body;
  box#add_class base_class;
  box#set_on_destroy (fun () ->
      bar#destroy ();
      body#destroy ());
  Lwt_result.return box
