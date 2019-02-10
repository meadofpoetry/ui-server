open Components
open Lwt_result.Infix
open Common

let base_class = "pipeline-settings"

let make_streams (cpu : Topology.topo_cpu) () =
  Application_js.Requests.HTTP.get_streams ()
  |> Lwt_result.map_err Api_js.Requests.err_to_string
  >>= fun init ->
  let event, sock = Application_js.Requests.WS.get_streams () in
  let box = Application_js.Streams_selector.make ~init ~event cpu () in
  box#set_on_destroy (fun () ->
      React.E.stop ~strong:true event;
      sock##close);
  Lwt_result.return box#widget

let make_structure () =
  Pipeline_js.Requests_structure.HTTP.get_streams_with_source ()
  >>= (fun init ->
    Pipeline_js.Requests_structure.HTTP.get_applied ()
    >>= fun init_applied ->
    let event, sock = Pipeline_js.Requests_structure.WS.get_streams_with_source () in
    let event_applied, sock_applied =  Pipeline_js.Requests_structure.WS.get_applied () in
    let w, s, set = Pipeline_js.Ui.Structure.make
                      ~init ~init_applied ~event ~event_applied () in
    let apply = new Ui_templates.Buttons.Set.t s set () in
    let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
    let actions = new Card.Actions.t ~widgets:[buttons] () in
    let box = new Vbox.t ~widgets:[w; actions#widget] () in
    box#set_on_destroy (fun () ->
        React.E.stop ~strong:true event;
        sock_applied##close;
        sock##close);
    Lwt_result.return box#widget)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let make_settings () =
  Pipeline_js.Requests_settings.HTTP.get ()
  |> Lwt_result.map_err Api_js.Requests.err_to_string
  >>= fun init ->
  let event, sock = Pipeline_js.Requests_settings.WS.get () in
  let w, s, set = Pipeline_js.Ui.Settings.make ~init ~event () in
  let apply = new Ui_templates.Buttons.Set.t s set () in
  let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  let box = new Vbox.t ~widgets:[w; actions#widget] () in
  box#set_on_destroy (fun () ->
      React.E.stop ~strong:true event;
      sock##close);
  Lwt_result.return box#widget

let make (cpu : Topology.topo_cpu) () : (#Widget.t,string) Lwt_result.t =
  let wrap f () = Ui_templates.Loader.create_widget_loader (f ()) in
  let tabs =
    [ new Tab.t
        ~content:(Text "Выбор потоков")
        ~value:(wrap (make_streams cpu)) ()
    ; new Tab.t
        ~content:(Text "Выбор PID")
        ~value:(wrap make_structure) ()
    ] in
  let bar, body = Ui_templates.Tabs.create_dynamic tabs in
  let box = Ui_templates.Tabs.wrap bar body in
  body#add_class @@ CSS.add_element base_class "body";
  box#add_class base_class;
  Lwt_result.return box
