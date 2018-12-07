open Containers
open Components
open Lwt_result.Infix
open Common

let base_class = "pipeline-settings"

let make_streams (cpu : Topology.topo_cpu) () =
  Requests.HTTP.get_streams ()
  >>= (fun init ->
    let event, sock = Requests.WS.get_streams () in
    let box = Streams_selector.make ~init ~event cpu () in
    box#set_on_destroy (fun () ->
        React.E.stop ~strong:true event;
        sock##close);
    Lwt_result.return box#widget)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let make_structure () =
  Pipeline_js.Requests_structure.HTTP.get ()
  >>= (fun init ->
    let event, sock = Pipeline_js.Requests_structure.WS.get () in
    let w, s, set = Pipeline_js.Ui.Structure.make ~init ~event () in
    let apply = new Ui_templates.Buttons.Set.t s set () in
    let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
    let actions = new Card.Actions.t ~widgets:[buttons] () in
    let box = new Vbox.t ~widgets:[w; actions#widget] () in
    box#set_on_destroy (fun () ->
        React.E.stop ~strong:true event;
        sock##close);
    Lwt_result.return box#widget)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let make_settings () =
  Pipeline_js.Requests_settings.HTTP.get ()
  >>= (fun init ->
    let event, sock = Pipeline_js.Requests_settings.WS.get () in
    let w, s, set = Pipeline_js.Ui.Settings.make ~init ~event () in
    let apply = new Ui_templates.Buttons.Set.t s set () in
    let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
    let actions = new Card.Actions.t ~widgets:[buttons] () in
    let box = new Vbox.t ~widgets:[w; actions#widget] () in
    box#set_on_destroy (fun () ->
        React.E.stop ~strong:true event;
        sock##close);
    Lwt_result.return box#widget)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let make (cpu : Topology.topo_cpu) () : (#Widget.t,string) Lwt_result.t =
  let wrap f () = Ui_templates.Loader.create_widget_loader (f ()) in
  let tabs =
    [ new Tab.t
        ~content:(Text "Выбор потоков")
        ~value:(wrap (make_streams cpu)) ()
    ; new Tab.t
        ~content:(Text "Выбор PID")
        ~value:(wrap make_structure) ()
    (* ; new Tab.t
     *     ~content:(Text "Настройки анализа")
     *     ~value:(wrap make_settings) () *)
    ] in
  let bar, body = Ui_templates.Tabs.create_dynamic tabs in
  let box = Ui_templates.Tabs.wrap bar body in
  body#add_class @@ Markup.CSS.add_element base_class "body";
  box#add_class base_class;
  Lwt_result.return box
