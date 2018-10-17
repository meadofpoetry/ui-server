open Containers
open Components
open Lwt_result.Infix

let base_class = "pipeline-settings"

let make_streams () =
  Requests.HTTP.get_streams ()
  >>= (fun init ->
    let event, sock = Requests.WS.get_streams () in
    let w, s, set = Streams_selector.make ~init ~event () in
    let apply = new Ui_templates.Buttons.Set.t s set () in
    let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
    let actions = new Card.Actions.t ~widgets:[buttons] () in
    let box = new Vbox.t ~widgets:[w; actions#widget] () in
    box#set_on_destroy
    @@ Some (fun () ->
           React.E.stop ~strong:true event;
           sock##close);
    Lwt_result.return box#widget)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let make_structure () =
  Pipeline_js.Requests.get_structure ()
  >>= (fun init ->
    let event, sock = Pipeline_js.Requests.get_structure_socket () in
    let w, s, set = Pipeline_js.Ui.Structure.make ~init ~event () in
    let apply = new Ui_templates.Buttons.Set.t s set () in
    let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
    let actions = new Card.Actions.t ~widgets:[buttons] () in
    let box = new Vbox.t ~widgets:[w; actions#widget] () in
    box#set_on_destroy
    @@ Some (fun () ->
           React.E.stop ~strong:true event;
           sock##close);
    Lwt_result.return box#widget)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let make_settings () =
  Pipeline_js.Requests.get_settings ()
  >>= (fun init ->
    let event, sock = Pipeline_js.Requests.get_settings_socket () in
    let w, s, set = Pipeline_js.Ui.Settings.make ~init ~event () in
    let apply = new Ui_templates.Buttons.Set.t s set () in
    let buttons = new Card.Actions.Buttons.t ~widgets:[apply] () in
    let actions = new Card.Actions.t ~widgets:[buttons] () in
    let box = new Vbox.t ~widgets:[w; actions#widget] () in
    box#set_on_destroy
    @@ Some (fun () ->
           React.E.stop ~strong:true event;
           sock##close);
    Lwt_result.return box#widget)
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let make ?error_prefix () : (#Widget.t,string) Lwt_result.t =
  let wrap f () = Ui_templates.Loader.create_widget_loader (f ()) in
  let tabs =
    [ new Tab.t
        ~content:(Text "Выбор потоков")
        ~value:(wrap make_streams) ()
    ; new Tab.t
        ~content:(Text "Выбор PID")
        ~value:(wrap make_structure) ()
    ; new Tab.t
        ~content:(Text "Настройки анализа")
        ~value:(wrap make_settings) ()
    ] in
  let bar, body = Ui_templates.Tabs.create_dynamic tabs in
  let box = Ui_templates.Tabs.wrap bar body in
  body#add_class @@ Markup.CSS.add_element base_class "body";
  box#add_class base_class;
  Lwt_result.return box
