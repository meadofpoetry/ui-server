open Containers
open Components
open Lwt_result.Infix

let make_streams () : (#Widget.widget,string) Lwt_result.t =
  Requests.get_stream_table ()
  >>= (fun init ->
    let event,sock = Requests.get_stream_table_socket () in
    let w,s,set    = Streams_selector.make ~init ~event () in
    let a          = Ui_templates.Buttons.create_apply s set in
    let abox       = new Card.Actions.Buttons.t ~widgets:[a] () in
    let box        = new Box.t ~vertical:true ~widgets:[w;abox#widget] () in
    let ()         = box#set_on_destroy @@ Some (fun () -> sock##close) in
    Lwt_result.return box#widget)

let make_structure () : (#Widget.widget,string) Lwt_result.t =
  Pipeline_js.Requests.get_structure ()
  >>= (fun init ->
    let event,sock = Pipeline_js.Requests.get_structure_socket () in
    let w,s,set    = Pipeline_js.Ui.Structure.make ~init ~event () in
    let a          = Ui_templates.Buttons.create_apply s set in
    let abox       = new Card.Actions.Buttons.t ~widgets:[a] () in
    let box        = new Box.t ~vertical:true ~widgets:[w;abox#widget] () in
    let ()         = box#set_on_destroy @@ Some (fun () -> sock##close) in
    Lwt_result.return box#widget)

let make_settings () : (#Widget.widget,string) Lwt_result.t =
  Pipeline_js.Requests.get_settings ()
  >>= (fun init ->
    let event,sock = Pipeline_js.Requests.get_settings_socket () in
    let w,s,set    = Pipeline_js.Ui.Settings.make ~init ~event () in
    let a          = Ui_templates.Buttons.create_apply s set in
    let abox       = new Card.Actions.Buttons.t ~widgets:[a] () in
    let box        = new Box.t ~vertical:true ~widgets:[w;abox#widget] () in
    let ()         = box#set_on_destroy @@ Some (fun () -> sock##close) in
    Lwt_result.return box#widget)

let make ?error_prefix () : (#Widget.widget,string) Lwt_result.t =
  let pgs  = Fun.(Ui_templates.Loader.create_widget_loader ?error_prefix %> Widget.coerce) in
  let sms  = make_streams () in
  let str  = make_structure () in
  let set  = make_settings () in
  let tabs = Ui_templates.Tabs.create_simple_tabs [ `Text "Выбор потоков", pgs sms
                                                  ; `Text "Выбор PID", pgs str
                                                  ; `Text "Настройки анализа", pgs set
                                                  ]
  in
  let fin () = sms >>= (fun w -> w#destroy; Lwt_result.return ()) |> Lwt.ignore_result;
               str >>= (fun w -> w#destroy; Lwt_result.return ()) |> Lwt.ignore_result;
               set >>= (fun w -> w#destroy; Lwt_result.return ()) |> Lwt.ignore_result
  in
  tabs#set_on_destroy @@ Some fin;
  print_endline "created tabs";
  Lwt_result.return tabs
