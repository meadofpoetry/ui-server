open Containers
open Components
open Lwt_result.Infix

let make_streams () : Ui_templates.Types.settings_section_lwt =
  Requests.get_stream_table ()
  >>= (fun init ->
    let event,sock = Requests.get_stream_table_socket () in
    let w,s,set    = Streams_selector.make ~init ~event () in
    let a          = Ui_templates.Buttons.create_apply s set in
    Lwt_result.return (w, (fun () -> sock##close)))

let make_structure () : Ui_templates.Types.settings_section_lwt =
  Pipeline_js.Requests.get_structure ()
  >>= (fun init ->
    let event,sock = Pipeline_js.Requests.get_structure_socket () in
    let w,s,set    = Pipeline_js.Ui.Structure.make ~init ~event () in
    let a          = Ui_templates.Buttons.create_apply s set in
    Lwt_result.return (w, (fun () -> sock##close)))

let make_settings () : Ui_templates.Types.settings_section_lwt =
  Pipeline_js.Requests.get_settings ()
  >>= (fun init ->
    let event,sock = Pipeline_js.Requests.get_settings_socket () in
    let w,s,set    = Pipeline_js.Ui.Settings.make ~init ~event () in
    let a          = Ui_templates.Buttons.create_apply s set in
    Lwt_result.return (w, (fun () -> sock##close)))

let make ?error_prefix () : Ui_templates.Types.settings_section_lwt =
  let pgs  = Ui_templates.Progress.create_progress_block_lwt ?error_prefix ~get:(fun (w,_) -> w) in
  let sms  = make_streams () in
  let str  = make_structure () in
  let set  = make_settings () in
  let tabs = Ui_templates.Tabs.create_simple_tabs [ `Text "Выбор потоков", pgs sms
                                                  ; `Text "Выбор PID", pgs str
                                                  ; `Text "Настройки анализа", pgs set
                                                  ]
  in
  Lwt_result.return (tabs,(fun () ->
                       sms >>= (fun (_,c) -> c (); Lwt_result.return ()) |> Lwt.ignore_result;
                       str >>= (fun (_,c) -> c (); Lwt_result.return ()) |> Lwt.ignore_result;
                       set >>= (fun (_,c) -> c (); Lwt_result.return ()) |> Lwt.ignore_result))
