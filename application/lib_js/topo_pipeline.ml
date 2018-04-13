open Containers
open Components
open Lwt_result.Infix

let make_section () : Ui_templates.Types.settings_section_lwt =
  Requests.get_stream_table ()
  >>= (fun streams ->
    Pipeline_js.Requests.get_structure ()
    >>= (fun structure ->
         let e_streams,streams_sock = Requests.get_stream_table_socket () in
         let e_structs,structs_sock = Pipeline_js.Requests.get_structure_socket () in
         let sms_w,sms_s,sms_set    = Streams_selector.make ~init:streams ~event:e_streams () in
         let sms_a                  = Ui_templates.Buttons.create_apply sms_s sms_set in
         let str_w,str_s,str_set    = Pipeline_js.Ui.Structure.make ~init:structure ~event:e_structs () in
         let str_a                  = Ui_templates.Buttons.create_apply str_s str_set in
         let tabs = Ui_templates.Tabs.create_simple_tabs [ `Text "Выбор потоков", sms_w
                                                         ; `Text "Выбор PID", str_w
                                                         ]
         in
         Lwt_result.return (tabs,(fun () -> structs_sock##close; streams_sock##close))))
