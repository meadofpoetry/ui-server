open Containers
open Common
open Components
open Lwt_result.Infix

let ( % ) = Fun.( % )

let make_overview ?inputs ?streams ~init e_log =
  let open React in
  let item = Widget_log.make_dashboard_item ~init:[] ?streams ?inputs () in
  let widget = item.widget in
  let thread =
    init
    >|= (fun (init : Stream.Log_message.t list)->
      List.iter widget#prepend_item init;
      widget) in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  let item = { item with widget = loader } in
  let e_log = E.map (List.iter (ignore % widget#prepend_item)) e_log in
  let close = (fun () -> E.stop ~strong:true e_log) in
  Dashboard.Item.make item, close

let make ?inputs ?streams () =
  let init =
    Requests.HTTP.get_log ~limit:200 ?inputs ?streams ()
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _ -> Lwt.fail_with "got compressed")
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let e_log, log_sock = Requests.WS.get_log ?inputs ?streams () in
  let overview, overview_close = make_overview ?inputs ?streams ~init e_log in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells = [overview_cell] in
    new t ~cells () in
  box#set_on_destroy
  @@ Some (fun () ->
         overview#destroy ();
         overview_close ();
         React.E.stop ~strong:true e_log;
         log_sock##close);
  box#widget
