open Containers
open Common
open Components
open Lwt_result.Infix
open Page_common

let ( % ) = Fun.( % )

let make (id : Stream.ID.t) control =
  let thread = Widget_errors_log.make id control in
  let t_lwt =
    wrap "Обзор" thread
    >|= (fun (w, box) ->
      let e, sock = Requests.Streams.WS.get_errors ~ids:[id] control in
      let e = React.E.map (
                  List.iter (fun (_, x) ->
                      List.iter (ignore % w#prepend_error) x)) e in
      (box, (e, sock))) in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview = Ui_templates.Loader.create_widget_loader (t_lwt >|= fst) in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells = [overview_cell] in
    new t ~cells () in
  box#set_on_destroy
  @@ Some (fun () ->
         t_lwt
         >|= snd
         >|= (fun (e, sock) ->
             React.E.stop ~strong:true e;
             sock##close)
         |> Lwt.ignore_result);
  box#widget
