open Containers
open Common
open Components
open Lwt_result.Infix

let ( % ) = Fun.( % )

let make_overview ?boards ?cpu ?inputs ?streams e_log =
  let open React in
  let item =
    Widget_log.make_dashboard_item ?boards ?cpu ?streams ?inputs ()
    |> fun x -> { x with name = "Журнал событий" } in
  let widget = item.widget in
  let e_log = E.map (List.iter widget#prepend_item) e_log in
  let close = (fun () -> E.stop ~strong:true e_log) in
  Dashboard.Item.make item, close

let make ?boards ?cpu ?inputs ?streams () =
  let e_log, log_sock = Requests.WS.get_log ?inputs ?streams () in
  let overview, overview_close =
    make_overview ?boards ?cpu ?inputs ?streams e_log in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells = [overview_cell] in
    new t ~cells () in
  box#set_on_destroy (fun () ->
      overview#destroy ();
      overview_close ();
      React.E.stop ~strong:true e_log;
      log_sock##close);
  box#widget
