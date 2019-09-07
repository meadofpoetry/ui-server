open Containers
open Components

let make_overview ?boards ?cpu ?inputs ?streams e_log =
  let open React in
  let item =
    Widget_log.make_dashboard_item ?boards ?cpu ?streams ?inputs ()
    |> fun x -> {x with name = "Журнал событий"}
  in
  let widget = item.widget in
  let e_log = E.map (List.iter widget#prepend_item) e_log in
  let close () = E.stop ~strong:true e_log in
  Dashboard.Item.make item, close

let make ?boards ?cpu ?inputs ?streams () =
  let ( >>= ) = Lwt_result.( >>= ) in
  let e_log, set_log = React.E.create () in
  Requests.Event.get_log
    ?inputs
    ?streams
    ~f:(fun _ -> function
      | Ok x -> set_log x
      | _ -> ())
    ()
  >>= fun socket ->
  let overview, overview_close = make_overview ?boards ?cpu ?inputs ?streams e_log in
  let box =
    let open Layout_grid in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells = [overview_cell] in
    new t ~cells ()
  in
  box#set_on_destroy (fun () ->
      overview#destroy ();
      overview_close ();
      React.E.stop ~strong:true e_log;
      socket##close);
  Lwt.return_ok box#widget
