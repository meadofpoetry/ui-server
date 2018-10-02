open Containers
open Components
open Common
open Lwt_result.Infix
open Board_types
open Page_common

let make (id : Stream.ID.t) control =
  let open React in
  let thread = Widget_tables_overview.make id control in
  let sock_lwt =
    thread
    >|= fun w ->
    let rate, rate_sock = Requests.Streams.WS.get_bitrate ~ids:[id] control in
    let e, sock = Requests.Streams.WS.get_tables ~ids:[id] control in
    let e = E.map (function [(_, x)] -> w#update x | _ -> ()) e in
    let rate =
      E.map (function
          | [(_, (x : Bitrate.t timestamped))] ->
             w#set_rate x.data
          | _ -> ()) rate in
    e, sock, rate, rate_sock in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview = wrap "Обзор" thread in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells = [overview_cell] in
    new t ~cells () in
  box#set_on_destroy
  @@ Some (fun () ->
         sock_lwt
         >|= (fun (e, sock, rate, rate_sock) ->
             React.E.stop ~strong:true e;
             React.E.stop ~strong:true rate;
             sock##close;
             rate_sock##close)
         |> Lwt.ignore_result);
  box#widget
