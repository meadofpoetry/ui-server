open Containers
open Components
open Common
open Lwt_result.Infix
open Board_types
open Page_common

let make_summary init pids rate state id control =
  let thread = Widget_pids_summary.make ~init id control in
  let state_lwt =
    let open React in
    state
    >>= fun (state, _) -> thread
    >|= fun w ->
    let pids = E.map (function [(_, x)] -> w#update x | _ -> ()) pids in
    let rate =
      E.map (function
          | [(_, (x : Bitrate.t timestamped))] ->
             w#set_rate @@ Option.return x.data
          | _ -> ()) rate in
    let state = S.map w#set_state state in
    pids, rate, state in
  let close = fun () ->
    state_lwt
    >|= (fun (e1, e2, s) ->
      React.S.stop ~strong:true s;
      React.E.stop ~strong:true e1;
      React.E.stop ~strong:true e2)
    |> Lwt.ignore_result in
  wrap "Сводка" thread, close

let make_overview init pids rate state id control =
  let thread = Widget_pids_overview.make ~init id control in
  let state_lwt =
    let open React in
    state
    >>= fun (state, _) -> thread
    >|= fun w ->
    let pids = E.map (function [(_, x)] -> w#update x | _ -> ()) pids in
    let rate =
      E.map (function
          | [(_, (x : Bitrate.t timestamped))] ->
             w#set_rate @@ Option.return x.data
          | _ -> ()) rate in
    let state = S.map w#set_state state in
    pids, rate, state in
  let close = fun () ->
    state_lwt
    >|= (fun (e1, e2, s) ->
      React.S.stop ~strong:true s;
      React.E.stop ~strong:true e1;
      React.E.stop ~strong:true e2)
    |> Lwt.ignore_result in
  wrap "Обзор" thread, close

let make (id : Stream.ID.t) control =
  let init =
    Requests.Streams.HTTP.get_pids ~ids:[id] control
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let state = get_state id control in
  let rate, rate_sock = Requests.Streams.WS.get_bitrate ~ids:[id] control in
  let pids, pids_sock = Requests.Streams.WS.get_pids ~ids:[id] control in
  let summary, summary_close =
    make_summary init pids rate state id control in
  let overview, overview_close =
    make_overview init pids rate state id control in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let summary_cell  = new Cell.t ~span ~widgets:[summary] () in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells =
      [ summary_cell
      ; overview_cell ] in
    new t ~cells () in
  box#set_on_destroy
  @@ Some (fun () ->
         state >|= (fun (_, f) -> f ()) |> Lwt.ignore_result;
         summary_close ();
         overview_close ();
         React.E.stop ~strong:true rate;
         React.E.stop ~strong:true pids;
         rate_sock##close;
         pids_sock##close);
  box#widget
