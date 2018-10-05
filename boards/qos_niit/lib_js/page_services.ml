open Containers
open Components
open Common
open Lwt_result.Infix
open Board_types
open Page_common

let make (id : Stream.ID.t) control =
  let state = get_state id control in
  let thread = Widget_services_overview.make id control in
  let t_lwt =
    let open React in
    state
    >>= fun (state, state_close) -> wrap "Обзор" thread
    >|= (fun (w, box) ->
      let rate, rate_sock = Requests.Streams.WS.get_bitrate ~ids:[id] control in
      let pids, pids_sock = Requests.Streams.WS.get_pids ~ids:[id] control in
      let e, sock = Requests.Streams.WS.get_services ~ids:[id]  control in
      let e = E.map (function [(_, x)] -> w#update x | _ -> ()) e in
      let pids = E.map (function [(_, x)] -> w#update_pids x | _ -> ()) pids in
      let rate =
        E.map (function
            | [(_, (x : Bitrate.t timestamped))] ->
               w#set_rate @@ Option.return x.data
            | _ -> ()) rate in
      let state = S.map w#set_state state in
      let state = e, sock, pids, pids_sock, rate, rate_sock,
                  state, state_close in
      box, state) in
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
         >|= (fun (e, sock, pids, pids_sock, rate,
                   rate_sock, state, state_close) ->
             state_close ();
             React.S.stop ~strong:true state;
             React.E.stop ~strong:true e;
             React.E.stop ~strong:true rate;
             React.E.stop ~strong:true pids;
             sock##close;
             rate_sock##close;
             pids_sock##close)
         |> Lwt.ignore_result);
  box#widget
