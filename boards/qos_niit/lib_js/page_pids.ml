open Containers
open Components
open Common
open Lwt_result.Infix
open Board_types
open Page_common

let make_summary init pids rate state =
  let open React in
  let item = Widget_pids_summary.make_dashboard_item None in
  let widget = item.widget in
  let thread =
    init
    >>= fun init -> state
    >|= (fun state ->
      widget#set_state @@ React.S.value state;
      Option.iter widget#update init;
      widget) in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  let item = { item with widget = loader } in
  let pids =
    E.map (function
        | [(_, x)] -> widget#update x
        | _ -> ()) pids in
  let rate =
    E.map (function
        | [(_, (x : Bitrate.t timestamped))] ->
           widget#set_rate @@ Option.return x.data
        | _ -> ()) rate in
  let state = state >|= (fun s -> S.map ~eq:Equal.unit widget#set_state s) in
  let close = (fun () ->
      React.E.stop ~strong:true pids;
      React.E.stop ~strong:true rate;
      state >|= (React.S.stop ~strong:true)
      |> Lwt.ignore_result;) in
  Dashboard.Item.make item, close

let make_overview init pids rate state =
  let open React in
  let item = Widget_pids_overview.make_dashboard_item None in
  let widget = item.widget in
  let thread =
    init
    >>= fun init -> state
    >|= (fun state ->
      widget#set_state @@ React.S.value state;
      Option.iter widget#update init;
      widget) in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  let item = { item with widget = loader } in
  let pids =
    E.map (function
        | [(_, x)] -> widget#update x
        | _ -> ()) pids in
  let rate =
    E.map (function
        | [(_, (x : Bitrate.t timestamped))] ->
           widget#set_rate @@ Option.return x.data
        | _ -> ()) rate in
  let state = state >|= (fun s -> S.map ~eq:Equal.unit widget#set_state s) in
  let close = (fun () ->
      React.E.stop ~strong:true pids;
      React.E.stop ~strong:true rate;
      state >|= (React.S.stop ~strong:true)
      |> Lwt.ignore_result) in
  Dashboard.Item.make item, close

let make (id : Stream.ID.t) control =
  let init =
    let open Lwt.Infix in
    Requests.Streams.HTTP.get_pids ~ids:[id] control
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    >|= (function
         | Ok [] -> Ok None
         | Ok [(_, x)] -> Ok (Some x)
         | Ok _ -> Error "Got pids for more than one stream"
         | Error e -> Error e)in
  let state = get_state id control in
  let state' = state >|= fst in
  let rate, rate_sock = Requests.Streams.WS.get_bitrate ~ids:[id] control in
  let pids, pids_sock = Requests.Streams.WS.get_pids ~ids:[id] control in
  let summary, summary_close = make_summary init pids rate state' in
  let overview, overview_close = make_overview init pids rate state' in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let summary_cell = new Cell.t ~span ~widgets:[summary] () in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells =
      [ summary_cell
      ; overview_cell ] in
    new t ~cells () in
  box#set_on_destroy (fun () ->
      state >|= (fun (_, f) -> f ()) |> Lwt.ignore_result;
      summary#destroy ();
      overview#destroy ();
      summary_close ();
      overview_close ();
      React.E.stop ~strong:true rate;
      React.E.stop ~strong:true pids;
      rate_sock##close;
      pids_sock##close);
  box#widget
