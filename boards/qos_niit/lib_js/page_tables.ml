open Containers
open Components
open Common
open Lwt_result.Infix
open Board_types
open Page_common

let l2 a b = a >>= fun a -> b >|= fun b -> a, b

let make_overview init e_tables e_rate state stream control =
  let open React in
  let item = Widget_tables_overview.make_dashboard_item None stream control in
  let widget = item.widget in
  let thread =
    l2 init state
    >|= (fun (init, state) ->
      widget#set_state @@ React.S.value state;
      Option.iter widget#update init;
      widget) in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  let item = { item with widget = loader } in
  let e_tables =
    E.map (function
        | [(_, x)] -> widget#update x
        | _ -> ()) e_tables in
  let e_rate =
    E.map (function
        | [(_, (x : Bitrate.t timestamped))] -> widget#set_rate x.data
        | _ -> ()) e_rate in
  let state = state >|= S.map widget#set_state in
  let close = (fun () ->
      E.stop ~strong:true e_tables;
      E.stop ~strong:true e_rate;
      state >|= (S.stop ~strong:true)
      |> Lwt.ignore_result) in
  Dashboard.Item.make item, close

let make (id : Stream.ID.t) control =
  let init =
    let open Lwt.Infix in
    Requests.Streams.HTTP.get_tables ~ids:[id] control
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    >|= (function
         | Ok [] -> Ok None
         | Ok [(_, x)] -> Ok (Some x)
         | Ok _ -> Error "Got tables for more than one stream"
         | Error e -> Error e) in
  let state = get_state id control in
  let state' = state >|= fst in
  let e_rate, rate_sock =
    Requests.Streams.WS.get_bitrate ~ids:[id] control in
  let e_tables, tables_sock =
    Requests.Streams.WS.get_tables ~ids:[id] control in
  let overview, overview_close =
    make_overview init e_tables e_rate state' id control in
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
         state >|= (fun (_, f) -> f ()) |> Lwt.ignore_result;
         React.E.stop ~strong:true e_tables;
         React.E.stop ~strong:true e_rate;
         tables_sock##close;
         rate_sock##close);
  box#widget
