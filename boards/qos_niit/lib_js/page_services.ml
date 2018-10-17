open Containers
open Components
open Common
open Lwt_result.Infix
open Board_types
open Page_common

let l3 a b c = a >>= fun a -> b >>= fun b -> c >|= fun c -> a, b, c

let make_overview init pids e_services e_pids e_rate state =
  let open React in
  let item = Widget_services_overview.make_dashboard_item None None in
  let widget = item.widget in
  let thread =
    l3 init pids state
    >|= (fun (init, pids, state) ->
      widget#set_state @@ React.S.value state;
      Option.iter widget#update_pids pids;
      Option.iter widget#update init;
      widget) in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  let item = { item with widget = loader } in
  let e_services =
    E.map (function
        | [(_, x)] -> widget#update x
        | _ -> ()) e_services in
  let e_pids =
    E.map (function
        | [(_, x)] -> widget#update_pids x
        | _ -> ()) e_pids in
  let e_rate =
    E.map (function
        | [(_, (x : 'a timestamped))] ->
           widget#set_rate @@ Option.return x.data
        | _ -> ()) e_rate in
  let state = state >|= S.map widget#set_state in
  let close = (fun () ->
      E.stop ~strong:true e_services;
      E.stop ~strong:true e_pids;
      E.stop ~strong:true e_rate;
      state >|= (S.stop ~strong:true)
      |> Lwt.ignore_result) in
  Dashboard.Item.make item, close


let make (id : Stream.ID.t) control =
  let init =
    let open Lwt.Infix in
    Requests.Streams.HTTP.get_services ~ids:[id] control
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    >|= (function
         | Ok [] -> Ok None
         | Ok [(_, x)] -> Ok (Some x)
         | Ok _ -> Error "Got services for more than one stream"
         | Error e -> Error e) in
  let pids =
    let open Lwt.Infix in
    Requests.Streams.HTTP.get_pids ~ids:[id] control
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    >|= (function
         | Ok [] -> Ok None
         | Ok [(_, x)] -> Ok (Some x)
         | Ok _ -> Error "Got pids for more than one stream"
         | Error e -> Error e) in
  let state = get_state id control in
  let state' = state >|= fst in
  let e_rate, rate_sock =
    Requests.Streams.WS.get_bitrate ~ids:[id] control in
  let e_pids, pids_sock =
    Requests.Streams.WS.get_pids ~ids:[id] control in
  let e_services, services_sock =
    Requests.Streams.WS.get_services ~ids:[id]  control in
  let overview, overview_close =
    make_overview init pids e_services e_pids e_rate state' in
  let box =
    let open Layout_grid in
    let open Typography in
    let span = 12 in
    let overview_cell = new Cell.t ~span ~widgets:[overview] () in
    let cells = [overview_cell] in
    new t ~cells () in
  box#set_on_destroy
  @@ Some (fun () ->
         state >|= (fun (_, f) -> f ()) |> Lwt.ignore_result;
         overview#destroy ();
         overview_close ();
         React.E.stop ~strong:true e_services;
         React.E.stop ~strong:true e_pids;
         React.E.stop ~strong:true e_rate;
         services_sock##close;
         rate_sock##close;
         pids_sock##close);
  box#widget
