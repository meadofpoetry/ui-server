open Containers
open Common
open Components
open Lwt_result.Infix
open Page_common
open Board_types

let ( % ) = Fun.( % )

let make_overview init e_errors stream control =
  let open React in
  let item = Widget_errors_log.make_dashboard_item [] stream control in
  let widget = item.widget in
  let thread =
    init
    >|= (fun (init : Error.raw)->
      List.iter (widget#prepend_error % snd) init;
      widget) in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  let item = { item with widget = loader } in
  let e_errors =
    E.map (List.iter (fun (_, x) ->
               List.iter (ignore % widget#prepend_error) x)) e_errors in
  let close = (fun () ->
      E.stop ~strong:true e_errors) in
  Dashboard.Item.make item, close

let make (id : Stream.ID.t) control =
  let init =
    Requests.History.HTTP.Errors.get ~limit:50 ~ids:[id] control
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _ -> Lwt.fail_with "got compressed")
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let e_errors, errors_sock =
    Requests.Streams.WS.get_errors ~ids:[id] control in
  let overview, overview_close =
    make_overview init e_errors id control in
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
         React.E.stop ~strong:true e_errors;
         errors_sock##close);
  box#widget
