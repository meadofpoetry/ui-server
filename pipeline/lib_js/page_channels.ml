open Containers
open Components
open Common

let make_chart (e : Qoe_errors.Video_data.t React.event)
      (typ : Qoe_errors.labels)
    : Widget_parameter_chart.t =
  let open Widget_parameter_chart in
  let config =
    { duration = Time.Span.of_int_s 2
    ; sources = []
    ; settings = None
    ; typ
    } in
  let chart = new t ~init:[] ~config () in
  React.E.map (fun (d : Qoe_errors.Video_data.t) ->
      let data = convert_video_data config d in
      chart#append_data data) e
  |> React.E.keep;
  chart

let make (id : Stream.ID.t) control =
  let e, sock = Requests_measurements.WS.get_video () in
  let black_chart = make_chart e `Black in
  let freeze_chart = make_chart e `Freeze in
  let blocky_chart = make_chart e `Blocky in
  let box =
    new Vbox.t ~widgets:[ black_chart
                        ; freeze_chart
                        ; blocky_chart ] () in
  box#style##.height := Js.string "100vh";
  box#set_on_destroy @@ Some (fun () -> sock##close);
  box#widget
