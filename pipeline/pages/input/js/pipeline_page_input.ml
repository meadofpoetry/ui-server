open Js_of_ocaml
open Netlib
open Components
open Pipeline_types
open Pipeline_http_js
open Pipeline_widgets.Parameter_chart

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

let make_config typ : widget_config =
  {duration = Time.Period.of_int_s 60; typ; sources = []; settings = None}

let () =
  let (_scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    let open React in
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Http_measurements.Event.get_video socket
    >>=? fun (_v_id, v_ev) ->
    Http_measurements.Event.get_audio socket
    >>=? fun (_a_id, a_ev) ->
    Http_structure.get_annotated ()
    >>=? fun structures ->
    let blk = make [] structures (make_config `Black) in
    let lum = make [] structures (make_config `Luma) in
    let frz = make [] structures (make_config `Freeze) in
    let dif = make [] structures (make_config `Diff) in
    let blo = make [] structures (make_config `Blocky) in
    let sht = make [] structures (make_config `Shortt) in
    let mom = make [] structures (make_config `Moment) in
    let charts = [blk; lum; frz; dif; blo; sht; mom] in
    let notif =
      E.merge
        (fun _ data -> List.iter (fun x -> x#notify data) charts)
        ()
        [ E.map
            (fun data ->
              let data =
                List.map
                  (fun ({stream; channel; pid; data; _} : Qoe_errors.Video_data.t) ->
                    let (source : data_source) = {stream; service = channel; pid} in
                    source, `Video data)
                  data
              in
              `Data data)
            v_ev
        ; E.map
            (fun data ->
              let data =
                List.map
                  (fun ({stream; channel; pid; data; _} : Qoe_errors.Audio_data.t) ->
                    let (source : data_source) = {stream; service = channel; pid} in
                    source, `Audio data)
                  data
              in
              `Data data)
            a_ev ]
    in
    List.iter
      (fun chart ->
        (Js.Unsafe.coerce chart#root##.style)##.boxSizing := Js.string "border-box";
        chart#root##.style##.height := Js.string "350px";
        chart#root##.style##.padding := Js.string "24px";
        chart#root##.style##.marginBottom := Js.string "20px";
        chart#add_class Card.CSS.root)
      charts;
    let box = Box.make ~vertical:true ~children:(List.map Widget.markup charts) () in
    box#root##.style##.margin := Js.string "1rem";
    box#root##.style##.padding := Js.string "24px";
    box#set_on_destroy (fun () ->
        List.iter Widget.destroy charts;
        E.stop ~strong:true notif;
        E.stop ~strong:true v_ev;
        E.stop ~strong:true a_ev;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok box
  in
  let _loader =
    Components_lab.Loader.make_widget_loader
      (* ~elt:(Dom_html.getElementById "pipeline") *)
      thread
  in
  ()
