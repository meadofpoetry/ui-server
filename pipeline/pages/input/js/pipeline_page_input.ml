open Js_of_ocaml
open Netlib
open Components
open Pipeline_types
open Pipeline_http_js
open Pipeline_widgets.Parameter_chart
include Pipeline_page_input_tyxml

type state = {
  mutable socket : Api_js.Websocket.JSON.t option;
  mutable finalize : unit -> unit;
}

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

let make_config typ : widget_config =
  { duration = Time.Period.of_int_s 60; typ; sources = []; settings = None }

let make_charts structures =
  let blk = make [] structures (make_config `Black) in
  let lum = make [] structures (make_config `Luma) in
  let frz = make [] structures (make_config `Freeze) in
  let dif = make [] structures (make_config `Diff) in
  let blo = make [] structures (make_config `Blocky) in
  let sht = make [] structures (make_config `Shortt) in
  let mom = make [] structures (make_config `Moment) in
  let charts = [ blk; lum; frz; dif; blo; sht; mom ] in
  object
    inherit Widget.t Dom_html.(createDiv document) () as super

    method charts = charts

    method! init () : unit =
      List.iter super#append_child charts;
      super#init ()

    method notify data = List.iter (fun x -> x#notify data) charts
  end

let on_visible (elt : Dom_html.element Js.t) charts (state : state) =
  let open React in
  List.iter (fun x -> x#clear ()) charts#charts;
  let thread =
    let open React in
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Http_measurements.Event.get_video socket >>=? fun (_v_id, v_ev) ->
    Http_measurements.Event.get_audio socket >>=? fun (_a_id, a_ev) ->
    Http_structure.get_annotated () >>=? fun structures ->
    let notif =
      E.merge
        (fun _ data -> List.iter (fun x -> x#notify data) charts#charts)
        ()
        [
          E.map
            (fun data ->
              let data =
                List.map
                  (fun ({ stream; channel; pid; data; _ } : Qoe_errors.Video_data.t) ->
                    let (source : data_source) = { stream; service = channel; pid } in
                    source, `Video data)
                  data
              in
              `Data data)
            v_ev;
          E.map
            (fun data ->
              let data =
                List.map
                  (fun ({ stream; channel; pid; data; _ } : Qoe_errors.Audio_data.t) ->
                    let (source : data_source) = { stream; service = channel; pid } in
                    source, `Audio data)
                  data
              in
              `Data data)
            a_ev;
        ]
    in
    state.finalize <-
      (fun () ->
        List.iter Widget.destroy charts#charts;
        E.stop ~strong:true notif;
        E.stop ~strong:true v_ev;
        E.stop ~strong:true a_ev;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok state
  in
  let _loader = Components_lab.Loader.make_loader ~elt thread in
  ()

let on_hidden state =
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- None;
  state.finalize ()

let init () =
  let state = { socket = None; finalize = (fun () -> ()) } in
  let charts = make_charts [] in
  let result =
    Ui_templates.Tabbed_page.Tabpanel.init
      ~id
      ~on_visible:(fun tabpanel -> on_visible tabpanel charts state)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ()
  in
  match result with
  | Error _ -> ()
  | Ok (elt, _) -> Dom.appendChild elt charts#root

let () = init ()
