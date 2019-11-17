open Js_of_ocaml
open Netlib
open Components
open Application_types
open Board_niitv_dvb4ch_types
open Board_niitv_dvb4ch_http_js
include Board_niitv_dvb4ch_page_input_tyxml

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

module Attr = struct
  let hidden = "hidden"
end

type state =
  { mutable socket : Api_js.Websocket.JSON.t option
  ; mutable finalize : unit -> unit
  }

let make_charts mode =
  let open Board_niitv_dvb4ch_widgets.Measurements_chart in
  let pwr = make ~init:[] ~mode (make_config `Power) in
  let mer = make ~init:[] ~mode (make_config `Mer) in
  let ber = make ~init:[] ~mode (make_config `Ber) in
  let frq = make ~init:[] ~mode (make_config `Freq) in
  let btr = make ~init:[] ~mode (make_config `Bitrate) in
  let charts = [ pwr; mer; ber; frq; btr ] in
  object
    inherit Widget.t Dom_html.(createDiv document) () as super

    method charts = charts

    method! init () : unit =
      List.iter super#append_child charts;
      super#init ()

    method! layout () : unit =
      pwr#layout ();
      mer#layout ();
      ber#layout ();
      frq#layout ();
      btr#layout ();
      super#layout ()

    method notify data = List.iter (fun x -> x#notify data) charts
  end

let on_visible (elt : Dom_html.element Js.t) charts (state : state) control =
  let open React in
  List.iter (fun x -> x#clear ()) charts#charts;
  let thread =
    Http_device.get_mode control
    >>=? fun mode ->
    charts#notify (`Mode mode);
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter Api_js.Websocket.close_socket state.socket;
    state.socket <- Some socket;
    Http_receivers.Event.get_measurements socket control
    >>=? fun (_, meas_ev) ->
    Http_device.Event.get_mode socket control
    >>=? fun (_, mode_ev) ->
    let _ev =
      Lwt_react.E.from (fun () ->
          Js_of_ocaml_lwt.Lwt_js.sleep 1.
          >>= fun () ->
          let data =
            { Measure.power = Some (Random.float @@ -50.)
            ; ber = Some (Random.float 0.00001)
            ; mer = Some (Random.float 40.)
            ; freq = Some (Random.int 5)
            ; bitrate = Some (Random.int 50000000)
            ; lock = true
            }
          in
          Lwt.return [ 0, [ { data; timestamp = Ptime_clock.now () } ] ])
    in
    let notif =
      E.merge
        (fun _ x -> charts#notify x)
        ()
        [ E.map (fun x -> `Data (List.map (fun (id, x) -> id, [ x ]) x)) meas_ev
        ; E.map (fun x -> `Mode x) mode_ev
          (* ; E.map (fun x -> `Data x) _ev *)
        ]
    in
    charts#layout ();
    state.finalize <-
      (fun () ->
        E.stop ~strong:true mode_ev;
        E.stop ~strong:true meas_ev;
        E.stop ~strong:true notif;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok state
  in
  let _loader = Components_lab.Loader.make_loader ~elt thread in
  ()

let on_hidden state =
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- None;
  state.finalize ()

let init control =
  let state = { socket = None; finalize = (fun () -> ()) } in
  let charts = make_charts [] in
  let result =
    Ui_templates.Tabbed_page.Tabpanel.init
      ~id:(id control)
      ~on_visible:(fun tabpanel -> on_visible tabpanel charts state control)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ()
  in
  match result with
  | Error _ -> ()
  | Ok (elt, _) -> Dom.appendChild elt charts#root

let () =
  let boards =
    Topology.boards_of_yojson
    @@ Yojson.Safe.from_string
    @@ Js.to_string Js.Unsafe.global##.boards
  in
  match boards with
  | Error _ -> ()
  | Ok boards -> (
      match List.find_opt (Topology.equal_board_id board_id % fst) boards with
      | None -> ()
      | Some (_, controls) -> List.iter init controls)
