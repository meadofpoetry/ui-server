open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_http_js
open Board_niitv_tsan_widgets
include Board_niitv_tsan_page_services_tyxml
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt_result.bind

module Selector = struct
  let root = Printf.sprintf ".%s" CSS.root

  let service_overview = "." ^ Service_overview.CSS.root
end

type state =
  { mutable socket : Api_js.Websocket.JSON.t option
  ; mutable finalize : unit -> unit
  }
(** Tab state. *)

(* let stream = Option.get (Uuidm.of_string "d6db41ba-ec76-5666-a7d9-3fe4a3f39efb")
 * 
 * let services =
 *   [ ( stream
 *     , ({ data =
 *            [ ( 100
 *              , { Board_niitv_tsan_types.Service.name = "Первый канал"
 *                ; provider_name = "RTRS"
 *                ; pmt_pid = 1000
 *                ; pcr_pid = 4096
 *                ; has_pmt = true
 *                ; has_sdt = true
 *                ; dscr = true
 *                ; dscr_list = true
 *                ; eit_schedule = true
 *                ; eit_pf = true
 *                ; free_ca_mode = true
 *                ; running_status = 1
 *                ; service_type = 27
 *                ; service_type_list = 27
 *                ; elements = [ 1001; 1002 ]
 *                } )
 *            ; ( 101
 *              , { Board_niitv_tsan_types.Service.name = "Россия 1"
 *                ; provider_name = "RTRS"
 *                ; pmt_pid = 1001
 *                ; pcr_pid = 4097
 *                ; has_pmt = true
 *                ; has_sdt = true
 *                ; dscr = true
 *                ; dscr_list = true
 *                ; eit_schedule = true
 *                ; eit_pf = true
 *                ; free_ca_mode = true
 *                ; running_status = 1
 *                ; service_type = 27
 *                ; service_type_list = 27
 *                ; elements = [ 1003; 1004 ]
 *                } )
 *            ]
 *        ; timestamp = Ptime_clock.now ()
 *        }
 *         : _ Board_niitv_tsan_types.ts) )
 *   ]
 * 
 * let bitrate_ev =
 *   Lwt_react.(
 *     E.from (fun () ->
 *         Lwt.(
 *           Js_of_ocaml_lwt.Lwt_js.sleep 1.0
 *           >>= fun () ->
 *           Lwt.return
 *             [ ( stream
 *               , Board_niitv_tsan_types.Bitrate.cur_to_ext
 *                   { Board_niitv_tsan_types.Bitrate.total = 1000000
 *                   ; effective = 9_000_000
 *                   ; tables = []
 *                   ; pids = [ 1001, Random.int 5_000_000; 1002, Random.int 2_000_000 ]
 *                   ; services = [ 100, Random.int 7_000_000 ]
 *                   ; timestamp = Ptime_clock.now ()
 *                   } )
 *             ]))) *)

(** Make necessary HTTP and Websocket requests to the server. *)
let do_requests state control =
  Http_streams.get_streams control
  >>= fun streams_init ->
  Http_monitoring.get_pids control
  >>= fun pids_init ->
  Http_monitoring.get_services control
  >>= fun services_init ->
  Api_js.Websocket.JSON.open_socket ~path:(Netlib.Uri.Path.Format.of_string "ws") ()
  >>= fun socket ->
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- Some socket;
  Http_streams.Event.get_streams socket control
  >>= fun (_, streams_ev) ->
  Http_device.Event.get_state socket control
  >>= fun (_, state_ev) ->
  Http_monitoring.Event.get_bitrate_with_stats socket control
  >>= fun (_, bitrate_ev) ->
  Http_monitoring.Event.get_pids socket control
  >>= fun (_, pids_ev) ->
  Http_monitoring.Event.get_services socket control
  >>= fun (_, services_ev) ->
  let streams = React.S.hold streams_init streams_ev in
  let pids = React.S.hold pids_init pids_ev in
  let services = React.S.hold services_init services_ev in
  let fin () =
    React.(
      S.stop ~strong:true streams;
      S.stop ~strong:true pids;
      S.stop ~strong:true services;
      E.stop ~strong:true state_ev;
      E.stop ~strong:true bitrate_ev;
      E.stop ~strong:true pids_ev;
      E.stop ~strong:true services_ev;
      E.stop ~strong:true streams_ev);
    Api_js.Websocket.JSON.close_socket socket
  in
  Lwt.return_ok (streams, pids, services, state_ev, bitrate_ev, fin)

(** Extract needed data from the association list by the provided stream. *)
let get_data v = function
  | None -> []
  | Some s -> (
      match List.assoc_opt s v with
      | None -> []
      | Some (x : _ Board_niitv_tsan_types.ts) -> x.data)

(** Called when this tab becomes active. *)
let on_visible (elt : Dom_html.element Js.t) (state : state) control =
  let open React in
  let open ReactiveData in
  let thread =
    do_requests state control
    >>= fun (streams, pids, services, state_ev, bitrate_ev, fin) ->
    let stream =
      S.map
        (function
          | [] -> None
          | (x : Application_types.Stream.t) :: _ -> Some x.id)
        streams
    in
    let selected, set_selected = S.create None in
    let s_pids = S.l2 get_data pids stream in
    let s_data = S.l2 get_data services stream in
    let s_service =
      S.l2
        (fun id data ->
          match id with
          | None -> None
          | Some id -> Option.map (fun x -> id, x) (List.assoc_opt id data))
        selected
        s_data
    in
    let bitrate =
      S.hold None
      @@ S.sample
           (fun bitrate -> function
             | None -> None
             | Some stream -> List.assoc_opt stream bitrate)
           bitrate_ev
           stream
    in
    let hex, set_hex = S.create false in
    let service_overview =
      Service_overview.attach ~set_hex ~on_row_action:set_selected
      @@ Tyxml_js.To_dom.of_element
      @@ Service_overview.R.create
           ~pids:s_pids
           ~selected:s_service
           ~hex
           ~init:(RList.from_signal s_data)
           ~bitrate
           ~control
           ()
    in
    let cell =
      Tyxml_js.To_dom.of_element
      @@ Layout_grid.D.layout_grid_cell ~span:12 ~children:[ service_overview#markup ] ()
    in
    Dom.appendChild elt cell;
    state.finalize <-
      (fun () ->
        Dom.removeChild elt cell;
        service_overview#destroy ();
        fin ());
    Lwt.return_ok state
  in
  let _loader = Components_lab.Loader.make_loader ~elt thread in
  ()

(** Called when this tab becomes inactive. *)
let on_hidden state =
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- None;
  state.finalize ()

(** Called on page initialization. *)
let init control =
  let state = { socket = None; finalize = (fun () -> ()) } in
  let _result =
    Ui_templates.Tabbed_page.Tabpanel.init
      ~id:(id control)
      ~on_visible:(fun tabpanel -> on_visible tabpanel state control)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ()
  in
  ()
