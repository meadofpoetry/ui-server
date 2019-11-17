open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_widgets
open Board_niitv_tsan_http_js
open Components
include Board_niitv_tsan_page_pids_tyxml
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

let ( >>= ) = Lwt_result.bind

type state =
  { mutable socket : Api_js.Websocket.JSON.t option
  ; mutable finalize : unit -> unit
  }
(** Tab state. *)

module Selector = struct
  let stream_select = Printf.sprintf ".%s" Stream_select.CSS.root

  let pid_bitrate_pie_chart = Printf.sprintf ".%s" Pid_bitrate_pie_chart.CSS.root

  let pid_summary = Printf.sprintf ".%s" Pid_summary.CSS.root

  let pid_overview = Printf.sprintf ".%s" Pid_overview.CSS.root
end

type event = [ `Bitrate of (Stream.ID.t * Bitrate.ext) list ]

(** Make necessary HTTP and Websocket requests to the server. *)
let do_requests state control =
  Http_streams.get_streams control
  >>= fun streams_init ->
  Http_monitoring.get_pids control
  >>= fun pids_init ->
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
  let streams = React.S.hold streams_init streams_ev in
  let pids = React.S.hold pids_init pids_ev in
  let fin () =
    React.(
      S.stop ~strong:true streams;
      S.stop ~strong:true pids;
      E.stop ~strong:true state_ev;
      E.stop ~strong:true bitrate_ev;
      E.stop ~strong:true pids_ev;
      E.stop ~strong:true streams_ev)
  in
  Lwt.return_ok (streams, pids, state_ev, bitrate_ev, fin)

class t ~set_stream ~set_hex elt () =
  object
    val stream_select : Stream_select.t =
      Stream_select.attach ~on_change:(fun x ->
          let id =
            match x#value with
            | None -> None
            | Some (x : Stream.t) -> Some x.id
          in
          set_stream id)
      @@ Element.query_selector_exn elt Selector.stream_select

    val pid_bitrate_pie_chart : Pid_bitrate_pie_chart.t =
      Pid_bitrate_pie_chart.attach
      @@ Element.query_selector_exn elt Selector.pid_bitrate_pie_chart

    val pid_overview : Pid_overview.t =
      Pid_overview.attach ~set_hex @@ Element.query_selector_exn elt Selector.pid_overview

    inherit Widget.t elt () as super

    method! destroy () : unit =
      stream_select#destroy ();
      pid_bitrate_pie_chart#destroy ();
      pid_overview#destroy ();
      super#destroy ()

    method notify : event -> unit =
      function
      | `Bitrate ((_, x) :: _) -> pid_bitrate_pie_chart#notify (`Bitrate (Some x))
      | _ -> ()
  end

let attach ~set_stream ~set_hex elt : t =
  new t ~set_stream ~set_hex (elt :> Dom_html.element Js.t) ()

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
    >>= fun (streams, pids, state_ev, bitrate_ev, fin) ->
    let stream, set_stream =
      S.create
        (match S.value streams with
        | [] -> None
        | x :: _ -> Some x.id)
    in
    let s_data = S.l2 get_data pids stream in
    let signal = RList.from_signal s_data in
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
    let stream_select = Stream_select.R.create ~streams () in
    let bitrate_summary = Bitrate_summary.R.create ~bitrate () in
    let pid_summary = Pid_summary.R.create ~hex ~pids:signal () in
    let pid_overview = Pid_overview.R.create ~hex ~init:signal ~bitrate ~control () in
    let page =
      attach ~set_stream ~set_hex
      @@ Tyxml_js.To_dom.of_div
      @@ D.create ~stream_select ~pid_overview ~bitrate_summary ~pid_summary ()
    in
    let notif =
      E.merge (fun _ -> page#notify) () [ E.map (fun x -> `Bitrate x) bitrate_ev ]
    in
    Dom.appendChild elt page#root;
    state.finalize <-
      (fun () ->
        Dom.removeChild elt page#root;
        page#destroy ();
        S.stop ~strong:true stream;
        E.stop ~strong:true notif;
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
