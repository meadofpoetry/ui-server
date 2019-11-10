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

let ( >>=? ) = Lwt_result.bind

module Selector = struct
  let pid_bitrate_pie_chart = Printf.sprintf ".%s" Pid_bitrate_pie_chart.CSS.root

  let pid_summary = Printf.sprintf ".%s" Pid_summary.CSS.root

  let pid_overview = Printf.sprintf ".%s" Pid_overview.CSS.root
end

type event =
  [ `Bitrate of (Stream.ID.t * Bitrate.ext) list
  | `PIDs of (Stream.ID.t * (int * PID.t) list ts) list
  ]

class t ~set_hex elt () =
  object
    val pid_bitrate_pie_chart : Pid_bitrate_pie_chart.t =
      Pid_bitrate_pie_chart.attach
      @@ Element.query_selector_exn elt Selector.pid_bitrate_pie_chart

    val pid_overview : Pid_overview.t =
      Pid_overview.attach ~set_hex @@ Element.query_selector_exn elt Selector.pid_overview

    inherit Widget.t elt () as super

    method! init () : unit = super#init ()

    method! destroy () : unit =
      pid_bitrate_pie_chart#destroy ();
      pid_overview#destroy ();
      super#destroy ()

    method notify : event -> unit =
      function
      | `Bitrate ((_, x) :: _) -> pid_bitrate_pie_chart#notify (`Bitrate (Some x))
      | _ -> ()
  end

let attach ~set_hex elt : t = new t ~set_hex (elt :> Dom_html.element Js.t) ()

type state = {
  mutable socket : Api_js.Websocket.JSON.t option;
  mutable finalize : unit -> unit;
}

let on_visible (elt : Dom_html.element Js.t) (state : state) control =
  let open React in
  let stream =
    S.const (Option.get (Uuidm.of_string "d6db41ba-ec76-5666-a7d9-3fe4a3f39efb"))
  in
  let thread =
    Http_streams.get_streams control >>=? fun streams ->
    Http_monitoring.get_pids control >>=? fun pids ->
    Api_js.Websocket.JSON.open_socket ~path:(Netlib.Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter Api_js.Websocket.close_socket state.socket;
    state.socket <- Some socket;
    Http_device.Event.get_state socket control >>=? fun (_, _state_ev) ->
    Http_monitoring.Event.get_bitrate_with_stats socket control
    >>=? fun (_, bitrate_ev) ->
    Http_monitoring.Event.get_pids socket control >>=? fun (_, pids_ev) ->
    Http_streams.Event.get_streams socket control >>=? fun (_, streams_ev) ->
    let streams_signal = S.hold streams streams_ev in
    let _ =
      S.map
        (List.iter (print_endline % Yojson.Safe.to_string % Stream.to_yojson))
        streams_signal
    in
    let init =
      match List.assoc_opt (S.value stream) pids with
      | None -> []
      | Some (x : _ ts) -> x.data
    in
    let signal =
      ReactiveData.RList.from_signal
      @@ S.hold init
      @@ S.sample
           (fun pids stream ->
             match List.assoc_opt stream pids with
             | None -> []
             | Some (x : _ ts) -> x.data)
           pids_ev
           stream
    in
    let bitrate =
      S.hold None
      @@ S.sample (fun bitrate stream -> List.assoc_opt stream bitrate) bitrate_ev stream
    in
    let hex, set_hex = S.create false in
    let bitrate_summary = Bitrate_summary.R.create ~bitrate () in
    let pid_summary = Pid_summary.R.create ~hex ~pids:signal () in
    let pid_overview = Pid_overview.R.create ~hex ~init:signal ~bitrate ~control () in
    let page =
      attach ~set_hex
      @@ Tyxml_js.To_dom.of_div
      @@ D.create ~pid_overview ~bitrate_summary ~pid_summary ()
    in
    Dom.appendChild elt page#root;
    let notif =
      E.merge
        (fun _ -> page#notify)
        ()
        [ E.map (fun x -> `Bitrate x) bitrate_ev; E.map (fun x -> `PIDs x) pids_ev ]
    in
    state.finalize <-
      (fun () ->
        Dom.removeChild elt page#root;
        page#destroy ();
        E.stop ~strong:true bitrate_ev;
        E.stop ~strong:true notif);
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
  let _result =
    Ui_templates.Tabbed_page.Tabpanel.init
      ~id:(id control)
      ~on_visible:(fun tabpanel -> on_visible tabpanel state control)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ()
  in
  ()
