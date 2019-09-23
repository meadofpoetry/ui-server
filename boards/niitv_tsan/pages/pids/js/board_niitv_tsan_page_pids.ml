open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_widgets
open Board_niitv_tsan_http_js
open Components
include Board_niitv_tsan_page_pids_tyxml
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>=? ) = Lwt_result.bind

module Selector = struct
  let pid_bitrate_pie_chart = "." ^ Pid_bitrate_pie_chart.CSS.root

  let bitrate_summary = "." ^ Bitrate_summary.CSS.root

  let pid_summary = "." ^ Pid_summary.CSS.root

  let pid_overview = "." ^ Pid_overview.CSS.root
end

type event =
  [ `Bitrate of (Stream.ID.t * Bitrate.ext) list
  | `PIDs of (Stream.ID.t * (int * PID.t) list ts) list
  | `State of Topology.state ]

class t elt () =
  object
    val pid_bitrate_pie_chart : Pid_bitrate_pie_chart.t =
      Pid_bitrate_pie_chart.attach
      @@ Element.query_selector_exn elt Selector.pid_bitrate_pie_chart

    val bitrate_summary : Bitrate_summary.t =
      Bitrate_summary.attach @@ Element.query_selector_exn elt Selector.bitrate_summary

    val pid_summary : Pid_summary.t =
      Pid_summary.attach @@ Element.query_selector_exn elt Selector.pid_summary

    val pid_overview : Pid_overview.t =
      Pid_overview.attach @@ Element.query_selector_exn elt Selector.pid_overview

    inherit Widget.t elt () as super

    method! init () : unit =
      let ( >>= ) = Lwt.bind in
      let n = 50 in
      let make_pid x =
        { PID.has_pts = true
        ; has_pcr = true
        ; scrambled = x mod 2 = 0
        ; present = true
        ; service_id = None
        ; service_name = Some "BBC"
        ; typ = SEC [0] }
      in
      let pids = List.init n (fun x -> x, make_pid x) in
      let s =
        React.S.hold []
        @@ Lwt_react.E.from (fun () ->
               Js_of_ocaml_lwt.Lwt_js.sleep 1.
               >>= fun () ->
               let pid = Random.int 10 in
               let hd = pid, make_pid pid in
               let pids =
                 match pids with
                 | x :: _ :: tl -> x :: hd :: tl
                 | x -> x
               in
               Lwt.return pids)
      in
      let bitrate =
        React.S.hold None
        @@ Lwt_react.E.from (fun () ->
               Js_of_ocaml_lwt.Lwt_js.sleep 1.
               >>= fun () ->
               Lwt.return
                 (Some
                    ({ total = {cur = 20_000_000; min = 0; max = 0}
                     ; effective = {cur = 0; min = 0; max = 0}
                     ; tables = []
                     ; timestamp = Ptime.epoch
                     ; pids =
                         List.init n (fun x ->
                             ( x
                             , { Bitrate.cur = Random.int 12_000_000
                               ; min = Random.int 10_000_000
                               ; max = Random.int 13_000_000 } )) }
                      : Bitrate.ext)))
      in
      let init = ReactiveData.RList.from_signal s in
      let pid_overview_r =
        Tyxml_js.To_dom.of_element @@ Pid_overview.R.create ~bitrate ~init ~control:0 ()
      in
      Dom.appendChild super#root pid_overview_r;
      super#init ()

    method! destroy () : unit =
      pid_bitrate_pie_chart#destroy ();
      bitrate_summary#destroy ();
      pid_summary#destroy ();
      pid_overview#destroy ();
      super#destroy ()

    method notify : event -> unit =
      function
      | `Bitrate ((_, x) :: _) ->
          bitrate_summary#notify (`Bitrate (Some x));
          pid_bitrate_pie_chart#notify (`Bitrate (Some x));
          pid_overview#notify (`Bitrate (Some x))
      | `PIDs ((_, x) :: _) ->
          pid_summary#notify (`PIDs x);
          pid_overview#notify (`PIDs x)
      | `State (x : Topology.state) ->
          pid_overview#notify (`State (x :> [Topology.state | `No_sync]))
      | _ -> ()
  end

let attach elt : t = new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?a ?children ~control () : t =
  D.create ?classes ?a ?children ~control () |> Tyxml_js.To_dom.of_div |> attach

type state =
  { mutable socket : Api_js.Websocket.JSON.t option
  ; mutable finalize : unit -> unit }

let on_visible (page : t) (state : state) control =
  let open React in
  let thread =
    Http_monitoring.get_pids control
    >>=? fun pids ->
    page#notify (`PIDs pids);
    Api_js.Websocket.JSON.open_socket ~path:(Netlib.Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter Api_js.Websocket.close_socket state.socket;
    state.socket <- Some socket;
    Http_device.Event.get_state socket control
    >>=? fun (_, state_ev) ->
    Http_monitoring.Event.get_bitrate_with_stats socket control
    >>=? fun (_, bitrate_ev) ->
    Http_monitoring.Event.get_pids socket control
    >>=? fun (_, pids_ev) ->
    let notif =
      E.merge
        (fun _ -> page#notify)
        ()
        [ E.map (fun x -> `Bitrate x) bitrate_ev
        ; E.map (fun x -> `PIDs x) pids_ev
        ; E.map (fun x -> `State x) state_ev ]
    in
    state.finalize <-
      (fun () ->
        E.stop ~strong:true bitrate_ev;
        E.stop ~strong:true notif);
    Lwt.return_ok state
  in
  let _loader = Components_lab.Loader.make_loader ~elt:page#root thread in
  ()

let on_hidden state =
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- None;
  state.finalize ()

let init control =
  let state = {socket = None; finalize = (fun () -> ())} in
  let result =
    Ui_templates.Tabbed_page.Tabpanel.init_map
      ~id:(id control)
      ~on_visible:(fun tabpanel -> on_visible tabpanel state control)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ~f:attach
      ()
  in
  match result with
  | Ok (page, finalize) -> page#set_on_destroy (fun () -> finalize ())
  | Error (`Msg _msg) -> ()
