open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Components
open Ui_templates
open Board_niitv_tsan_http_js
open Board_niitv_tsan_widgets
include Board_niitv_tsan_page_services_tyxml
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

let ( >>= ) = Lwt_result.bind

module Selector = struct
  let root = Printf.sprintf ".%s" CSS.root

  let stream_select = Printf.sprintf ".%s" Stream_select.CSS.root

  let service_overview = Printf.sprintf ".%s" Service_overview.CSS.root
end

type state = {
  mutable socket : Api_js.Websocket.JSON.t option;
  mutable finalize : unit -> unit;
}
(** Tab state. *)

(** Make necessary HTTP and Websocket requests to the server. *)
let do_requests ~input state control =
  Http_streams.get_streams ~inputs:[ input ] control >>= fun streams_init ->
  Http_monitoring.get_pids control >>= fun pids_init ->
  Http_monitoring.get_services control >>= fun services_init ->
  Api_js.Websocket.JSON.open_socket
    ~path:(Netlib.Uri.Path.Format.of_string "ws")
    ()
  >>= fun socket ->
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- Some socket;
  Http_streams.Event.get_streams ~inputs:[ input ] socket control
  >>= fun (_, streams_ev) ->
  Http_device.Event.get_state socket control >>= fun (_, state_ev) ->
  Http_monitoring.Event.get_bitrate_with_stats socket control
  >>= fun (_, bitrate_ev) ->
  Http_monitoring.Event.get_pids socket control >>= fun (_, pids_ev) ->
  Http_monitoring.Event.get_services socket control >>= fun (_, services_ev) ->
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

class t ~set_stream ~set_selected ~set_hex elt () =
  object
    val stream_select : Stream_select.t =
      Stream_select.attach ~on_change:(fun x ->
          let id =
            match x#value with None -> None | Some (x : Stream.t) -> Some x.id
          in
          set_stream id)
      @@ Element.query_selector_exn elt Selector.stream_select

    val service_overview : Service_overview.t =
      Service_overview.attach ~set_hex ~on_row_action:set_selected
      @@ Element.query_selector_exn elt Selector.service_overview

    inherit Widget.t elt () as super

    method! layout () : unit =
      stream_select#layout ();
      service_overview#layout ();
      super#layout ()

    method! destroy () : unit =
      stream_select#destroy ();
      service_overview#destroy ();
      super#destroy ()
  end

let attach ~set_stream ~set_selected ~set_hex elt : t =
  new t ~set_stream ~set_selected ~set_hex elt ()

(** Extract needed data from the association list by the provided stream. *)
let get_data v = function
  | None -> []
  | Some s -> (
      match List.assoc_opt s v with
      | None -> []
      | Some (x : _ Board_niitv_tsan_types.ts) -> x.data )

(** Called when this tab becomes active. *)
let on_visible ~input (elt : Dom_html.element Js.t) (state : state) control =
  let open React in
  let open ReactiveData in
  let thread =
    do_requests ~input state control
    >>= fun (streams, pids, services, state_ev, bitrate_ev, fin) ->
    let stream, set_stream =
      S.create (match S.value streams with [] -> None | x :: _ -> Some x.id)
    in
    let selected, set_selected = S.create None in
    let s_pids = S.l2 get_data pids stream in
    let s_data = S.l2 get_data services stream in
    let s_service =
      S.l2
        (fun id data ->
          match id with
          | None -> None
          | Some id -> Option.map (fun x -> (id, x)) (List.assoc_opt id data))
        selected s_data
    in
    let bitrate =
      S.hold None
      @@ S.sample
           (fun bitrate -> function None -> None
             | Some stream -> List.assoc_opt stream bitrate)
           bitrate_ev stream
    in
    let hex, set_hex = S.create false in
    let stream_select = Stream_select.R.create ~streams () in
    let service_overview =
      Service_overview.R.create ~pids:s_pids ~selected:s_service ~hex
        ~init:(RList.from_signal s_data) ~bitrate ~control ()
    in
    let page =
      attach ~set_stream ~set_selected ~set_hex
      @@ Tyxml_js.To_dom.of_div
      @@ D.create ~stream_select ~service_overview ()
    in
    Dom.appendChild elt page#root;
    page#layout ();
    state.finalize <-
      (fun () ->
        Dom.removeChild elt page#root;
        page#destroy ();
        S.stop ~strong:true stream;
        fin ());
    Lwt.return_ok state
  in
  let _loader = Components_lab.Loader.make_loader ~elt thread in
  state.finalize <- (fun () -> Lwt.cancel thread);
  ()

(** Called when this tab becomes inactive. *)
let on_hidden state =
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- None;
  state.finalize ()

(** Called on page initialization. *)
let init ~input control =
  let state = { socket = None; finalize = (fun () -> ()) } in
  let _result =
    Ui_templates.Tabbed_page.Tabpanel.init ~id:(id control)
      ~on_visible:(fun tabpanel -> on_visible ~input tabpanel state control)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ()
  in
  ()
