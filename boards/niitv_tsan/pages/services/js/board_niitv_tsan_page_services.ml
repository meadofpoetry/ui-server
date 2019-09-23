open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_http_js
open Board_niitv_tsan_widgets
include Board_niitv_tsan_page_services_tyxml
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>=? ) = Lwt_result.bind

module Selector = struct
  let root = Printf.sprintf ".%s" CSS.root

  let service_overview = "." ^ Service_overview.CSS.root
end

type state =
  { mutable socket : Api_js.Websocket.JSON.t option
  ; mutable finalize : unit -> unit }

let get_data v s =
  match List.assoc_opt s v with
  | None -> []
  | Some (x : _ Board_niitv_tsan_types.ts) -> x.data

let on_visible (elt : Dom_html.element Js.t) (state : state) control =
  let open React in
  let page = Element.query_selector_exn elt Selector.root in
  let stream =
    React.S.const (Option.get (Uuidm.of_string "d6db41ba-ec76-5666-a7d9-3fe4a3f39efb"))
  in
  let thread =
    Http_monitoring.get_pids control
    >>=? fun _pids ->
    Http_monitoring.get_services control
    >>=? fun services ->
    Api_js.Websocket.JSON.open_socket ~path:(Netlib.Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter Api_js.Websocket.close_socket state.socket;
    state.socket <- Some socket;
    Http_device.Event.get_state socket control
    >>=? fun (_, _state_ev) ->
    Http_monitoring.Event.get_bitrate_with_stats socket control
    >>=? fun (_, bitrate_ev) ->
    Http_monitoring.Event.get_pids socket control
    >>=? fun (_, _pids_ev) ->
    Http_monitoring.Event.get_services socket control
    >>=? fun (_, services_ev) ->
    let signal =
      ReactiveData.RList.from_signal
      @@ S.hold (get_data services (React.S.value stream))
      @@ S.sample get_data services_ev stream
    in
    let bitrate =
      S.hold None
      @@ S.sample (fun bitrate stream -> List.assoc_opt stream bitrate) bitrate_ev stream
    in
    let overview, set_hex =
      Service_overview.R.create ~init:signal ~bitrate ~control ()
    in
    let service_overview =
      Service_overview.attach ~set_hex @@ Tyxml_js.To_dom.of_element overview
    in
    let cell =
      Tyxml_js.To_dom.of_element
      @@ Layout_grid.D.layout_grid_cell ~span:12 ~children:[service_overview#markup] ()
    in
    Dom.appendChild page cell;
    state.finalize <- (fun () -> Dom.removeChild page cell);
    Lwt.return_ok state
  in
  let _loader = Components_lab.Loader.make_loader ~elt thread in
  ()

let on_hidden state =
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- None;
  state.finalize ()

let init control =
  let state = {socket = None; finalize = (fun () -> ())} in
  let _result =
    Ui_templates.Tabbed_page.Tabpanel.init_map
      ~id:(id control)
      ~on_visible:(fun tabpanel -> on_visible tabpanel state control)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ~f:(fun x -> x)
      ()
  in
  ()
