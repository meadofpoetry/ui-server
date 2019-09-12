open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_http_js
open Components
open Netlib

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Attr = struct
  let hidden = "hidden"
end

type state = < finalize : unit -> unit >

type event =
  [ `Bitrate of (Stream.ID.t * Bitrate.t) list
  | `PIDs of (Stream.ID.t * (int * PID.t) list ts) list
  | `Services of (Stream.ID.t * (int * Service.t) list ts) list
  | `State of Topology.state ]

class type page =
  object
    method notify : event -> unit
  end

let on_visible (page : #page) state_ref control elt =
  let open React in
  let thread =
    Http_monitoring.get_pids control
    >>=? fun pids ->
    page#notify (`PIDs pids);
    Http_monitoring.get_services control
    >>=? fun services ->
    page#notify (`Services services);
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter (fun (state : state) -> state#finalize ()) !state_ref;
    Http_device.Event.get_state socket control
    >>=? fun (_, state_ev) ->
    Http_monitoring.Event.get_bitrate socket control
    >>=? fun (_, bitrate_ev) ->
    Http_monitoring.Event.get_pids socket control
    >>=? fun (_, pids_ev) ->
    Http_monitoring.Event.get_services socket control
    >>=? fun (_, services_ev) ->
    let notif =
      E.merge
        (fun _ -> page#notify)
        ()
        [ E.map (fun x -> `Bitrate x) bitrate_ev
        ; E.map (fun x -> `PIDs x) pids_ev
        ; E.map (fun x -> `State x) state_ev
        ; E.map (fun x -> `Services x) services_ev ]
    in
    let state =
      object
        method finalize () =
          E.stop ~strong:true bitrate_ev;
          E.stop ~strong:true notif;
          Api_js.Websocket.close_socket socket
      end
    in
    state_ref := Some state;
    Lwt.return_ok state
  in
  let _loader = Components_lab.Loader.make_loader ~elt thread in
  ()

let on_hidden state_ref =
  match !state_ref with
  | None -> ()
  | Some state ->
      state#finalize ();
      state_ref := None

let observe page state_ref control records _observer =
  let open MutationObserver in
  let record =
    List.find_opt (fun (x : mutationRecord Js.t) ->
        x##._type == Js.string "attributes"
        && x##.attributeName == Js.some @@ Js.string Attr.hidden)
    @@ Array.to_list
    @@ Js.to_array records
  in
  match record with
  | None -> ()
  | Some x ->
      let (target : Dom_html.element Js.t) = Js.Unsafe.coerce x##.target in
      let current = target##getAttribute (Js.string "hidden") in
      if x##.oldValue != current
      then
        Js.Opt.case
          current
          (fun () -> on_visible page state_ref control target)
          (fun _ -> on_hidden state_ref)

let initialize id control =
  let open Board_niitv_tsan_widgets in
  let id =
    String.map (function
        | '/' -> '-'
        | c -> c)
    @@ Topology.make_board_path id control
  in
  let elt = Dom_html.getElementById id in
  let pie = Pid_bitrate_pie_chart.make () in
  let rate = Bitrate_summary.make () in
  let pids = Pid_summary.make () in
  let pid_overview = Pid_overview.make () in
  let service_overview = Service_overview.make () in
  let cells =
    Layout_grid.Markup_js.
      [ create_cell ~span:4 ~span_tablet:8 ~children:[pie#markup] ()
      ; create_cell
          ~span:8
          ~children:[rate#markup; Divider.Markup_js.create_hr (); pids#markup]
          ()
      ; create_cell ~span:12 ~children:[pid_overview#markup] ()
      ; create_cell ~span:12 ~children:[service_overview#markup] () ]
  in
  Element.add_class elt Layout_grid.CSS.inner;
  List.iter (Dom.appendChild elt % Tyxml_js.To_dom.of_element) cells;
  let state_ref = ref None in
  let page =
    object
      method notify =
        function
        | `Bitrate ((_, x) :: _) ->
            rate#notify (`Bitrate (Some x));
            pie#notify (`Bitrate (Some x));
            pid_overview#notify (`Bitrate (Some x));
            service_overview#notify (`Bitrate (Some x))
        | `PIDs ((_, x) :: _) ->
            pids#notify (`PIDs x);
            pid_overview#notify (`PIDs x);
            service_overview#notify (`PIDs x)
        | `State (x : Topology.state) ->
            pid_overview#notify (`State (x :> [Topology.state | `No_sync]));
            service_overview#notify (`State (x :> [Topology.state | `No_sync]))
        | `Services ((_, x) :: _) -> service_overview#notify (`Services x)
        | _ -> ()
    end
  in
  let _observer =
    MutationObserver.observe
      ~node:elt
      ~f:(observe page state_ref control)
      ~attributes:true
      ~attribute_old_value:true
      ~attribute_filter:[Js.string "hidden"]
      ()
  in
  Js.Opt.case
    (elt##getAttribute (Js.string Attr.hidden))
    (fun () -> on_visible page state_ref control elt)
    (fun _ -> on_hidden state_ref)

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
    | Some (id, controls) -> List.iter (initialize id) controls)
