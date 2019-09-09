open Js_of_ocaml
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
  | `PIDs of (Stream.ID.t * (int * PID_info.t) list ts) list ]

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
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter (fun (state : state) -> state#finalize ()) !state_ref;
    Http_monitoring.Event.get_bitrate socket control
    >>=? fun (_, bitrate_ev) ->
    Http_monitoring.Event.get_pids socket control
    >>=? fun (_, pids_ev) ->
    let notif =
      E.merge
        (fun _ -> page#notify)
        ()
        [E.map (fun x -> `Bitrate x) bitrate_ev; E.map (fun x -> `PIDs x) pids_ev]
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
  let pie_cell =
    Layout_grid.Cell.make ~span:4 ~span_tablet:8 ~children:[pie#markup] ()
  in
  let rate_cell =
    Layout_grid.Cell.make
      ~span:8
      ~children:[rate#markup; Divider.Markup_js.create_hr (); pids#markup]
      ()
  in
  let cells = [pie_cell; rate_cell] in
  Element.add_class elt Layout_grid.CSS.inner;
  List.iter (fun x -> Dom.appendChild elt x#root) cells;
  let state_ref = ref None in
  let page =
    object
      method notify =
        function
        | `Bitrate ((_, x) :: _) ->
            rate#notify (`Bitrate (Some x));
            pie#notify (`Bitrate (Some x))
        | `PIDs ((_, x) :: _) -> pids#notify (`PIDs x)
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
