open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_widgets
open Board_niitv_tsan_http_js
open Components
include Board_niitv_tsan_page_pids_tyxml
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Attr = struct
  let hidden = "hidden"
end

module Selector = struct
  let pid_bitrate_pie_chart = "." ^ Pid_bitrate_pie_chart.CSS.root

  let bitrate_summary = "." ^ Bitrate_summary.CSS.root

  let pid_summary = "." ^ Pid_summary.CSS.root

  let pid_overview = "." ^ Pid_overview.CSS.root
end

type event =
  [ `Bitrate of (Stream.ID.t * Bitrate.t) list
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

let make ?classes ?attrs ?children ~control () : t =
  Markup_js.create ?classes ?attrs ?children ~control ()
  |> Tyxml_js.To_dom.of_div
  |> attach

(* Tab switch logic *)

type state = < finalize : unit -> unit >

let on_visible (page : t) state_ref control elt =
  let open React in
  let thread =
    Http_monitoring.get_pids control
    >>=? fun pids ->
    page#notify (`PIDs pids);
    Api_js.Websocket.JSON.open_socket ~path:(Netlib.Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter (fun (state : state) -> state#finalize ()) !state_ref;
    Http_device.Event.get_state socket control
    >>=? fun (_, state_ev) ->
    Http_monitoring.Event.get_bitrate socket control
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

let init control =
  match Dom_html.getElementById_opt (id control) with
  | None -> None
  | Some elt ->
      let state_ref = ref None in
      let page = attach elt in
      let node =
        Js.Opt.get (Components.Element.get_parent elt) (fun () -> assert false)
      in
      let observer =
        MutationObserver.observe
          ~node
          ~f:(observe page state_ref control)
          ~attributes:true
          ~attribute_old_value:true
          ~attribute_filter:[Js.string Attr.hidden]
          ()
      in
      Js.Opt.case
        (node##getAttribute (Js.string Attr.hidden))
        (fun () -> on_visible page state_ref control elt)
        (fun _ -> on_hidden state_ref);
      page#set_on_destroy (fun () -> observer##disconnect);
      Some page
