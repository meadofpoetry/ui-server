open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Components
open Board_niitv_tsan_types
open Board_niitv_tsan_http_js
open Board_niitv_tsan_widgets
include Board_niitv_tsan_page_si_psi_tyxml
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>=? ) = Lwt_result.bind

module Selector = struct
  let si_psi_overview = Printf.sprintf ".%s" Si_psi_overview.CSS.root
end

type event =
  [ `Bitrate of (Stream.ID.t * Bitrate.ext) list
  | `Tables of (Stream.ID.t * (SI_PSI_table.id * SI_PSI_table.t) list ts) list
  | `State of Topology.state
  ]

class t elt () =
  object
    val si_psi_overview : Si_psi_overview.t =
      Si_psi_overview.attach @@ Element.query_selector_exn elt Selector.si_psi_overview

    inherit Widget.t elt () as super

    method! destroy () : unit =
      si_psi_overview#destroy ();
      super#destroy ()
  end

let attach elt : t = new t (elt :> Dom_html.element Js.t) ()

type state = {
  mutable socket : Api_js.Websocket.JSON.t option;
  mutable finalize : unit -> unit;
}

let on_visible (elt : Dom_html.element Js.t) (state : state) control =
  let open React in
  let thread =
    Http_monitoring.get_si_psi_tables control >>=? fun _tables ->
    Api_js.Websocket.JSON.open_socket ~path:(Netlib.Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter Api_js.Websocket.close_socket state.socket;
    state.socket <- Some socket;
    Http_device.Event.get_state socket control >>=? fun (_, state_ev) ->
    Http_monitoring.Event.get_bitrate_with_stats socket control
    >>=? fun (_, bitrate_ev) ->
    Http_monitoring.Event.get_si_psi_tables socket control >>=? fun (_, tables_ev) ->
    let notif =
      E.merge
        (fun _ _ -> ())
        ()
        [
          E.map (fun x -> `Bitrate x) bitrate_ev;
          E.map (fun x -> `Tables x) tables_ev;
          E.map (fun x -> `State x) state_ev;
        ]
    in
    state.finalize <-
      (fun () ->
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
  let result =
    Ui_templates.Tabbed_page.Tabpanel.init
      ~id:(id control)
      ~on_visible:(fun tabpanel -> on_visible tabpanel state control)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ()
  in
  match result with
  | Ok (page, finalize) -> (* page#set_on_destroy (fun () -> finalize ()) *) ()
  | Error (`Msg _msg) -> ()
