open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Ui_templates
open Components
open Pipeline_types
open Pipeline_http_js
open Pipeline_widgets
include Pipeline_page_input_tyxml
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

  let parameter_chart = Printf.sprintf ".%s" Parameter_chart.CSS.root
end

type event = Parameter_chart.event

(** Make necessary HTTP and Websocket requests to the server. *)
let do_requests ~input state =
  Http_structure.get_annotated ()
  >>= fun structures_init ->
  Api_js.Websocket.JSON.open_socket ~path:(Netlib.Uri.Path.Format.of_string "ws") ()
  >>= fun socket ->
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- Some socket;
  Http_structure.Event.get_annotated socket
  >>= fun (_, structures_ev) ->
  Http_measurements.Event.get_video socket
  >>= fun (_, video_ev) ->
  Http_measurements.Event.get_audio socket
  >>= fun (_, audio_ev) ->
  let structures = React.S.hold structures_init structures_ev in
  let fin () =
    React.(
      S.stop ~strong:true structures;
      E.stop ~strong:true structures_ev)
  in
  Lwt.return_ok (structures, video_ev, audio_ev, fin)

class t ~set_stream ~set_hex ~structures elt () =
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

    val charts : Parameter_chart.t list =
      List.map (fun x -> Parameter_chart.attach ~init:[] ~structures x)
      @@ Element.query_selector_all elt Selector.parameter_chart

    inherit Widget.t elt () as super

    method! layout () : unit =
      stream_select#layout ();
      super#layout ()

    method! destroy () : unit =
      stream_select#destroy ();
      super#destroy ()

    method notify : event -> unit =
      function
      | `Structures _ as d ->
          List.iter (fun (chart : Parameter_chart.t) -> chart#notify d) charts
      | `Data _ as d ->
          List.iter (fun (chart : Parameter_chart.t) -> chart#notify d) charts
  end

let attach ~set_stream ~set_hex ~structures elt : t =
  new t ~set_stream ~set_hex ~structures (elt :> Dom_html.element Js.t) ()

let on_visible ~input (elt : Dom_html.element Js.t) (state : state) =
  let open React in
  let thread =
    do_requests ~input state
    >>= fun (structures, video_ev, audio_ev, fin) ->
    (* FIXME *)
    let stream, set_stream = S.create None in
    (* FIXME *)
    let hex, set_hex = S.create false in
    let stream_select = Stream_select.R.create ~streams:(S.const []) () in
    let page =
      attach ~set_stream ~set_hex ~structures:(React.S.value structures)
      @@ Tyxml_js.To_dom.of_div
      @@ D.create ~stream_select ()
    in
    let notif =
      E.merge
        (fun _ -> function
          | `Data _ as d -> page#notify d
          | `Structures _ as d -> page#notify d)
        ()
        [ E.map (fun structures -> `Structures structures) (S.changes structures)
        ; E.map
            (fun data ->
              let data =
                List.map
                  (fun ({ stream; channel; pid; data; _ } : Qoe_errors.Video_data.t) ->
                    let (source : Parameter_chart.data_source) =
                      { stream; service = channel; pid }
                    in
                    source, `Video data)
                  data
              in
              `Data data)
            video_ev
        ; E.map
            (fun data ->
              let data =
                List.map
                  (fun ({ stream; channel; pid; data; _ } : Qoe_errors.Audio_data.t) ->
                    let (source : Parameter_chart.data_source) =
                      { stream; service = channel; pid }
                    in
                    source, `Audio data)
                  data
              in
              `Data data)
            audio_ev
        ]
    in
    Dom.appendChild elt page#root;
    page#layout ();
    state.finalize <-
      (fun () ->
        Dom.removeChild elt page#root;
        page#destroy ();
        S.stop ~strong:true hex;
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
let init ~input () =
  let state = { socket = None; finalize = (fun () -> ()) } in
  let _result =
    Ui_templates.Tabbed_page.Tabpanel.init
      ~id
      ~on_visible:(fun tabpanel -> on_visible ~input tabpanel state)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ()
  in
  ()

let () =
  let input =
    Topology.topo_input_of_yojson
    @@ Yojson.Safe.from_string
    @@ Js.to_string Js.Unsafe.global##.input
  in
  match input with
  | Ok input -> init ~input ()
  | Error _e -> ()
