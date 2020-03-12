open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Ui_templates
open Components
open Pipeline_http_js
open Pipeline_widgets
open Pipeline_types
include Pipeline_page_input_tyxml
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

let ( >>= ) = Lwt_result.bind

type state = {
  mutable socket : Api_js.Websocket.JSON.t option;
  mutable finalize : unit -> unit;
}
(** Tab state. *)

module Selector = struct
  let stream_select = Printf.sprintf ".%s" Stream_select.CSS.root

  let parameter_chart = Printf.sprintf ".%s" Parameter_chart.CSS.root
end

type event =
  [ `Video_data of Qoe_errors.Video_data.t list
  | `Audio_data of Qoe_errors.Audio_data.t list
  | `Structures of Structure.Annotated.t ]

(** Make necessary HTTP and Websocket requests to the server. *)
let do_requests ~input state =
  Http_structure.get_streams ~inputs:[ input ] () >>= fun streams_init ->
  Http_structure.get_annotated () >>= fun structures_init ->
  Api_js.Websocket.JSON.open_socket
    ~path:(Netlib.Uri.Path.Format.of_string "ws")
    ()
  >>= fun socket ->
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- Some socket;
  Http_structure.Event.get_annotated socket >>= fun (_, structures_ev) ->
  Http_measurements.Event.get_video socket >>= fun (_, video_ev) ->
  Http_measurements.Event.get_audio socket >>= fun (_, audio_ev) ->
  (* FIXME replace `never` with something meaningful *)
  let streams =
    React.S.map (List.map snd) (React.S.hold streams_init React.E.never)
  in
  let structures = React.S.hold structures_init structures_ev in
  let fin () =
    React.(
      S.stop ~strong:true structures;
      E.stop ~strong:true structures_ev)
  in
  Lwt.return_ok (streams, structures, video_ev, audio_ev, fin)

class t ~set_stream ~set_hex ~structures elt () =
  let charts : Parameter_chart.t list =
    List.map (fun x -> Parameter_chart.attach ~init:[] ~structures x)
    @@ Element.query_selector_all elt Selector.parameter_chart
  in
  object
    val stream_select : Stream_select.t =
      Stream_select.attach ~on_change:(fun x ->
          let id =
            match x#value with None -> None | Some (x : Stream.t) -> Some x.id
          in
          List.iter (fun (x : Parameter_chart.t) -> x#clear ()) charts;
          set_stream id)
      @@ Element.query_selector_exn elt Selector.stream_select

    val black_chart : Parameter_chart.t =
      List.find (fun x -> Util.equal_measure_typ x#typ `Black) charts

    val luma_chart : Parameter_chart.t =
      List.find (fun x -> Util.equal_measure_typ x#typ `Luma) charts

    val freeze_chart : Parameter_chart.t =
      List.find (fun x -> Util.equal_measure_typ x#typ `Freeze) charts

    val diff_chart : Parameter_chart.t =
      List.find (fun x -> Util.equal_measure_typ x#typ `Diff) charts

    val blocky_chart : Parameter_chart.t =
      List.find (fun x -> Util.equal_measure_typ x#typ `Blocky) charts

    val shortt_chart : Parameter_chart.t =
      List.find (fun x -> Util.equal_measure_typ x#typ `Shortt) charts

    val moment_chart : Parameter_chart.t =
      List.find (fun x -> Util.equal_measure_typ x#typ `Moment) charts

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
      | `Video_data data ->
          let black, luma, freeze, diff, blocky =
            List.fold_right
              (fun ({ stream; channel; pid; data; _ } : Qoe_errors.Video_data.t)
                   (black, luma, freeze, diff, blocky) ->
                let source =
                  ({ stream; channel; pid } : Parameter_chart.data_source)
                in
                ( (source, data.black) :: black,
                  (source, data.luma) :: luma,
                  (source, data.freeze) :: freeze,
                  (source, data.diff) :: diff,
                  (source, data.blocky) :: blocky ))
              data ([], [], [], [], [])
          in
          black_chart#notify (`Data black);
          luma_chart#notify (`Data luma);
          freeze_chart#notify (`Data freeze);
          diff_chart#notify (`Data diff);
          blocky_chart#notify (`Data blocky)
      | `Audio_data data ->
          let moment, shortt =
            List.fold_right
              (fun ({ stream; channel; pid; data; _ } : Qoe_errors.Audio_data.t)
                   (moment, shortt) ->
                let source =
                  ({ stream; channel; pid } : Parameter_chart.data_source)
                in
                ( (source, data.moment) :: moment,
                  (source, data.shortt) :: shortt ))
              data ([], [])
          in
          moment_chart#notify (`Data moment);
          shortt_chart#notify (`Data shortt)
  end

let attach ~set_stream ~set_hex ~structures elt : t =
  new t ~set_stream ~set_hex ~structures (elt :> Dom_html.element Js.t) ()

let filter_video_data data = function
  | None -> []
  | Some id ->
      List.filter
        (fun ({ stream; _ } : Qoe_errors.Video_data.t) ->
          Stream.ID.equal stream id)
        data

let filter_audio_data data = function
  | None -> []
  | Some id ->
      List.filter
        (fun ({ stream; _ } : Qoe_errors.Audio_data.t) ->
          Stream.ID.equal stream id)
        data

let on_visible ~input (elt : Dom_html.element Js.t) (state : state) =
  let open React in
  let thread =
    do_requests ~input state
    >>= fun (streams, structures, video_ev, audio_ev, fin) ->
    let stream, set_stream =
      S.create (match S.value streams with [] -> None | x :: _ -> Some x.id)
    in
    let hex, set_hex = S.create false in
    let stream_select = Stream_select.R.create ~streams () in
    let page =
      attach ~set_stream ~set_hex ~structures:(React.S.value structures)
      @@ Tyxml_js.To_dom.of_div
      @@ D.create ~stream_select ()
    in
    let notif =
      E.merge
        (fun _ x -> page#notify x)
        ()
        [
          E.map
            (fun structures -> `Structures structures)
            (S.changes structures);
          E.map
            (fun data -> `Video_data data)
            (S.sample filter_video_data video_ev stream);
          E.map
            (fun data -> `Audio_data data)
            (S.sample filter_audio_data audio_ev stream);
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
    Ui_templates.Tabbed_page.Tabpanel.init ~id
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
  match input with Ok input -> init ~input () | Error _e -> ()
