open Js_of_ocaml
open Netlib
open Components
open Application_types
open Board_niitv_dvb_types
open Board_niitv_dvb_http_js

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Attr = struct
  let hidden = "hidden"
end

let make_charts mode =
  let open Board_niitv_dvb_widgets.Widget_chart in
  let pwr = make ~init:[] ~mode (make_config `Power) in
  let mer = make ~init:[] ~mode (make_config `Mer) in
  let ber = make ~init:[] ~mode (make_config `Ber) in
  let frq = make ~init:[] ~mode (make_config `Freq) in
  let btr = make ~init:[] ~mode (make_config `Bitrate) in
  let charts = [pwr; mer; ber; frq; btr] in
  List.iter (fun chart ->
      chart#root##.style##.padding := Js.string "1rem";
      chart#root##.style##.marginBottom := Js.string "20px";
      chart#add_class Card.CSS.root) charts;
  object
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      List.iter super#append_child charts;
      super#init ()

    method notify data =
      List.iter (fun x -> x#notify data) charts
  end

let on_visible charts state control elt =
  let open React in
  let thread =
    Http_device.get_mode control
    >>=? fun mode ->
    charts#notify (`Mode mode);
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter (fun f -> f ()) !state;
    Http_receivers.Event.get_measurements socket control
    >>=? fun (_, meas_ev) ->
    Http_device.Event.get_mode socket control
    >>=? fun (_, mode_ev) ->
    let notif = E.merge (fun _ x -> charts#notify x) ()
        [ E.map (fun x -> `Data (List.map (fun (id, x) -> id, [x]) x)) meas_ev
        ; E.map (fun x -> `Mode x) mode_ev
        ] in
    state := Some (fun () ->
        E.stop ~strong:true mode_ev;
        E.stop ~strong:true meas_ev;
        E.stop ~strong:true notif;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok () in
  let _loader = Ui_templates.Loader.make_loader ~elt thread in
  ()

let on_hidden state = match !state with
  | None -> ()
  | Some finalize -> finalize (); state := None

let observe charts state control records _observer =
  let open MutationObserver in
  let record =
    List.find_opt (fun (x : mutationRecord Js.t) ->
        x##._type == Js.string "attributes"
        && x##.attributeName == Js.some @@ Js.string Attr.hidden)
    @@ Array.to_list @@ Js.to_array records in
  match record with
  | None -> ()
  | Some x ->
    let (target : Dom_html.element Js.t) = Js.Unsafe.coerce x##.target in
    let current = target##getAttribute (Js.string "hidden") in
    if x##.oldValue != current
    then Js.Opt.case current
        (fun () -> on_visible charts state control target)
        (fun _ -> on_hidden state)

let initialize id control =
  let id =
    String.map (function '/' -> '-' | c -> c)
    @@ Topology.make_board_path id control in
  let (_scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let state = ref None in
  let elt = Dom_html.getElementById id in
  let charts = make_charts [] in
  Dom.appendChild elt charts#root;
  let _observer = MutationObserver.observe
      ~node:elt
      ~f:(observe charts state control)
      ~attributes:true
      ~attribute_old_value:true
      ~attribute_filter:[Js.string "hidden"]
      () in
  Js.Opt.case (elt##getAttribute (Js.string Attr.hidden))
    (fun () -> on_visible charts state control elt)
    (fun _ -> on_hidden state)

let () =
  let boards =
    Topology.boards_of_yojson
    @@ Yojson.Safe.from_string
    @@ Js.to_string Js.Unsafe.global##.boards in
  match boards with
  | Error _ -> ()
  | Ok boards ->
    match List.find_opt (Topology.equal_board_id board_id % fst) boards with
    | None -> ()
    | Some (id, controls) -> List.iter (initialize id) controls
