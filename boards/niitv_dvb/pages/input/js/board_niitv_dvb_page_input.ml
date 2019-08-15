open Js_of_ocaml
open Netlib
open Components
open Board_niitv_dvb_http_js
(* open Board_niitv_dvb_widgets *)

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Attr = struct
  let hidden = "hidden"
end

(* module Chart = Widget_chart.Make(struct
 *     include Util_json.Int
 *     let to_string = string_of_int
 *     let equal = ( = )
 *   end)
 * 
 * let make_config ?(sources = []) ?range
 *     ?(period = `Realtime (Time.Span.of_int_s 60)) typ =
 *   { Chart. sources; typ; settings = { range; period }} *)

let on_visible (socket_ref : 'a ref) elt =
  let thread =
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Option.iter Api_js.Websocket.close_socket !socket_ref;
    socket_ref := Some socket;
    Http_receivers.Event.get_measurements socket 1
    >>=? fun (_, _meas_ev) ->
    Lwt.return_ok () in
  let _loader = Ui_templates.Loader.make_loader ~elt thread in
  ()

let on_hidden socket_ref =
  match !socket_ref with
  | None -> ()
  | Some socket ->
    Api_js.Websocket.close_socket socket;
    socket_ref := None

let observe socket records _observer =
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
        (fun () -> on_visible socket target)
        (fun _ -> on_hidden socket)

let () =
  let (_scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let socket = ref None in
  let elt = Dom_html.getElementById "rf-tabpanel" in
  let _observer = MutationObserver.observe
      ~node:elt
      ~f:(observe socket)
      ~attributes:true
      ~attribute_old_value:true
      ~attribute_filter:[Js.string "hidden"]
      () in
  Js.Opt.case (elt##getAttribute (Js.string Attr.hidden))
    (fun () -> on_visible socket elt)
    (fun _ -> on_hidden socket)


  (* let thread =
   *   let open React in
   *   Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
   *   >>=? fun socket -> Http_receivers.Event.get_measurements socket 1
   *   >>=? fun (_, meas_ev) ->
   *   (\* let pwr = Chart.make [] (make_config `Power) in
   *    * let mer = Chart.make [] (make_config `Mer) in
   *    * let ber = Chart.make [] (make_config `Ber) in
   *    * let frq = Chart.make [] (make_config `Freq) in
   *    * let btr = Chart.make [] (make_config `Bitrate) in *\)
   *   let charts = [(\* pwr; mer; ber; frq; btr *\)] in
   *   let notif =
   *     E.merge (fun _ data -> List.iter (fun x -> x#notify data) charts) ()
   *       [ E.map (fun x -> `Data (List.map (fun (id, x) -> id, [x]) x)) meas_ev
   *       ] in
   *   List.iter (fun chart ->
   *       chart#root##.style##.height := Js.string "200px";
   *       chart#root##.style##.padding := Js.string "1rem";
   *       chart#root##.style##.marginBottom := Js.string "20px";
   *       chart#add_class Card.CSS.root) charts;
   *   let box = Box.make ~dir:`Column charts in
   *   box#set_on_destroy (fun () ->
   *       E.stop ~strong:true notif;
   *       E.stop ~strong:true meas_ev;
   *       Api_js.Websocket.close_socket socket);
   *   Lwt.return_ok box in
   * let _loader = Ui_templates.Loader.make_widget_loader ~elt thread in *)
