open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Application_http_js
open Components
include Page_log_tyxml
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

module Selector = struct
  let application_log = Printf.sprintf ".%s" Application_widgets.Log.CSS.root
end

type data = {
  has_more : bool;
  last : Ptime.t option;
  log_entries : Stream.Log_message.t list;
}

type state = {
  mutable socket : Api_js.Websocket.JSON.t option;
  mutable finalize : unit -> unit;
}

let get_log ?order ?till ?from ~input () =
  Application_http_js.get_log ?order ?from ?till ~limit:50 ~inputs:[ input ] ()
  >>=? function
  | Api.Raw log_init -> Lwt.return_ok log_init
  | Compressed _ -> Lwt.return_error (`Msg "Unexpected compressed response")

let get_last_timestamp = function
  | [] -> None
  | (hd : Stream.Log_message.t) :: _ -> Some hd.time

class t ~input ~signal ~update (elt : Dom_html.element Js.t) () =
  object (self)
    val application_log : Application_widgets.Log.t =
      Application_widgets.Log.attach
      @@ Element.query_selector_exn elt Selector.application_log

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [ clicks application_log#has_more_button#root self#on_has_more_click ]

    method private on_has_more_click _ _ : unit Lwt.t =
      get_log ~order:`Desc ?till:(React.S.value signal).last ~input ()
      >>= function
      | Ok ({ has_more; data; _ } : _ Api.raw) ->
          application_log#set_has_more has_more;
          let more_data =
            {
              has_more;
              log_entries = List.rev data;
              last = get_last_timestamp data;
            }
          in
          update more_data;
          Lwt.return ()
      (* TODO: do smth here *)
      | Error _ -> Lwt.return ()
  end

let attach ~input ~update ~signal elt : t =
  new t ~input ~update ~signal (elt :> Dom_html.element Js.t) ()

let do_requests ~input state =
  get_log ~input () >>=? fun log_init ->
  Api_js.Websocket.JSON.open_socket
    ~path:(Netlib.Uri.Path.Format.of_string "ws")
    ()
  >>=? fun socket ->
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- Some socket;
  Event.get_log ~inputs:[ input ] socket >>=? fun (_, log_ev) ->
  let fin () = React.(E.stop ~strong:true log_ev) in
  Lwt.return_ok (log_init, log_ev, fin)

let on_visible ~input (elt : Dom_html.element Js.t) state =
  let open React in
  let open ReactiveData in
  let thread =
    do_requests ~input state >>=? fun ({ data; has_more; _ }, log_ev, fin) ->
    let init =
      { has_more; log_entries = List.rev data; last = get_last_timestamp data }
    in
    let e_more, set_more = E.create () in
    let s_data =
      S.fold
        (fun acc -> function
          | `More { log_entries; last; _ } ->
              { acc with log_entries = acc.log_entries @ log_entries; last }
          | `Socket x -> { acc with log_entries = x @ acc.log_entries })
        init
        (E.select
           [
             E.map (fun x -> `Socket x) log_ev; E.map (fun x -> `More x) e_more;
           ])
    in
    let signal = RList.from_signal @@ S.map (fun x -> x.log_entries) s_data in
    let data_table =
      Application_widgets.Log.R.create ~has_more ~init:signal ()
    in
    let page =
      attach ~input ~update:set_more ~signal:s_data
      @@ Tyxml_js.To_dom.of_div
      @@ D.create ~data_table ()
    in
    Dom.appendChild elt page#root;
    page#layout ();
    state.finalize <-
      (fun () ->
        Dom.removeChild elt page#root;
        page#destroy ();
        fin ());
    Lwt.return_ok state
  in
  let _loader = Components_lab.Loader.make_loader ~elt thread in
  state.finalize <- (fun () -> Lwt.cancel thread);
  ()

let on_hidden state =
  Option.iter Api_js.Websocket.close_socket state.socket;
  state.socket <- None;
  state.finalize ()

let init ~input () =
  let state = { socket = None; finalize = (fun () -> ()) } in
  let _result =
    Ui_templates.Tabbed_page.Tabpanel.init ~id
      ~on_visible:(fun tabpanel -> on_visible ~input tabpanel state)
      ~on_hidden:(fun _tabpanel -> on_hidden state)
      ()
  in
  ()
