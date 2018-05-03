open Containers
open Components
open Board_types
open Lwt_result.Infix
open Common.Topology

module L = struct
  type config = Board_types.config
  type status = Board_types.user_status
  type config_listener = WebSockets.webSocket Js.t
  type status_listener = WebSockets.webSocket Js.t

  let get_config    = Requests.get_config
  let listen_config = Requests.get_config_ws
  let listen_status = Requests.get_status_ws
  let unlisten_config c = c##close
  let unlisten_status s = s##close
end
module Listener = Boards_js.Topo_listener.Make(L)

type board_info_ext =
  { streams : Common.Stream.t_list React.signal
  ; base    : Listener.board_info
  }

let make_t2mi (board:topo_board) ({streams;base = {config;events;state}}:board_info_ext) =
  let init    = config.mode.t2mi in
  let event   = React.E.map (fun x -> x.mode.t2mi) events.config in
  let w,s,set = Settings.make_t2mi_mode ~init ~event ~streams ~state board.control () in
  let a       = Ui_templates.Buttons.create_apply s set in
  let abox    = (new Card.Actions.t ~widgets:[a] ())#widget in
  let box     = (new Box.t ~vertical:true ~widgets:[w;abox] ())#widget in
  box

let make_jitter (board:topo_board) ({streams;base = { config;events;state}}:board_info_ext) =
  let init    = config.jitter_mode in
  let event   = React.E.map (fun x -> x.jitter_mode) events.config in
  let w,s,set = Settings.make_jitter_mode ~init ~event ~state board.control () in
  let a       = Ui_templates.Buttons.create_apply s set in
  let abox    = (new Card.Actions.t ~widgets:[a] ())#widget in
  let box     = (new Box.t ~vertical:true ~widgets:[w;abox] ())#widget in
  box

let make ?error_prefix (board:topo_board) : (#Widget.widget,string) Lwt_result.t =
  Listener.listen board.control
  >>= (fun (base,state) ->
    let res =
      Requests.get_incoming_streams board.control
      >>= (fun streams ->
        let es,sock = Requests.get_incoming_streams_ws board.control in
        let streams = React.S.hold ~eq:(Equal.list Common.Stream.equal) streams es in
        let bi_ext  = { streams; base } in
        let t2mi    = make_t2mi board bi_ext in
        let jitter  = make_jitter board bi_ext in
        Lwt_result.return ((t2mi,jitter), (fun () -> sock##close)))
    in
    let pgs    = Ui_templates.Loader.create_widget_loader ?error_prefix in
    let t2mi   = pgs @@ Lwt.map (function Ok x    -> Ok (fst @@ fst x)
                                        | Error e -> Error (Api_js.Requests.err_to_string e)) res in
    let jitter = pgs @@ Lwt.map (function Ok x    -> Ok (snd @@ fst x)
                                        | Error e -> Error (Api_js.Requests.err_to_string e)) res in
    let tabs   = Ui_templates.Tabs.create_simple_tabs [ `Text "T2-MI", t2mi#widget
                                                      ; `Text "Джиттер", jitter#widget
                                                      ]
    in
    let fin () = Listener.unlisten state;
                 res >>= (fun (_,fin) -> fin (); Lwt_result.return ())
                 |> Lwt.ignore_result
    in
    tabs#set_on_destroy (Some fin);
    Lwt_result.return tabs)
