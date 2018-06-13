open Common.Topology
open Api.Interaction
open Boards.Board
open Board_types

module Api_handler = Api.Handler.Make (Common.User)

open Lwt.Infix

module Data = struct

  type t      = Board_types.config
  let default = Board_types.config_default
  let dump    = Board_types.config_to_string
  let restore = Board_types.config_of_string

end

module Config_storage = Storage.Options.Make (Data)

type 'a request = 'a Board_protocol.request

let create_sm = Board_protocol.SM.create

let create (b:topo_board) incoming_streams convert_streams send db base step =
  let storage          = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush   = React.S.create `No_response in
  let s_inp,s_inp_push = React.S.create storage#get.mode.input in
  let events,api,step  = create_sm send storage spush step in
  (* FIXME incoming streams should be modified to include streams that are detected by the board itself *)
  let handlers         = Board_api.handlers b.control api events s_state s_inp incoming_streams in
  let e_status         = React.E.map (fun (x : user_status) -> s_inp_push x.mode.input) events.status in
  let s_streams        =
    React.S.l2 (fun x inp ->
        let open Common.Stream in
        List.map (fun x : stream ->
                          { source = (match x with
                                      | T2mi_plp _ -> Stream Single
                                      | _          -> Port (match inp with SPI -> 0 | ASI -> 1))
                          ; id          = `Ts x
                          ; description = Some "" }) x) events.streams s_inp
  in
  let sms = convert_streams s_streams b in
  let state           = (object
                           method e_status   = e_status;
                           method s_streams = s_streams;
                           method finalize () = ()
                         end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = sms
  ; step           = step
  ; connection     = s_state
  ; ports_active   = (List.fold_left (fun acc p ->
                          (match p.port with
                           | 0 -> React.S.map (function SPI -> true | _ -> false) s_inp
                           | 1 -> React.S.map (function ASI -> true | _ -> false) s_inp
                           | x -> raise (Invalid_port ("Board_qos_niit: invalid port " ^ (string_of_int x))))
                          |> fun x -> Ports.add p.port x acc)
                                     Ports.empty b.ports)
  ; settings_page  = ("QOS", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("QOS", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
