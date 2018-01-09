open Common.Topology
open Api.Interaction
open Meta_board
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

let create (b:topo_board) convert_streams send db base step =
  let storage          = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush   = React.S.create `No_response in
  let events,api,step  = create_sm send storage spush step in
  let handlers         = Board_api.handlers b.control api events s_state in
  let s_asi,s_asi_push = React.S.create false in
  let s_spi,s_spi_push = React.S.create false in
  let e_status         = React.E.map (fun (x : user_status) -> match x.mode.input with
                                                               | SPI -> s_spi_push true; s_asi_push false
                                                               | ASI -> s_asi_push true; s_spi_push false)
                                     events.status in
  let s_streams        = React.S.map (fun x ->
                             let open Common.Stream in
                             List.map (fun x : stream ->
                                               { source      = (match x with
                                                                | T2mi_plp _ -> Stream Single
                                                                | _          -> Port (if React.S.value s_spi
                                                                                      then 0
                                                                                      else 1))
                                               ; id          = `Ts x
                                               ; description = Some "" }) x)
                                     events.streams in
  let sms = convert_streams s_streams b in
  let _e = React.E.map (fun s ->
               `List (List.map Common.Stream.to_yojson s)
               |> Yojson.Safe.pretty_to_string
               |> Lwt_io.printf "QOS sms: %s\n"
               |> ignore;) @@ React.S.changes sms in
  let state           = (object
                           method e_status   = e_status;
                           method s_streams = s_streams;
                           method _e = _e;
                         end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = sms
  ; step           = step
  ; connection     = s_state
  ; ports_active   = (List.fold_left (fun acc p ->
                          (match p.port with
                           | 0 -> s_spi
                           | 1 -> s_asi
                           | x -> raise (Invalid_port ("Board_qos_niit: invalid port " ^ (string_of_int x))))
                          |> fun x -> Ports.add p.port x acc)
                        Ports.empty b.ports)
  ; settings_page  = ("QOS", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("QOS", React.S.const (Tyxml.Html.div []))]
  ; state          = (state :> < >)
  }
