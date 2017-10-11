open Common.Hardware
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

let create (b:topo_board) send db base step =
  let storage          = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush   = React.S.create `No_response in
  let events,api,step  = create_sm send storage spush step in
  let handlers         = Board_api.handlers b.control api events in
  let s_asi,s_asi_push = React.S.create false in
  let s_spi,s_spi_push = React.S.create false in
  (* let _                = React.S.map (fun x -> Lwt_io.printf "%s" ("spi is " ^ (if x then "active" else "inactive"))) *)
  (*                                    s_spi in *)
  (* let _                = React.S.map (fun x -> Lwt_io.printf "%s" ("asi is " ^ (if x then "active" else "inactive"))) *)
  (*                                    s_asi in *)
  let e_status         = React.E.map (fun (x : user_status) -> (match x.mode.input with
                                                                | SPI -> s_spi_push true; s_asi_push false
                                                                | ASI -> s_asi_push true; s_spi_push false))
                                     events.status in
  let e_ts_found       = React.E.map (fun strm ->
                             Common.Stream.id_to_yojson strm
                             |> Yojson.Safe.to_string
                             |> Lwt_io.printf "Stream found: %s\n"
                             |> ignore) events.ts_found in
  let e_ts_lost       = React.E.map (fun strm ->
                            Common.Stream.id_to_yojson strm
                            |> Yojson.Safe.to_string
                            |> Lwt_io.printf "Stream lost: %s\n"
                            |> ignore) events.ts_lost in
  let e_state         = React.S.map (function
                                     | `No_response -> Lwt_io.printf "QoS Board not responding\n"
                                     | `Fine        -> Lwt_io.printf "QoS Board is OK\n"
                                     | `Init        -> Lwt_io.printf "QoS Board is initializing\n") s_state in
  let state           = (object
                           method e_status   = e_status;
                           method e_state    = e_state;
                           method e_ts_found = e_ts_found;
                           method e_ts_lost  = e_ts_lost;
                         end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = None
  ; step           = step
  ; is_converter   = false
  ; connection     = s_state
  ; ports_active   = (List.fold_left (fun acc p ->
                          (match p.port with
                           | 0 -> s_spi
                           | 1 -> s_asi
                           | x -> raise (Invalid_port ("Board_qos_niit: invalid port " ^ (string_of_int x))))
                          |> fun x -> Ports.add p.port x acc)
                                     Ports.empty b.ports)
  ; state          = (state :> < >)
  }
