open Boards.Board
open Common
open Board_protocol

let handle (api:api) (events:events) =
  [ Board_api_device.handler api events
  ; Board_api_transmitter.handler api events
  ]

let handlers id api events =
  [ Api_handler.add_layer (Topology.get_api_path id) (handle api events) ]
