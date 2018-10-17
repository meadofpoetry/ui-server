open Boards.Board
open Board_qos_types
open Common

let handlers id db sources api events =
  [ Api_handler.add_layer (Topology.get_api_path id)
      [ Board_api_device.handler db api events.device
      ; Board_api_streams.handler sources api events
      ; Board_api_history.handler db events
      ]
  ]
