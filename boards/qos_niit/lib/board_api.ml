open Boards.Board
open Common
open Types

let handlers id db api events =
  [ Api_handler.add_layer (Topology.get_api_path id)
      [ Board_api_device.handler db api events.device
      ; Board_api_streams.handler db api events
      ]
  ]
