open Boards.Board
open Common

let handle db api events =
  [ Board_api_device.handler api events
  ; Board_api_streams.handler api events
  ; Board_api_history.handler db
  ]

let handlers id db api events =
  [ Api_handler.add_layer (Topology.get_api_path id) (handle db api events)
  ]
