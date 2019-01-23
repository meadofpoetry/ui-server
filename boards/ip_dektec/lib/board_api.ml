open Boards.Board

let handle api events =
  [ Board_api_device.handler api events
  ; Board_api_receiver.handler api events
  ]

let handlers id api events =
  [ Api_handler.add_layer (Common.Topology.get_api_path id) (handle api events)
  ]
