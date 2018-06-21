open Board_protocol
open Boards.Board
open Common

let handle api events =
  [ (module struct
       let domain = "device"
       let handle = Board_api_device.handler api events
     end : Api_handler.HANDLER)
  ; (module struct
       let domain = "receiver"
       let handle = Board_api_receiver.handler api events
     end : Api_handler.HANDLER)
  ]

let handlers id api events =
  [ Api_handler.add_layer (Topology.get_api_path id) (handle api events)
  ]
