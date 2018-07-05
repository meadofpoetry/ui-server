open Boards.Board
open Common
open Types

let handle db (api:api) (events:events) =
  [ Board_api_device.handler api events.device
  ; Api_handler.add_layer "errors"  (Board_api_errors.handlers db events.errors)
  ; Api_handler.add_layer "stream"  (Board_api_stream.handlers api events.streams)
  ; Api_handler.add_layer "streams" (Board_api_streams.handlers api events.streams)
  ; Api_handler.add_layer "jitter"  (Board_api_jitter.handlers events.jitter)
  ]

let handlers id db api events =
  [ Api_handler.add_layer (Topology.get_api_path id) (handle db api events)
  ]
