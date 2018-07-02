open Boards.Board
open Common
open Types

let handle (api:api) (events:events) =
  [ Board_api_device.handler api events.device
  ; Api_handler.add_layer "errors"  (Board_api_errors.handlers events.errors)
  ; Api_handler.add_layer "stream"  (Board_api_stream.handlers api events.streams)
  ; Api_handler.add_layer "streams" (Board_api_streams.handlers api events.streams)
  ; Api_handler.add_layer "jitter"  (Board_api_jitter.handlers events.jitter)
  ]

let handlers id api events =
  [ Api_handler.add_layer (Common.Topology.get_api_path id) (handle api events)
  ]
