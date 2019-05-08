open Application_types
open Netlib.Uri
open Board_niitv_tsan_protocol

module Api_http = Api_cohttp.Make(User)(Body)

module Api_websocket = Api_websocket.Make(User)(Body)

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [ merge ~prefix:(Topology.get_api_path control)
      [ make ~prefix:"stream"
          [ node ~doc:"Returns structures of the analyzed streams"
              ~meth:`GET
              ~path:(Path.Format.of_string "structure")
              ~query:Query.empty
              (Api_stream.get_structure api)
          ]
      ]
  ]
