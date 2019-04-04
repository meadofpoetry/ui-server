open Application_types
open Netlib.Uri
open Board_niitv_dvb_protocol

module Api_http = Api_cohttp.Make(User)(Body)

module Api_websocket = Api_websocket.Make(User)(Body)

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [ merge ~prefix:(Topology.get_api_path control)
      [ make ~prefix:"device"
          [ node ~doc:"Resets the board"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.("reset" @/ empty)
              ~query:Query.empty
              (Api_device.reset api)
          ; node ~doc:"Sets tuner receiving mode"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.("mode" @/ Int ^/ empty)
              ~query:Query.empty
              (Api_device.set_mode api)
          ; node ~doc:"Returns the state of the board"
              ~meth:`GET
              ~path:Path.Format.("state" @/ empty)
              ~query:Query.empty
              (Api_device.get_state api)
          ; node ~doc:"Returns board description and capabilities, if available"
              ~meth:`GET
              ~path:Path.Format.("info" @/ empty)
              ~query:Query.empty
              (Api_device.get_devinfo api)
          ; node ~doc:"Returns available tuner indexes"
              ~meth:`GET
              ~path:Path.Format.("receivers" @/ empty)
              ~query:Query.empty
              (Api_device.get_receivers api)
          ; node ~doc:"Returns the receiving mode of the requested tuner(s)"
              ~meth:`GET
              ~path:Path.Format.("mode" @/ empty)
              ~query:Query.["id", (module List(Int))]
              (Api_device.get_mode api)
          ]
      ; make ~prefix:"history"
          [ node ~doc:"Returns measurement results for the specified time interval"
              ~meth:`GET
              ~path:Path.Format.("measurements" @/ empty)
              ~query:Query.[ "id", (module List(Int))
                           ; "limit", (module Option(Int))
                           ; "from", (module Option(Time_uri.Show))
                           ; "to", (module Option(Time_uri.Show))
                           ; "duration", (module Option(Time_uri.Show_relative)) ]
              (Api_history.get_measurements api)
          ]
      ]
  ]

let ws (control : int) (api : Protocol.api) =
  let open Api_http in
  let open Api_websocket in
  [ merge ~prefix:(Topology.get_api_path control)
      [ make ~prefix:"device"
          [ node ~doc:"Board state socket"
              ~path:Path.Format.("state" @/ empty)
              ~query:Query.empty
              (Api_device.Event.get_state api)
          ; node ~doc:"Tuner receiving mode socket"
              ~path:Path.Format.("mode" @/ empty)
              ~query:Query.["id", (module List(Int))]
              (Api_device.Event.get_mode api)
          ; node ~doc:"Available tuner indexes socket"
              ~path:Path.Format.("receivers" @/ empty)
              ~query:Query.empty
              (Api_device.Event.get_receivers api)
          ]
      ; make ~prefix:"receiver"
          [ node ~doc:"Receiver measures socket"
              ~path:Path.Format.("measurements" @/ empty)
              ~query:Query.["id", (module List(Int))]
              (Api_receiver.Event.get_measures api)
          ; node ~doc:"Receiver parameters socket"
              ~path:Path.Format.("parameters" @/ empty)
              ~query:Query.["id", (module List(Int))]
              (Api_receiver.Event.get_parameters api)
          ; node ~doc:"Receiver PLP list socket"
              ~path:Path.Format.("plp-list" @/empty)
              ~query:Query.["id", (module List(Int))]
              (Api_receiver.Event.get_plps api)
          ]
      ; make ~prefix:"stream"
          [ node ~doc:"Streams socket"
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_stream.Event.get_streams api)
          ; node ~doc:"Stream measures socket"
              ~path:Path.Format.("measurements" @/ empty)
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_stream.Event.get_measures api)
          ; node ~doc:"Stream parameters socket"
              ~path:Path.Format.("parameters" @/ empty)
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_stream.Event.get_parameters api)
          ; node ~doc:"Stream PLP list socket"
              ~path:Path.Format.("plp-list" @/ empty)
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_stream.Event.get_plps api)
          ]
      ]
  ]
