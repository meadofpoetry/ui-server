open Application_types
open Netlib.Uri
open Board_niitv_dvb_protocol

module Api_http = Api_cohttp.Make(User)(Body)

module Api_websocket = Api_websocket.Make(User)(Body)

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [ merge ~prefix:(string_of_int control)
      [ make ~prefix:"device"
          [ node ~doc:"Resets the board"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "reset")
              ~query:Query.empty
              (Api_device.reset api)
          ; node ~doc:"Returns the state of the board"
              ~meth:`GET
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.get_state api)
          ; node ~doc:"Returns board description and capabilities, if available"
              ~meth:`GET
              ~path:(Path.Format.of_string "info")
              ~query:Query.empty
              (Api_device.get_info api)
          ; node ~doc:"Returns available tuner indexes"
              ~meth:`GET
              ~path:(Path.Format.of_string "receivers")
              ~query:Query.empty
              (Api_device.get_receivers api)
          ; node ~doc:"Returns the receiving mode of the requested tuner(s)"
              ~meth:`GET
              ~path:(Path.Format.of_string "mode")
              ~query:Query.["id", (module List(Int))]
              (Api_device.get_mode api)
          ]
      ; make ~prefix:"receiver"
          [ node ~doc:"Sets tuner receiving mode"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.(Int ^/ "mode" @/ empty)
              ~query:Query.empty
              (Api_receiver.set_mode api)
          ; node ~doc:"Returns the receiving mode of the requested tuner"
              ~meth:`GET
              ~path:Path.Format.(Int ^/ "mode" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_mode api)
          ; node ~doc:"Returns corresponding stream description, if any"
              ~meth:`GET
              ~path:Path.Format.(Int ^/ "stream" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_stream api)
          ; node ~doc:"Returns measurements for the specified receiver"
              ~meth:`GET
              ~path:Path.Format.(Int ^/ "measurements" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_measurements api)
          ; node ~doc:"Returns parameters for the specified receiver"
              ~meth:`GET
              ~path:Path.Format.(Int ^/ "parameters" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_parameters api)
          ; node ~doc:"Returns PLP list for the specified receiver"
              ~meth:`GET
              ~path:Path.Format.(Int ^/ "plp-list" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_plp_list api)
          ]
      ; make ~prefix:"stream"
          [ node ~doc:"Returns list of available streams"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_stream.get_streams api)
          ; node ~doc:"Returns stream description, if any"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ empty)
              ~query:Query.empty
              (Api_stream.get_stream api)
          ; node ~doc:"Returns measurements for the specified stream"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "measurements" @/ empty)
              ~query:Query.empty
              (Api_stream.get_measurements api)
          ; node ~doc:"Returns parameters for the specified stream"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "parameters" @/ empty)
              ~query:Query.empty
              (Api_stream.get_parameters api)
          ]
      ; make ~prefix:"history"
          [ node ~doc:"Returns measurement results for the specified time interval"
              ~meth:`GET
              ~path:(Path.Format.of_string "measurements")
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
  (* TODO add closing event *)
  let socket_table = make_socket_table () in
  
  [ merge ~prefix:(string_of_int control)
      [ make ~prefix:"device"
          [ node ~doc:"Device state socket"
              ~socket_table
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.Event.get_state api)
          ; node ~doc:"Tuner receiving mode socket"
              ~socket_table
              ~path:(Path.Format.of_string "mode")
              ~query:Query.["id", (module List(Int))]
              (Api_device.Event.get_mode api)
          ; node ~doc:"Available tuner indexes socket"
              ~socket_table
              ~path:(Path.Format.of_string "receivers")
              ~query:Query.empty
              (Api_device.Event.get_receivers api)
          ]
      ; make ~prefix:"receivers"
          [ node ~doc:"Receiver measurements socket"
              ~socket_table
              ~path:(Path.Format.of_string "measurements")
              ~query:Query.["id", (module List(Int))]
              (Api_receiver.Event.get_measurements api)
          ; node ~doc:"Receiver parameters socket"
              ~socket_table
              ~path:(Path.Format.of_string "parameters")
              ~query:Query.["id", (module List(Int))]
              (Api_receiver.Event.get_parameters api)
          ; node ~doc:"Receiver PLP list socket"
              ~socket_table
              ~path:(Path.Format.of_string "plp-list")
              ~query:Query.["id", (module List(Int))]
              (Api_receiver.Event.get_plp_list api)
          ]
      ; make ~prefix:"streams"
          [ node ~doc:"Streams socket"
              ~socket_table
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_stream.Event.get_streams api)
          ; node ~doc:"Stream measurements socket"
              ~socket_table
              ~path:(Path.Format.of_string "measurements")
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_stream.Event.get_measurements api)
          ; node ~doc:"Stream parameters socket"
              ~socket_table
              ~path:(Path.Format.of_string "parameters")
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_stream.Event.get_parameters api)
          ]
      ]
  ]
