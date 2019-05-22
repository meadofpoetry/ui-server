open Application_types
open Netlib.Uri
open Board_niitv_tsan_protocol

module Api_http = Api_cohttp.Make(User)(Body)

module Api_websocket = Api_websocket.Make(User)(Body)

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [ merge ~prefix:(string_of_int control)
      [ make ~prefix:"device"
          [ node ~doc:"Resets the device"
              ~meth:`POST
              ~path:(Path.Format.of_string "reset")
              ~query:Query.empty
              (Api_device.reset api)
          ; node ~doc:"Sets T2-MI monitoring mode of the device"
              ~meth:`POST
              ~path:(Path.Format.of_string "mode/t2mi")
              ~query:Query.empty
              (Api_device.set_t2mi_mode api)
          ; node ~doc:"Sets active input"
              ~meth:`POST
              ~path:(Path.Format.of_string "mode/input")
              ~query:Query.empty
              (Api_device.set_input api)
          ; node ~doc:"Sets port state"
              ~meth:`POST
              ~path:Path.Format.("mode/port" @/ Int ^/ empty)
              ~query:Query.empty
              (Api_device.set_port api)
          ; node ~doc:"Returns current state of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.get_state api)
          ; node ~doc:"Returns device description"
              ~meth:`GET
              ~path:(Path.Format.of_string "info")
              ~query:Query.["force", (module Option(Bool))]
              (Api_device.get_info api)
          ; node ~doc:"Returns device errors, if any"
              ~meth:`GET
              ~path:(Path.Format.of_string "errors")
              ~query:Query.[ "timeout", (module Option(Int))
                           ; "force", (module Option(Bool)) ]
              (Api_device.get_errors api)
          ; node ~doc:"Returns status of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_device.get_status api)
          ; node ~doc:"Returns currently selected input"
              ~meth:`GET
              ~path:(Path.Format.of_string "input")
              ~query:Query.empty
              (Api_device.get_input api)
          ; node ~doc:"Returns current T2-MI monitoring mode"
              ~meth:`GET
              ~path:(Path.Format.of_string "mode/t2mi")
              ~query:Query.["force", (module Option(Bool))]
              (Api_device.get_t2mi_mode api)
          ; node ~doc:"Returns current PCR jitter monitoring mode"
              ~meth:`GET
              ~path:(Path.Format.of_string "mode/jitter")
              ~query:Query.["force", (module Option(Bool))]
              (Api_device.get_jitter_mode api)
          ]
      ; make ~prefix:"monitoring"
          [ node ~doc:"Returns TS & T2-MI errors, if any"
              ~meth:`GET
              ~path:(Path.Format.of_string "errors")
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "timeout", (module Option(Int)) ]
              (Api_monitoring.get_errors api)
          ; node ~doc:"Returns list of available TS info"
              ~meth:`GET
              ~path:(Path.Format.of_string "ts-info")
              ~query:Query.[ "force", (module Option(Bool))
                           ; "id", (module List(Stream.ID)) ]
              (Api_monitoring.get_ts_info api)
          ; node ~doc:"Returns current bitrate"
              ~meth:`GET
              ~path:(Path.Format.of_string "bitrate")
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "timeout", (module Option(Int)) ]
              (Api_monitoring.get_bitrate api)
          ; node ~doc:"Returns available services"
              ~meth:`GET
              ~path:(Path.Format.of_string "services")
              ~query:Query.[ "force", (module Option(Bool))
                           ; "id", (module List(Stream.ID)) ]
              (Api_monitoring.get_services api)
          ; node ~doc:"Returns available PIDs"
              ~meth:`GET
              ~path:(Path.Format.of_string "pids")
              ~query:Query.[ "force", (module Option(Bool))
                           ; "id", (module List(Stream.ID)) ]
              (Api_monitoring.get_pids api)
          ; node ~doc:"Returns available SI/PSI tables"
              ~meth:`GET
              ~path:(Path.Format.of_string "tables")
              ~query:Query.[ "force", (module Option(Bool))
                           ; "id", (module List(Stream.ID)) ]
              (Api_monitoring.get_si_psi_tables api)
          ; node ~doc:"Returns available T2-MI info"
              ~meth:`GET
              ~path:(Path.Format.of_string "t2mi-info")
              ~query:Query.[ "force", (module Option(Bool))
                           ; "id", (module List(Stream.ID))
                           ; "t2mi-stream-id", (module List(Int))]
              (Api_monitoring.get_t2mi_info api)
          ]
      ; make ~prefix:"streams"
          [ node ~doc:"Returns current streams"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Api_streams.get_streams api)
          ; node ~doc:"Returns stream details"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ empty)
              ~query:Query.empty
              (Api_streams.get_stream api)
          ; node ~doc:"Returns stream bitrate"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "bitrate" @/ empty)
              ~query:Query.empty
              (Api_streams.get_bitrate api)
          ; node ~doc:"Returns TS details"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "ts-info" @/ empty)
              ~query:Query.["force", (module Option(Bool))]
              (Api_streams.get_ts_info api)
          ; node ~doc:"Returns PID list"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "pids" @/ empty)
              ~query:Query.["force", (module Option(Bool))]
              (Api_streams.get_pids api)
          ; node ~doc:"Returns available SI/PSI tables"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "tables" @/ empty)
              ~query:Query.empty
              (Api_streams.get_si_psi_tables api)
          ; node ~doc:"Returns available services"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "services" @/ empty)
              ~query:Query.empty
              (Api_streams.get_services api)
          ; node ~doc:"Returns T2-MI details"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "t2mi-info" @/ empty)
              ~query:Query.empty
              (Api_streams.get_t2mi_info api)
          ; node ~doc:"Returns T2-MI packet sequence"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "t2mi-sequence" @/ empty)
              ~query:Query.[ "duration", (module Option(Int))
                           ; "t2mi-stream-id", (module List(Int)) ]
              (Api_streams.get_t2mi_sequence api)
          ; node ~doc:"Returns SI/PSI section"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt
                                 ^/ "section"
                                 @/ Int
                                 ^/ empty)
              ~query:Query.[ "section", (module Option(Int))
                           ; "table-id-ext", (module Option(Int))
                           ; "id-ext-1", (module Option(Int))
                           ; "id-ext-2", (module Option(Int)) ]
              (Api_streams.get_section api)
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
          ; node ~doc:"Active input socket"
              ~socket_table
              ~path:(Path.Format.of_string "input")
              ~query:Query.empty
              (Api_device.Event.get_input api)
          ; node ~doc:"Device status socket"
              ~socket_table
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_device.Event.get_status api)
          ; node ~doc:"Device info socket"
              ~socket_table
              ~path:(Path.Format.of_string "info")
              ~query:Query.empty
              (Api_device.Event.get_info api)
          ; node ~doc:"Device errors socket"
              ~socket_table
              ~path:(Path.Format.of_string "errors")
              ~query:Query.empty
              (Api_device.Event.get_errors api)
          ; node ~doc:"T2-MI mode socket"
              ~socket_table
              ~path:(Path.Format.of_string "mode/t2mi")
              ~query:Query.empty
              (Api_device.Event.get_t2mi_mode api)
          ]
      ]
  ]
