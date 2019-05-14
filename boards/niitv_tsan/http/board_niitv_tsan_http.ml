open Application_types
open Netlib.Uri
open Board_niitv_tsan_protocol

module Api_http = Api_cohttp.Make(User)(Body)

module Api_websocket = Api_websocket.Make(User)(Body)

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [ merge ~prefix:(Topology.get_api_path control)
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
      ; make ~prefix:"measurements"
          [ node ~doc:"Returns list of available TS info"
              ~meth:`GET
              ~path:(Path.Format.of_string "ts-info")
              ~query:Query.empty
              (Api_measurements.get_ts_info api)
          ; node ~doc:"Returns current bitrate"
              ~meth:`GET
              ~path:(Path.Format.of_string "bitrate")
              ~query:Query.empty
              (Api_measurements.get_bitrate api)
          ; node ~doc:"Returns available services"
              ~meth:`GET
              ~path:(Path.Format.of_string "services")
              ~query:Query.empty
              (Api_measurements.get_services api)
          ; node ~doc:"Returns available PIDs"
              ~meth:`GET
              ~path:(Path.Format.of_string "pids")
              ~query:Query.empty
              (Api_measurements.get_pids api)
          ; node ~doc:"Returns available SI/PSI tables"
              ~meth:`GET
              ~path:(Path.Format.of_string "tables")
              ~query:Query.empty
              (Api_measurements.get_si_psi_tables api)
          ; node ~doc:"Returns available T2-MI info"
              ~meth:`GET
              ~path:(Path.Format.of_string "t2mi-info")
              ~query:Query.empty
              (Api_measurements.get_t2mi_info api)
          ]
      ; make ~prefix:"stream"
          [ node ~doc:"Returns current streams"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.empty
              (Api_stream.get_streams api)
          ; node ~doc:"Returns stream details"
              ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ empty)
              ~query:Query.empty
              (Api_stream.get_stream api)
          ; node ~doc:"Returns SI/PSI section"
              ~meth:`GET
              ~path:Path.Format.("si-psi-section"
                                 @/ Stream.ID.fmt
                                 ^/ Int
                                 ^/ empty)
              ~query:Query.[ "section", (module Option(Int))
                           ; "table-id-ext", (module Option(Int))
                           ; "id-ext-1", (module Option(Int))
                           ; "id-ext-2", (module Option(Int)) ]
              (Api_stream.get_section api)
          ]
      ]
  ]
