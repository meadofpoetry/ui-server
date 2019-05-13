open Application_types
open Netlib.Uri
open Board_niitv_ts2ip_protocol

module Api_http = Api_cohttp.Make(User)(Body)

module Api_websocket = Api_websocket.Make(User)(Body)

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  
  [ merge ~prefix:(Topology.get_api_path control)
      [ make ~prefix:"device"
          [ node ~doc:"Returns the state of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.get_state api)
          ; node ~doc:"Returns the status of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_device.get_status api)
          ; node ~doc:"Returns device description and capabilities, if available"
              ~meth:`GET
              ~path:(Path.Format.of_string "info")
              ~query:Query.["force", (module Option(Bool))]
              (Api_device.get_info api)
          ; node ~doc:"Returns device configuration"
              ~meth:`GET
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_device.get_config api)
          ; node ~doc:"Returns device mode"
              ~meth:`GET
              ~path:(Path.Format.of_string "mode")
              ~query:Query.empty
              (Api_device.get_mode api)
          ; node ~doc:"Returns device network mode"
              ~meth:`GET
              ~path:(Path.Format.of_string "network")
              ~query:Query.empty
              (Api_device.get_network api)
          ; node ~doc:"Returns MAC address of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "mac-address")
              ~query:Query.empty
              (Api_device.get_mac api)
          ; node ~doc:"Returns IP address of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "ip-address")
              ~query:Query.empty
              (Api_device.get_ip_address api)
          ; node ~doc:"Returns subnet mask of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "subnet-mask")
              ~query:Query.empty
              (Api_device.get_mask api)
          ; node ~doc:"Returns gateway address of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "gateway")
              ~query:Query.empty
              (Api_device.get_gateway api)
          ; node ~doc:"Sets MAC address of the device"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:(Path.Format.of_string "mac-address")
              ~query:Query.["force", (module Option(Bool))]
              (Api_device.set_mac api)
          ; node ~doc:"Sets IP address of the device"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:(Path.Format.of_string "ip-address")
              ~query:Query.empty
              (Api_device.set_ip_address api)
          ; node ~doc:"Sets subnet mask of the device"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:(Path.Format.of_string "subnet-mask")
              ~query:Query.empty
              (Api_device.set_mask api)
          ; node ~doc:"Sets gateway address of the device"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:(Path.Format.of_string "gateway")
              ~query:Query.empty
              (Api_device.set_gateway api)
          ; node ~doc:"Sets network mode"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:(Path.Format.of_string "network")
              ~query:Query.empty
              (Api_device.set_network api)
          ; node ~doc:"Sets mode"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:(Path.Format.of_string "mode")
              ~query:Query.empty
              (Api_device.set_mode api)
          ]
      ; make ~prefix:"transmitter"
          [ node ~doc:"Returns UDP transmitters status"
              ~meth:`GET
              ~path:Path.Format.("status" @/ empty)
              ~query:Query.empty
              (Api_transmitter.get_status api)
          ; node ~doc:"Returns incoming streams"
              ~meth:`GET
              ~path:Path.Format.("streams/incoming" @/ empty)
              ~query:Query.empty
              (Api_transmitter.get_incoming_streams api)
          ; node ~doc:"Returns outgoing streams"
              ~meth:`GET
              ~path:Path.Format.("streams/outgoing" @/ empty)
              ~query:Query.empty
              (Api_transmitter.get_outgoing_streams api)
          ; node ~doc:"Sets streams to transmit via UDP"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:Path.Format.("streams" @/ empty)
              ~query:Query.empty
              (Api_transmitter.set_streams api)
          ; node ~doc:"Sets UDP transmitter mode"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:Path.Format.("mode" @/ empty)
              ~query:Query.empty
              (Api_transmitter.set_mode api)
          ]
      ]
  ]

let ws (control : int) (api : Protocol.api) =
  let open Api_http in
  let open Api_websocket in
  (* TODO add closing event *)
  let socket_table = make_socket_table () in
  
  [ merge ~prefix:(Topology.get_api_path control)
      [ make ~prefix:"device"
          [ node ~doc:"Device state socket"
              ~socket_table
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.Event.get_state api)
          ; node ~doc:"Device config socket"
              ~socket_table
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_device.Event.get_config api)
          ; node ~doc:"Device mode socket"
              ~socket_table
              ~path:(Path.Format.of_string "mode")
              ~query:Query.empty
              (Api_device.Event.get_mode api)
          ; node ~doc:"Device network mode socket"
              ~socket_table
              ~path:(Path.Format.of_string "network")
              ~query:Query.empty
              (Api_device.Event.get_network_mode api)
          ; node ~doc:"Device status socket"
              ~socket_table
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_device.Event.get_status api)
          ]
      ; make ~prefix:"transmitter"
          [ node ~doc:"Transmitter status socket"
              ~socket_table
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_transmitter.Event.get_status api)
          ; node ~doc:"Incoming streams socket"
              ~socket_table
              ~path:(Path.Format.of_string "streams/incoming")
              ~query:Query.empty
              (Api_transmitter.Event.get_incoming_streams api)
          ; node ~doc:"Outgoing streams socket"
              ~socket_table
              ~path:(Path.Format.of_string "streams/outgoing")
              ~query:Query.empty
              (Api_transmitter.Event.get_outgoing_streams api)
          ; node ~doc:"Transmitters mode socket"
              ~socket_table
              ~path:(Path.Format.of_string "mode")
              ~query:Query.empty
              (Api_transmitter.Event.get_mode api)
          ]
      ]
  ]
