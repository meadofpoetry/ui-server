open Application_types
open Netlib.Uri
open Board_dektec_dtm3200_protocol

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
              (Api_device.reboot api)
          ; node ~doc:"Returns the state of the board"
              ~meth:`GET
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.get_state api)
          ; node ~doc:"Returns board description, if available"
              ~meth:`GET
              ~path:(Path.Format.of_string "info")
              ~query:Query.empty
              (Api_device.get_devinfo api)
          ; node ~doc:"Returns board configuration"
              ~meth:`GET
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_device.get_config api)
          ]
      ; make ~prefix:"network"
          [ node ~doc:"Returns network config"
              ~meth:`GET
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_network.get_config api)
          ; node ~doc:"Returns IP address of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "ip-address")
              ~query:Query.empty
              (Api_network.get_ip_address api)
          ; node ~doc:"Returns subnet mask"
              ~meth:`GET
              ~path:(Path.Format.of_string "subnet-mask")
              ~query:Query.empty
              (Api_network.get_subnet_mask api)
          ; node ~doc:"Returns gateway"
              ~meth:`GET
              ~path:(Path.Format.of_string "gateway")
              ~query:Query.empty
              (Api_network.get_gateway api)
          ; node ~doc:"Returns DHCP"
              ~meth:`GET
              ~path:(Path.Format.of_string "dhcp")
              ~query:Query.empty
              (Api_network.get_dhcp api)
          ; node ~doc:"Sets IP address of the device"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "ip-address")
              ~query:Query.empty
              (Api_network.set_ip_address api)
          ; node ~doc:"Sets subnet mask"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "subnet-mask")
              ~query:Query.empty
              (Api_network.set_subnet_mask api)
          ; node ~doc:"Sets gateway"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "gateway")
              ~query:Query.empty
              (Api_network.set_gateway api)
          ; node ~doc:"Sets DHCP"
              ~meth:`POST
              ~path:(Path.Format.of_string "dhcp")
              ~query:Query.empty
              (Api_network.set_dhcp api)
          ]
      ; make ~prefix:"receiver"
          [ node ~doc:"Returns addressing method"
              ~meth:`GET
              ~path:(Path.Format.of_string "addressing-method")
              ~query:Query.empty
              (Api_receiver.get_addressing_method api)
          ; node ~doc:"Returns [true] if the receiver is enabled"
              ~meth:`GET
              ~path:(Path.Format.of_string "enable")
              ~query:Query.empty
              (Api_receiver.get_enable api)
          ; node ~doc:"Returns FEC delay"
              ~meth:`GET
              ~path:(Path.Format.of_string "fec-delay")
              ~query:Query.empty
              (Api_receiver.get_fec_delay api)
          ]
      ]
  ]

let ws (control : int) (api : Protocol.api) =
  let open Api_http in
  let open Api_websocket in
  let socket_table = make_socket_table () in
  [ merge ~prefix:(string_of_int control)
      [ make ~prefix:"device"
          [ node ~doc:"Device state socket"
              ~socket_table
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.Event.get_state api)
          ; node ~doc:"Device info socket"
              ~socket_table
              ~path:(Path.Format.of_string "info")
              ~query:Query.empty
              (Api_device.Event.get_devinfo api)
          ; node ~doc:"Device configuration socket"
              ~socket_table
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_device.Event.get_config api)
          ]
      ; make ~prefix:"network"
          [ node ~doc:"Network configuration socket"
              ~socket_table
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_network.Event.get_config api)
          ]
      ]
  ]
