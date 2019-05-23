open Application_types
open Netlib.Uri
open Board_dektec_dtm3200_protocol

module Api_http = Api_cohttp.Make(User)(Body)

module Api_websocket = Api_websocket.Make(User)(Body)

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [ merge ~prefix:(string_of_int control)
      [ make ~prefix:"device"
          [ node ~doc:"State of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.get_state api)
          ; node ~doc:"Integrated device description"
              ~meth:`GET
              ~path:(Path.Format.of_string "info")
              ~query:Query.empty
              (Api_device.get_info api)
          ; node ~doc:"Device configuration"
              ~meth:`GET
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_device.get_config api)
          ; node ~doc:"Version number of the FPGA code on-board of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "fpga-version")
              ~query:Query.empty
              (Api_device.get_fpga_version api)
          ; node ~doc:"Hardware version number"
              ~meth:`GET
              ~path:(Path.Format.of_string "hardware-version")
              ~query:Query.empty
              (Api_device.get_hardware_version api)
          ; node ~doc:"Firmware version: the major version is encoded in the tens, \
                       the minor version in the units, e.g. `10` indicates v1.0"
              ~meth:`GET
              ~path:(Path.Format.of_string "firmware-version")
              ~query:Query.empty
              (Api_device.get_firmware_version api)
          ; node ~doc:"Unique serial number for the device, e.g. 3200.000.027"
              ~meth:`GET
              ~path:(Path.Format.of_string "serial-number")
              ~query:Query.empty
              (Api_device.get_serial_number api)
          ; node ~doc:"Device type number, e.g. 3200"
              ~meth:`GET
              ~path:(Path.Format.of_string "type")
              ~query:Query.empty
              (Api_device.get_type api)
          ]
      ; make ~prefix:"network"
          [ node ~doc:"Resets the board"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "reboot")
              ~query:Query.empty
              (Api_device.reboot api)
          ; node ~doc:"Integrated network configuration"
              ~meth:`GET
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_network.get_config api)
          ; node ~doc:"IP address of the device"
              ~meth:`GET
              ~path:(Path.Format.of_string "ip-address")
              ~query:Query.empty
              (Api_network.get_ip_address api)
          ; node ~doc:"IP address of the device"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "ip-address")
              ~query:Query.empty
              (Api_network.set_ip_address api)
          ; node ~doc:"Subnet mask"
              ~meth:`GET
              ~path:(Path.Format.of_string "subnet-mask")
              ~query:Query.empty
              (Api_network.get_subnet_mask api)
          ; node ~doc:"Subnet mask"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "subnet-mask")
              ~query:Query.empty
              (Api_network.set_subnet_mask api)
          ; node ~doc:"Gateway"
              ~meth:`GET
              ~path:(Path.Format.of_string "gateway")
              ~query:Query.empty
              (Api_network.get_gateway api)
          ; node ~doc:"Gateway"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "gateway")
              ~query:Query.empty
              (Api_network.set_gateway api)
          ; node ~doc:"DHCP"
              ~meth:`GET
              ~path:(Path.Format.of_string "dhcp")
              ~query:Query.empty
              (Api_network.get_dhcp api)
          ; node ~doc:"DHCP"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "dhcp")
              ~query:Query.empty
              (Api_network.set_dhcp api)
          ]
      ; make ~prefix:"ip-receiver"
          [ node ~doc:"Returns addressing (Unicast or any-source multicast)"
              ~meth:`GET
              ~path:(Path.Format.of_string "addressing-method")
              ~query:Query.empty
              (Api_ip_receiver.get_addressing_method api)
          ; node ~doc:"Sets addressing method as Unicast or any-source multicast"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "addressing-method")
              ~query:Query.empty
              (Api_ip_receiver.set_addressing_method api)
          ; node ~doc:"Returns if IP-to-ASI conversion is enabled or disabled"
              ~meth:`GET
              ~path:(Path.Format.of_string "enabled")
              ~query:Query.empty
              (Api_ip_receiver.get_enable api)
          ; node ~doc:"Enables or disables IP-to-ASI conversion"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "enabled")
              ~query:Query.empty
              (Api_ip_receiver.set_enable api)
          ; node ~doc:"FEC delay in milliseconds"
              ~meth:`GET
              ~path:(Path.Format.of_string "fec/delay")
              ~query:Query.empty
              (Api_ip_receiver.get_fec_delay api)
          ; node ~doc:"Returns if FEC encoding is disabled or enabled"
              ~meth:`GET
              ~path:(Path.Format.of_string "fec/enabled")
              ~query:Query.empty
              (Api_ip_receiver.get_fec_enable api)
          ; node ~doc:"Enables or disables FEC encoding"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "fec/enabled")
              ~query:Query.empty
              (Api_ip_receiver.set_fec_enable api)
          ; node ~doc:"#FEC columns"
              ~meth:`GET
              ~path:(Path.Format.of_string "fec/columns")
              ~query:Query.empty
              (Api_ip_receiver.get_fec_columns api)
          ; node ~doc:"#FEC rows"
              ~meth:`GET
              ~path:(Path.Format.of_string "fec/rows")
              ~query:Query.empty
              (Api_ip_receiver.get_fec_rows api)
          ; node ~doc:"The time (in ms) that the device waits to receive 'late' IP \
                       packets (1..120ms)"
              ~meth:`GET
              ~path:(Path.Format.of_string "ip-jitter-tolerance")
              ~query:Query.empty
              (Api_ip_receiver.get_ip_jitter_tolerance api)
          ; node ~doc:"Number of packets lost after FEC reconstruction"
              ~meth:`GET
              ~path:(Path.Format.of_string "ip-lost-after-fec")
              ~query:Query.empty
              (Api_ip_receiver.get_ip_lost_after_fec api)
          ; node ~doc:"Number of packets lost before FEC reconstruction"
              ~meth:`GET
              ~path:(Path.Format.of_string "ip-lost-before-fec")
              ~query:Query.empty
              (Api_ip_receiver.get_ip_lost_before_fec api)
          ; node ~doc:"UDP port number at which the device listens \
                       for the incoming TS"
              ~meth:`GET
              ~path:(Path.Format.of_string "upd-port")
              ~query:Query.empty
              (Api_ip_receiver.get_udp_port api)
          ; node ~doc:"UDP port number at which the device listens \
                       for the incoming TS"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "udp-port")
              ~query:Query.empty
              (Api_ip_receiver.set_udp_port api)
          ; node ~doc:"Delay (ms) from IP input to ASI output in milliseconds"
              ~meth:`GET
              ~path:(Path.Format.of_string "ip-to-output-delay")
              ~query:Query.empty
              (Api_ip_receiver.get_ip_to_output_delay api)
          ; node ~doc:"Delay (ms) from IP input to ASI output in milliseconds"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "ip-to-output-delay")
              ~query:Query.empty
              (Api_ip_receiver.set_ip_to_output_delay api)
          ; node ~doc:"Multicast address at which the device listens \
                       for the incoming TS"
              ~meth:`GET
              ~path:(Path.Format.of_string "multicast-address")
              ~query:Query.empty
              (Api_ip_receiver.get_multicast_address api)
          ; node ~doc:"Multicast address at which the device listens \
                       for the incoming TS"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "multicast-address")
              ~query:Query.empty
              (Api_ip_receiver.set_multicast_address api)
          ; node ~doc:"Number of transport packets per IP packet"
              ~meth:`GET
              ~path:(Path.Format.of_string "tp-per-ip")
              ~query:Query.empty
              (Api_ip_receiver.get_tp_per_ip api)
          ; node ~doc:"Current operational status"
              ~meth:`GET
              ~path:(Path.Format.of_string "receiver-status")
              ~query:Query.empty
              (Api_ip_receiver.get_receiver_status api)
          ; node ~doc:"Protocol used by the incoming IP stream"
              ~meth:`GET
              ~path:(Path.Format.of_string "protocol")
              ~query:Query.empty
              (Api_ip_receiver.get_protocol api)
          ; node ~doc:"Channel index; always 1 for the DTM-3200 (because it \
                       supports a single channel only)"
              ~meth:`GET
              ~path:(Path.Format.of_string "index")
              ~query:Query.empty
              (Api_ip_receiver.get_index api)
          ; node ~doc:"Identifies the type of output interface"
              ~meth:`GET
              ~path:(Path.Format.of_string "output-type")
              ~query:Query.empty
              (Api_ip_receiver.get_output_type api)
          ; node ~doc:"Size of transport stream packet (188 or 204)"
              ~meth:`GET
              ~path:(Path.Format.of_string "packet-size")
              ~query:Query.empty
              (Api_ip_receiver.get_packet_size api)
          ; node ~doc:"Estimated bitrate (in bps@188) of the incoming TS"
              ~meth:`GET
              ~path:(Path.Format.of_string "bitrate")
              ~query:Query.empty
              (Api_ip_receiver.get_bitrate api)
          ; node ~doc:"Returns if PCRs are present in the incoming TS"
              ~meth:`GET
              ~path:(Path.Format.of_string "pcr-present")
              ~query:Query.empty
              (Api_ip_receiver.get_pcr_present api)
          ; node ~doc:"Counter that keeps the number of bitrate changes \
                       detected on this channel. During normal operation this \
                       counter should remain constant"
              ~meth:`GET
              ~path:(Path.Format.of_string "rate-change-counter")
              ~query:Query.empty
              (Api_ip_receiver.get_rate_change_counter api)
          ; node ~doc:"Returns rate estimation mode"
              ~meth:`GET
              ~path:(Path.Format.of_string "rate-estimation-mode")
              ~query:Query.empty
              (Api_ip_receiver.get_rate_estimation_mode api)
          ; node ~doc:"Sets rate estimation mode"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:(Path.Format.of_string "rate-estimation-mode")
              ~query:Query.empty
              (Api_ip_receiver.get_rate_estimation_mode api)
          ; node ~doc:"Counter that keeps the number of IP jitter errors \
                       for this channel. A jitter error occurs when an incoming \
                       IP packet has a jitter that exceeds the Jitter Tolerance. \
                       During normal operation this counter should remain constant"
              ~meth:`GET
              ~path:(Path.Format.of_string "jitter-error-counter")
              ~query:Query.empty
              (Api_ip_receiver.get_jitter_error_counter api)
          ; node ~doc:"Counter that keeps the number of channel restarts. \
                       A channel is restarted only under adverse circumstances, like \
                       large jitter of IP packets in combination with packet loss. \
                       A channel restart causes a signal interruption for this channel \
                       of about 1 second. During normal operation this counter should \
                       remain constant"
              ~meth:`GET
              ~path:(Path.Format.of_string "lock-error-counter")
              ~query:Query.empty
              (Api_ip_receiver.get_lock_error_counter api)
          ; node ~doc:"The channel's delay factor in us. Delay factor is a measure \
                       of the maximum jitter on the IP packets received for this \
                       channel"
              ~meth:`GET
              ~path:(Path.Format.of_string "delay-factor")
              ~query:Query.empty
              (Api_ip_receiver.get_delay_factor api)
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
              (Api_device.Event.get_info api)
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
      ; make ~prefix:"ip-receiver"
          [ node ~doc:"IP receiver configuration socket"
              ~socket_table
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_ip_receiver.Event.get_config api)
          ; node ~doc:"Integrated status of the device"
              ~socket_table
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_ip_receiver.Event.get_status api)
          ]
      ]
  ]
