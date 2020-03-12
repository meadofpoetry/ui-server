open Application_types
open Netlib.Uri
open Board_niitv_ts2ip_protocol
open Boards.Board

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [
    merge ~prefix:(string_of_int control)
      [
        make ~prefix:"device"
          [
            node ~doc:"Returns the state of the device" ~meth:`GET
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty (Api_device.get_state api);
            node ~doc:"Returns the status of the device" ~meth:`GET
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_device.get_status api);
            node
              ~doc:"Returns device description and capabilities, if available"
              ~meth:`GET
              ~path:(Path.Format.of_string "info")
              ~query:Query.[ ("force", (module Option (Bool))) ]
              (Api_device.get_info api);
            node ~doc:"Returns device configuration" ~meth:`GET
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_device.get_config api);
            node ~doc:"Returns device mode" ~meth:`GET
              ~path:(Path.Format.of_string "mode")
              ~query:Query.empty (Api_device.get_mode api);
            node ~doc:"Returns device network mode" ~meth:`GET
              ~path:(Path.Format.of_string "network")
              ~query:Query.empty
              (Api_device.get_network api);
            node ~doc:"Returns MAC address of the device" ~meth:`GET
              ~path:(Path.Format.of_string "mac-address")
              ~query:Query.empty (Api_device.get_mac api);
            node ~doc:"Returns IP address of the device" ~meth:`GET
              ~path:(Path.Format.of_string "ip-address")
              ~query:Query.empty
              (Api_device.get_ip_address api);
            node ~doc:"Returns subnet mask of the device" ~meth:`GET
              ~path:(Path.Format.of_string "subnet-mask")
              ~query:Query.empty (Api_device.get_mask api);
            node ~doc:"Returns gateway address of the device" ~meth:`GET
              ~path:(Path.Format.of_string "gateway")
              ~query:Query.empty
              (Api_device.get_gateway api);
            node ~doc:"Sets MAC address of the device" ~meth:`POST
              ~restrict:[ `Guest ]
              ~path:(Path.Format.of_string "mac-address")
              ~query:Query.[ ("force", (module Option (Bool))) ]
              (Api_device.set_mac api);
            node ~doc:"Sets IP address of the device" ~meth:`POST
              ~restrict:[ `Guest ]
              ~path:(Path.Format.of_string "ip-address")
              ~query:Query.empty
              (Api_device.set_ip_address api);
            node ~doc:"Sets subnet mask of the device" ~meth:`POST
              ~restrict:[ `Guest ]
              ~path:(Path.Format.of_string "subnet-mask")
              ~query:Query.empty (Api_device.set_mask api);
            node ~doc:"Sets gateway address of the device" ~meth:`POST
              ~restrict:[ `Guest ]
              ~path:(Path.Format.of_string "gateway")
              ~query:Query.empty
              (Api_device.set_gateway api);
            node ~doc:"Sets network mode" ~meth:`POST ~restrict:[ `Guest ]
              ~path:(Path.Format.of_string "network")
              ~query:Query.empty
              (Api_device.set_network api);
            node ~doc:"Sets mode" ~meth:`POST ~restrict:[ `Guest ]
              ~path:(Path.Format.of_string "mode")
              ~query:Query.empty (Api_device.set_mode api);
          ];
        make ~prefix:"streams"
          [
            node ~doc:"Available streams" ~meth:`GET ~path:Path.Format.empty
              ~query:Query.[ ("incoming", (module Option (Bool))) ]
              (Api_streams.get_streams api);
            node ~doc:"Stream description, if available" ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ empty)
              ~query:Query.[ ("incoming", (module Option (Bool))) ]
              (Api_streams.get_stream api);
          ];
        make ~prefix:"transmitter"
          [
            node ~doc:"UDP transmitter status" ~meth:`GET
              ~path:Path.Format.("status" @/ empty)
              ~query:Query.empty
              (Api_transmitter.get_status api);
            node ~doc:"Sets streams to transmit via UDP" ~meth:`POST
              ~restrict:[ `Guest ]
              ~path:Path.Format.("streams" @/ empty)
              ~query:Query.empty
              (Api_transmitter.set_streams api);
            node ~doc:"Sets UDP transmitter mode" ~meth:`POST
              ~restrict:[ `Guest ]
              ~path:Path.Format.("mode" @/ empty)
              ~query:Query.empty
              (Api_transmitter.set_mode api);
          ];
      ];
  ]

let ws (control : int) (api : Protocol.api) =
  let open Api_events in
  (* TODO add closing event *)
  [
    merge ~prefix:(string_of_int control)
      [
        make ~prefix:"device"
          [
            event_node ~doc:"Device state socket"
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty
              (Api_device.Event.get_state api);
            event_node ~doc:"Device config socket"
              ~path:(Path.Format.of_string "config")
              ~query:Query.empty
              (Api_device.Event.get_config api);
            event_node ~doc:"Device status socket"
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_device.Event.get_status api);
          ];
        make ~prefix:"streams"
          [
            event_node ~doc:"Streams socket" ~path:Path.Format.empty
              ~query:Query.[ ("incoming", (module Option (Bool))) ]
              (Api_streams.Event.get_streams api);
          ];
        make ~prefix:"transmitter"
          [
            event_node ~doc:"Transmitter status socket"
              ~path:(Path.Format.of_string "status")
              ~query:Query.empty
              (Api_transmitter.Event.get_status api);
            event_node ~doc:"Transmitters mode socket"
              ~path:(Path.Format.of_string "mode")
              ~query:Query.empty
              (Api_transmitter.Event.get_mode api);
          ];
      ];
  ]
