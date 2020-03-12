open Application_types
open Netlib.Uri
open Board_niitv_dvb4ch_protocol
open Boards.Board

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [
    merge ~prefix:(string_of_int control)
      [
        make ~prefix:"device"
          [
            node ~doc:"Resets the board" ~restrict:[ `Guest ] ~meth:`POST
              ~path:(Path.Format.of_string "reset")
              ~query:Query.empty (Api_device.reset api);
            node ~doc:"Device state" ~meth:`GET
              ~path:(Path.Format.of_string "state")
              ~query:Query.empty (Api_device.get_state api);
            node ~doc:"Description and capabilities of the device" ~meth:`GET
              ~path:(Path.Format.of_string "info")
              ~query:Query.[ ("force", (module Option (Bool))) ]
              (Api_device.get_info api);
            node ~doc:"Indexes of installed tuners" ~meth:`GET
              ~path:(Path.Format.of_string "receivers")
              ~query:Query.empty
              (Api_device.get_receivers api);
            node ~doc:"Receiving mode of the requested tuner(s)" ~meth:`GET
              ~path:(Path.Format.of_string "mode")
              ~query:Query.[ ("id", (module List (Int))) ]
              (Api_device.get_mode api);
          ];
        make ~prefix:"receivers"
          [
            node ~doc:"Sets tuner receiving mode" ~restrict:[ `Guest ]
              ~meth:`POST
              ~path:Path.Format.(Int ^/ "mode" @/ empty)
              ~query:Query.empty
              (Api_receiver.set_mode api);
            node ~doc:"Returns the receiving mode of the requested tuner"
              ~meth:`GET
              ~path:Path.Format.(Int ^/ "mode" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_mode api);
            node ~doc:"Returns corresponding stream description, if any"
              ~meth:`GET
              ~path:Path.Format.(Int ^/ "stream" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_stream api);
            node ~doc:"Returns measurements for the specified receiver"
              ~meth:`GET
              ~path:Path.Format.(Int ^/ "measurements" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_measurements api);
            node ~doc:"Returns parameters for the specified receiver" ~meth:`GET
              ~path:Path.Format.(Int ^/ "parameters" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_parameters api);
            node ~doc:"Returns PLP list for the specified receiver" ~meth:`GET
              ~path:Path.Format.(Int ^/ "plp-list" @/ empty)
              ~query:Query.empty
              (Api_receiver.get_plp_list api);
          ];
        make ~prefix:"streams"
          [
            node ~doc:"Returns list of available streams" ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.[ ("id", (module List (Stream.ID))) ]
              (Api_stream.get_streams api);
            node ~doc:"Returns stream description, if any" ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ empty)
              ~query:Query.empty
              (Api_stream.get_stream api);
            node ~doc:"Returns measurements for the specified stream" ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "measurements" @/ empty)
              ~query:Query.empty
              (Api_stream.get_measurements api);
            node ~doc:"Returns parameters for the specified stream" ~meth:`GET
              ~path:Path.Format.(Stream.ID.fmt ^/ "parameters" @/ empty)
              ~query:Query.empty
              (Api_stream.get_parameters api);
          ];
        make ~prefix:"history"
          [
            node
              ~doc:"Returns measurement results for the specified time interval"
              ~meth:`GET
              ~path:(Path.Format.of_string "measurements")
              ~query:
                Query.
                  [
                    ("id", (module List (Int)));
                    ("limit", (module Option (Int)));
                    ("from", (module Option (Time_uri.Show)));
                    ("to", (module Option (Time_uri.Show)));
                    ("duration", (module Option (Time_uri.Show_relative)));
                  ]
              (Api_history.get_measurements api);
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
            event_node ~doc:"Tuner receiving mode socket"
              ~path:(Path.Format.of_string "mode")
              ~query:Query.[ ("id", (module List (Int))) ]
              (Api_device.Event.get_mode api);
          ];
        make ~prefix:"receivers"
          [
            event_node ~doc:"Receiver measurements socket"
              ~path:(Path.Format.of_string "measurements")
              ~query:Query.[ ("id", (module List (Int))) ]
              (Api_receiver.Event.get_measurements api);
            event_node ~doc:"Receiver parameters socket"
              ~path:(Path.Format.of_string "parameters")
              ~query:Query.[ ("id", (module List (Int))) ]
              (Api_receiver.Event.get_parameters api);
            event_node ~doc:"Receiver PLP list socket"
              ~path:(Path.Format.of_string "plp-list")
              ~query:Query.[ ("id", (module List (Int))) ]
              (Api_receiver.Event.get_plp_list api);
          ];
        make ~prefix:"streams"
          [
            event_node ~doc:"Streams socket" ~path:Path.Format.empty
              ~query:Query.[ ("id", (module List (Stream.ID))) ]
              (Api_stream.Event.get_streams api);
            event_node ~doc:"Stream measurements socket"
              ~path:(Path.Format.of_string "measurements")
              ~query:Query.[ ("id", (module List (Stream.ID))) ]
              (Api_stream.Event.get_measurements api);
            event_node ~doc:"Stream parameters socket"
              ~path:(Path.Format.of_string "parameters")
              ~query:Query.[ ("id", (module List (Stream.ID))) ]
              (Api_stream.Event.get_parameters api);
          ];
      ];
  ]
