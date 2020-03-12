open Application_types
open Board_niitv_tsan_types

module Demo = struct
  let stream : Stream.t =
    { id = Option.get (Uuidm.of_string "d6db41ba-ec76-5666-a7d9-3fe4a3f39efb")
    ; source =
        { node = Entry (Input { input = ASI; id = 2 })
        ; info =
            IPV4
              { scheme = "udp"; addr = Ipaddr.V4.of_string_exn "224.1.2.2"; port = 1234 }
        }
    ; typ = TS
    ; orig_id = TS_raw
    }

  let aux_stream : Stream.t =
    { id = Option.get (Uuidm.of_string "d6db41ba-ec76-5666-a7d9-3fe4a3f39efa")
    ; source =
        { node = Entry (Input { input = ASI; id = 2 })
        ; info =
            IPV4
              { scheme = "udp"; addr = Ipaddr.V4.of_string_exn "224.1.2.3"; port = 1234 }
        }
    ; typ = TS
    ; orig_id = TS_raw
    }

  let different_input_stream : Stream.t =
    { id = Option.get (Uuidm.of_string "d6db41ba-ec76-5666-a7d9-3fe4a3f39efc")
    ; source =
        { node = Entry (Input { input = RF; id = 1 })
        ; info =
            IPV4
              { scheme = "udp"; addr = Ipaddr.V4.of_string_exn "224.1.2.3"; port = 1234 }
        }
    ; typ = TS
    ; orig_id = TS_raw
    }

  let streams = [ stream; aux_stream; different_input_stream ]

  let pid_1003 : PID.t =
    { has_pts = false
    ; has_pcr = true
    ; scrambled = false
    ; present = true
    ; service_id = Some 100
    ; service_name = Some "Первый канал"
    ; typ = PES { stream_type = 27; stream_id = 10 }
    }

  let pids =
    [ ( 1001
      , ({ has_pts = false
         ; has_pcr = true
         ; scrambled = false
         ; present = true
         ; service_id = Some 100
         ; service_name = Some "Первый канал"
         ; typ = PES { stream_type = 27; stream_id = 10 }
         }
          : PID.t) )
    ; ( 1002
      , ({ has_pts = true
         ; has_pcr = false
         ; scrambled = true
         ; present = false
         ; service_id = Some 100
         ; service_name = Some "Первый канал"
         ; typ = PES { stream_type = 27; stream_id = 10 }
         }
          : PID.t) )
    ]

  let services : (int * Service.t) list =
    [ ( 100
      , { Board_niitv_tsan_types.Service.name = "Первый канал"
        ; provider_name = "RTRS"
        ; pmt_pid = 1000
        ; pcr_pid = 4096
        ; has_pmt = true
        ; has_sdt = true
        ; dscr = true
        ; dscr_list = true
        ; eit_schedule = true
        ; eit_pf = true
        ; free_ca_mode = true
        ; running_status = 1
        ; service_type = 27
        ; service_type_list = 27
        ; elements = [ 1001; 1002 ]
        } )
    ; ( 101
      , { Board_niitv_tsan_types.Service.name = "Россия 1"
        ; provider_name = "RTRS"
        ; pmt_pid = 1001
        ; pcr_pid = 4097
        ; has_pmt = true
        ; has_sdt = true
        ; dscr = true
        ; dscr_list = true
        ; eit_schedule = true
        ; eit_pf = true
        ; free_ca_mode = true
        ; running_status = 1
        ; service_type = 27
        ; service_type_list = 27
        ; elements = [ 1003; 1004 ]
        } )
    ]

  let tables : (SI_PSI_table.id * SI_PSI_table.t) list =
    [ ( { table_id = 10
        ; table_id_ext = 10
        ; id_ext_1 = 0
        ; id_ext_2 = 0
        ; section_syntax = true
        }
      , { pid = 1234
        ; version = 20
        ; service_id = None
        ; service_name = None
        ; last_section = 2
        ; eit_segment_lsn = 0
        ; eit_last_table_id = 0
        ; sections = []
        } )
    ]

  let structure : Structure.t =
    { info =
        { complete = true
        ; services_num = List.length services
        ; nw_pid = 42
        ; ts_id = 24
        ; nw_id = 4242
        ; orig_nw_id = 2424
        ; nw_name = "foo"
        ; bouquet_name = "bar"
        }
    ; services
    ; tables
    ; pids
    ; timestamp = Ptime_clock.now ()
    }

  let structures = [ stream.id, structure ]

  (* let pids_ev =
   *   let s, set = React.S.create pids in
   *   let modify pids =
   *     let pid = 1003 in
   *     if List.mem_assoc pid pids
   *     then List.filter (fun (id, _) -> id <> pid) pids
   *     else (pid, pid_1003) :: pids
   *   in
   *   let rec aux () =
   *     Lwt.(
   *       Lwt_unix.sleep 1.0
   *       >>= fun () ->
   *       let pids = React.S.value s in
   *       Lwt.return
   *         (set
   *            (List.map (fun (s, (x : _ ts)) -> s, { x with data = modify x.data }) pids))
   *       >>= aux)
   *   in
   *   let _ = aux () in
   *   React.S.changes s *)

  let bitrate_ev =
    Lwt_react.(
      E.from (fun () ->
          Lwt.(
            Lwt_unix.sleep 1.0
            >>= fun () ->
            let pids_rate = List.map (fun (pid, _) -> pid, Random.int 5_000_000) pids in
            let total = List.fold_left (fun acc (_, x) -> acc + x) 0 pids_rate in
            let effective =
              List.fold_left
                (fun acc (pid, x) -> if x <> 0x1FFF then acc + x else acc)
                0
                pids_rate
            in
            Lwt.return
              [ ( stream.id
                , { Board_niitv_tsan_types.Bitrate.total
                  ; effective
                  ; tables = []
                  ; pids = pids_rate
                  ; services = []
                  ; timestamp = Ptime.epoch
                  } )
              ])))

  let error_ev =
    Lwt_react.(
      E.from (fun () ->
          Lwt.(
            Lwt_unix.sleep 1.0
            >>= fun () ->
            Lwt.return
              [ ( stream.id
                , [ { Board_niitv_tsan_types.Error.count = 1
                    ; err_code = 0x16
                    ; err_ext = 2
                    ; is_t2mi = false
                    ; multi_pid = false
                    ; pid = Random.int 200
                    ; packet = 0l
                    ; param_1 = 0l
                    ; param_2 = 0l
                    ; timestamp = Ptime_clock.now ()
                    }
                  ] )
              ])))
end

let create
    (src : Logs.src)
    (_sender : Cstruct.t -> unit Lwt.t)
    (_streams_conv : Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (kv : config Kv_v.rw) : (Protocol.api, _) Lwt_result.t =
  let state, set_state = React.S.create ~eq:Topology.equal_state `Fine in
  let status, set_status = React.E.create () in
  let structure, set_structure = React.S.create Demo.structures in
  let bitrate_queue = Bitrate_queue.create () in
  let bitrate = Demo.bitrate_ev in
  let bitrate = Protocol.map_bitrate bitrate structure in
  let streams = React.S.const Demo.streams in
  let bitrate_ext = React.E.map (Bitrate_queue.map bitrate_queue) bitrate in
  let notifs : Protocol.notifs =
    { state
    ; devinfo = React.S.const (Some { typ = 5; ver = 2 })
    ; deverr = React.E.never
    ; status
    ; streams
    ; bitrate
    ; bitrate_ext
    ; errors = Demo.error_ev
    ; t2mi_info = React.S.const []
    ; structure
    }
  in
  let loop () = fst @@ Lwt.wait () in
  Lwt.return_ok
    ({ notifs
     ; kv
     ; channel = (fun _ -> Lwt.return_error (Request.Custom "Sorry, this is demo board"))
     ; loop
     ; push_data = (fun _ -> ())
     ; bitrate_queue
     }
      : Protocol.api)
