open Fsm_common

let ( >>= ) = Lwt.bind

let step ~(address : int)
    ~(return : unit -> unit Lwt.t)
    ~continue
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (rsp_queue : Cstruct.t Request.cmd Lwt_stream.t) =

  let rec fec_delay () =
    request ~address src sender rsp_queue (IP_receive FEC_delay)
    >>= function
    | Error _ -> return ()
    | Ok x -> fec_columns GList.(x :: [])

  and fec_columns acc =
    request ~address src sender rsp_queue (IP_receive FEC_columns)
    >>= function
    | Error _ -> return ()
    | Ok x -> fec_rows GList.(x :: acc)

  and fec_rows acc =
    request ~address src sender rsp_queue (IP_receive FEC_rows)
    >>= function
    | Error _ -> return ()
    | Ok x -> jitter_tolerance GList.(x :: acc)

  and jitter_tolerance acc =
    request ~address src sender rsp_queue (IP_receive IP_jitter_tolerance)
    >>= function
    | Error _ -> return ()
    | Ok x -> bitrate GList.(x :: acc)

  and bitrate acc =
    request ~address src sender rsp_queue (IP_receive Bitrate)
    >>= function
    | Error _ -> return ()
    | Ok x -> lost_after_fec GList.(x :: acc)

  and lost_after_fec acc =
    request ~address src sender rsp_queue (IP_receive IP_lost_after_FEC)
    >>= function
    | Error _ -> return ()
    | Ok x -> lost_before_fec GList.(x :: acc)

  and lost_before_fec acc =
    request ~address src sender rsp_queue (IP_receive IP_lost_before_FEC)
    >>= function
    | Error _ -> return ()
    | Ok x -> tp_per_ip GList.(x :: acc)

  and tp_per_ip acc =
    request ~address src sender rsp_queue (IP_receive TP_per_IP)
    >>= function
    | Error _ -> return ()
    | Ok x -> status GList.(x :: acc)

  and status acc =
    request ~address src sender rsp_queue (IP_receive Status)
    >>= function
    | Error _ -> return ()
    | Ok x -> protocol GList.(x :: acc)

  and protocol acc =
    request ~address src sender rsp_queue (IP_receive Protocol)
    >>= function
    | Error _ -> return ()
    | Ok x -> packet_size GList.(x :: acc)

  and packet_size acc =
    request ~address src sender rsp_queue (IP_receive Packet_size)
    >>= function
    | Error _ -> return ()
    | Ok x -> pcr_present GList.(x :: acc)

  and pcr_present acc =
    request ~address src sender rsp_queue (IP_receive PCR_present)
    >>= function
    | Error _ -> return ()
    | Ok x -> rate_change_counter GList.(x :: acc)

  and rate_change_counter acc =
    request ~address src sender rsp_queue (IP_receive Rate_change_counter)
    >>= function
    | Error _ -> return ()
    | Ok x -> jitter_error_counter GList.(x :: acc)

  and jitter_error_counter acc =
    request ~address src sender rsp_queue (IP_receive Jitter_error_counter)
    >>= function
    | Error _ -> return ()
    | Ok x -> lock_error_counter GList.(x :: acc)

  and lock_error_counter acc =
    request ~address src sender rsp_queue (IP_receive Lock_error_counter)
    >>= function
    | Error _ -> return ()
    | Ok x -> delay_factor GList.(x :: acc)

  and delay_factor acc =
    request ~address src sender rsp_queue (IP_receive Delay_factor)
    >>= function
    | Error _ -> return ()
    | Ok x -> asi_bitrate GList.(x :: acc)

  and asi_bitrate acc =
    request ~address src sender rsp_queue (ASI_output Bitrate)
    >>= function
    | Error _ -> return ()
    | Ok asi_bitrate ->
      let status = match acc with
        | delay_factor :: lock_err_cnt :: jitter_err_cnt
          :: rate_change_cnt :: pcr_present :: packet_size
          :: protocol :: status :: tp_per_ip :: lost_before_fec
          :: lost_after_fec :: bitrate :: jitter_tol :: fec_rows
          :: fec_cols :: fec_delay :: [] ->
          { Board_dektec_dtm3200_types.
            fec_delay
          ; fec_cols
          ; fec_rows
          ; jitter_tol
          ; lost_after_fec
          ; lost_before_fec
          ; tp_per_ip
          ; status
          ; protocol
          ; packet_size
          ; bitrate
          ; pcr_present
          ; rate_change_cnt
          ; jitter_err_cnt
          ; lock_err_cnt
          ; delay_factor
          ; asi_bitrate
          } in
      continue status
  in
  fec_delay
