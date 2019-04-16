open Board_dektec_dtm3200_types
open Fsm_common
open Request
open Netlib

let ( >>= ) = Lwt.bind

let timeout = 3. (* seconds *)

let step ~(address : int)
    ~(return : unit -> unit Lwt.t)
    ~continue
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (stream : Cstruct.t cmd Lwt_stream.t) =

  let request (type a) ~next (req : a Request.t) =
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; next x in

  let rec fec_delay () =
    request ~next:(fun x -> fec_columns GList.(x :: []))
      (IP_receive FEC_delay)

  and fec_columns acc =
    request ~next:(fun x -> fec_rows GList.(x :: acc))
      (IP_receive FEC_columns)

  and fec_rows acc =
    request ~next:(fun x -> jitter_tolerance GList.(x :: acc))
      (IP_receive FEC_rows)

  and jitter_tolerance acc =
    request ~next:(fun x -> bitrate GList.(x :: acc))
      (IP_receive IP_jitter_tolerance)

  and bitrate acc =
    request ~next:(fun x -> lost_after_fec GList.(x :: acc))
      (IP_receive Bitrate)

  and lost_after_fec acc =
    request ~next:(fun x -> lost_before_fec GList.(x :: acc))
      (IP_receive IP_lost_after_FEC)

  and lost_before_fec acc =
    request ~next:(fun x -> tp_per_ip GList.(x :: acc))
      (IP_receive IP_lost_before_FEC)

  and tp_per_ip acc =
    request ~next:(fun x -> status GList.(x :: acc))
      (IP_receive TP_per_IP)

  and status acc =
    request ~next:(fun x -> protocol GList.(x :: acc))
      (IP_receive Status)

  and protocol acc =
    request ~next:(fun x -> packet_size GList.(x :: acc))
      (IP_receive Protocol)

  and packet_size acc =
    request ~next:(fun x -> pcr_present GList.(x :: acc))
      (IP_receive Packet_size)

  and pcr_present acc =
    request ~next:(fun x -> rate_change_counter GList.(x :: acc))
      (IP_receive PCR_present)

  and rate_change_counter acc =
    request ~next:(fun x -> jitter_error_counter GList.(x :: acc))
      (IP_receive Rate_change_counter)

  and jitter_error_counter acc =
    request ~next:(fun x -> lock_error_counter GList.(x :: acc))
      (IP_receive Jitter_error_counter)

  and lock_error_counter acc =
    request ~next:(fun x -> delay_factor GList.(x :: acc))
      (IP_receive Lock_error_counter)

  and delay_factor acc =
    request ~next:(fun x -> asi_bitrate GList.(x :: acc))
      (IP_receive Delay_factor)

  and asi_bitrate acc =
    request ~next:(fun asi_bitrate ->
        let status = match acc with
          | delay_factor :: lock_err_cnt :: jitter_err_cnt
            :: rate_change_cnt :: pcr_present :: packet_size
            :: protocol :: status :: tp_per_ip :: lost_before_fec
            :: lost_after_fec :: bitrate :: jitter_tol :: fec_rows
            :: fec_cols :: fec_delay :: [] ->
            { fec_delay
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
        continue status)
      (ASI_output Bitrate)
  in
  fec_delay
