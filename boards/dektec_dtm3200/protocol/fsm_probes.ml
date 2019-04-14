(* open Boards
 * open Boards.Pools
 * open Board_dektec_dtm3200_types
 * open Request
 * open Netlib
 * 
 * let ( >>= ) = Lwt.bind
 * 
 * let timeout = 3. (\* seconds *\)
 * 
 * let interval = 1. (\* seconds *\)
 * 
 * let step ~(address : int)
 *       ~return
 *       ~continue
 *       (src : Logs.src)
 *       (sender : Cstruct.t -> unit Lwt.t)
 *       (pe : Sm_common.push_events) =
 * 
 *   let (module Logs : Logs.LOG) = Logs.src_log src in
 * 
 *   let make_req (type a) (req : a Request.t) =
 *     make_msg
 *       ~timeout:(fun () -> Lwt_unix.sleep timeout)
 *       ~send:(fun () -> sender @@ Serializer.make_req ~address req)
 *       ~resolve:(Parser.is_response req)
 *       () in
 * 
 *   let wait ~next_step pending log_data pool acc recvd =
 *     let (name, to_string) = log_data in
 *     let responses, acc =
 *       Parser.deserialize ~address src
 *       @@ Board.concat_acc acc recvd in
 *     Pool.apply pool responses;
 *     Pool._match pool
 *       ~resolved:(fun _ -> function
 *         | `Error e ->
 *            Logs.warn (fun m -> m "probes - error getting %s: %s" name e);
 *            return ()
 *         | `Value x ->
 *            Logs.debug (fun m -> m "probes - got %s: %s" name (to_string x));
 *            match next_step x with
 *            | `Next next -> next ()
 *            | `CC (r, next) ->
 *               Pool.(send (create [make_req r]))
 *               >>= fun pool ->
 *               Lwt.return @@ `Continue (next pool None))
 *       ~error:(fun _ -> function
 *         | `Timeout ->
 *            Logs.warn (fun m ->
 *                let err = "timeout" in
 *                m "probes - error getting %s: %s" name err);
 *            return ())
 *       ~pending:(fun pool -> Lwt.return @@ `Continue (pending pool acc))
 *       ~not_sent:(fun _ -> assert false) in
 * 
 *   let rec first_step () =
 *     Pool.(send (create [make_req (IP_receive FEC_delay)]))
 *     >>= fun pool ->
 *     Lwt.return @@ `Continue (fec_delay pool None)
 * 
 *   and fec_delay pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive FEC_columns, fec_columns GList.(x :: [])))
 *       fec_delay ("FEC delay", string_of_int) pool acc recvd
 * 
 *   and fec_columns racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive FEC_rows, fec_rows GList.(x :: racc)))
 *       (fec_columns racc) ("FEC columns", string_of_int) pool acc recvd
 * 
 *   and fec_rows racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive IP_jitter_tolerance, jitter_tolerance GList.(x :: racc)))
 *       (fec_rows racc) ("FEC rows", string_of_int) pool acc recvd
 * 
 *   and jitter_tolerance racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive Bitrate, bitrate GList.(x :: racc)))
 *       (jitter_tolerance racc) ("IP jitter tolerance", string_of_int) pool acc recvd
 * 
 *   and bitrate racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive IP_lost_after_FEC, lost_after_fec GList.(x :: racc)))
 *       (bitrate racc) ("Bitrate", string_of_int) pool acc recvd
 * 
 *   and lost_after_fec racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive IP_lost_before_FEC, lost_before_fec GList.(x :: racc)))
 *       (lost_after_fec racc) ("IP lost after FEC", Int64.to_string) pool acc recvd
 * 
 *   and lost_before_fec racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive TP_per_IP, tp_per_ip GList.(x :: racc)))
 *       (lost_before_fec racc) ("IP lost before FEC", Int64.to_string) pool acc recvd
 * 
 *   and tp_per_ip racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive Status, status GList.(x :: racc)))
 *       (tp_per_ip racc) ("TP per IP", string_of_int) pool acc recvd
 * 
 *   and status racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive Protocol, protocol GList.(x :: racc)))
 *       (status racc) ("status", receiver_status_to_string) pool acc recvd
 * 
 *   and protocol racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive Packet_size, packet_size GList.(x :: racc)))
 *       (protocol racc) ("protocol", protocol_to_string) pool acc recvd
 * 
 *   and packet_size racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive PCR_present, pcr_present GList.(x :: racc)))
 *       (packet_size racc) ("packet size", packet_sz_to_string) pool acc recvd
 * 
 *   and pcr_present racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive Rate_change_counter, rate_change_counter GList.(x :: racc)))
 *       (pcr_present racc) ("PCR present", string_of_bool) pool acc recvd
 * 
 *   and rate_change_counter racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive Jitter_error_counter, jitter_error_counter GList.(x :: racc)))
 *       (rate_change_counter racc)
 *       ("rate change counter", Int32.to_string)
 *       pool acc recvd
 * 
 *   and jitter_error_counter racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive Lock_error_counter, lock_error_counter GList.(x :: racc)))
 *       (jitter_error_counter racc)
 *       ("jitter error counter", Int32.to_string)
 *       pool acc recvd
 * 
 *   and lock_error_counter racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive Delay_factor, delay_factor GList.(x :: racc)))
 *       (lock_error_counter racc)
 *       ("lock error counter", Int32.to_string)
 *       pool acc recvd
 * 
 *   and delay_factor racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (ASI_output Bitrate, asi_bitrate GList.(x :: racc)))
 *       (delay_factor racc) ("delay factor", Int32.to_string) pool acc recvd
 * 
 *   and asi_bitrate racc pool acc recvd =
 *     wait (asi_bitrate racc)
 *       ~next_step:(fun x ->
 *         let status = match racc with
 *           | delay_factor :: lock_err_cnt :: jitter_err_cnt
 *             :: rate_change_cnt :: pcr_present :: packet_size
 *             :: protocol :: status :: tp_per_ip :: lost_before_fec
 *             :: lost_after_fec :: bitrate :: jitter_tol :: fec_rows
 *             :: fec_cols :: fec_delay :: [] ->
 *              { fec_delay
 *              ; fec_cols
 *              ; fec_rows
 *              ; jitter_tol
 *              ; lost_after_fec
 *              ; lost_before_fec
 *              ; tp_per_ip
 *              ; status
 *              ; protocol
 *              ; packet_size
 *              ; bitrate
 *              ; pcr_present
 *              ; rate_change_cnt
 *              ; jitter_err_cnt
 *              ; lock_err_cnt
 *              ; delay_factor
 *              ; asi_bitrate = x
 *              } in
 *         pe.status status;
 *         `Next continue)
 *       ("ASI bitrate", string_of_int) pool acc recvd
 * 
 *   in first_step *)
