open Common.Board.Ip
open Lwt.Infix

include Board_ip_parser

(* Board protocol implementation *)

let period = 50

let (detect : _ request) = Devinfo Get_fpga_ver

let (init : _ request list ) = []

module SM = struct

  exception Timeout

  let send (type a) msgs sender (msg : a request) : a Lwt.t =
    let b = match msg with
      | Devinfo x -> to_req_get msg
      | Overall x -> (match x with
                      | Get_mode          -> to_req_get msg
                      | Get_application   -> to_req_get msg
                      | Get_storage       -> to_req_get msg
                      | Set_mode x        -> to_req_set_int8 msg (mode_to_int x)
                      | Set_application x -> to_req_set_int8 msg (application_to_int x)
                      | Set_storage x     -> to_req_set_int8 msg (storage_to_int x))
      | Nw x      -> (match x with
                      | Get_ip        -> to_req_get msg
                      | Get_mask      -> to_req_get msg
                      | Get_gateway   -> to_req_get msg
                      | Get_dhcp      -> to_req_get msg
                      | Get_mac       -> to_req_get msg
                      | Set_ip x      -> to_req_set_ipaddr msg x
                      | Set_mask x    -> to_req_set_ipaddr msg x
                      | Set_gateway x -> to_req_set_ipaddr msg x
                      | Set_dhcp x    -> to_req_set_bool msg x
                      | Reboot        -> to_req_set_bool msg true)
      | Ip x      -> (match x with
                      | Get_method          -> to_req_get msg
                      | Get_enable          -> to_req_get msg
                      | Get_fec_delay       -> to_req_get msg
                      | Get_fec_enable      -> to_req_get msg
                      | Get_fec_cols        -> to_req_get msg
                      | Get_fec_rows        -> to_req_get msg
                      | Get_jitter_tol      -> to_req_get msg
                      | Get_lost_after_fec  -> to_req_get msg
                      | Get_lost_before_fec -> to_req_get msg
                      | Get_udp_port        -> to_req_get msg
                      | Get_delay           -> to_req_get msg
                      | Get_mcast_addr      -> to_req_get msg
                      | Get_tp_per_ip       -> to_req_get msg
                      | Get_status          -> to_req_get msg
                      | Get_protocol        -> to_req_get msg
                      | Get_output          -> to_req_get msg
                      | Get_packet_size     -> to_req_get msg
                      | Get_bitrate         -> to_req_get msg
                      | Get_pcr_present     -> to_req_get msg
                      | Get_rate_change_cnt -> to_req_get msg
                      | Get_rate_est_mode   -> to_req_get msg
                      | Get_jitter_err_cnt  -> to_req_get msg
                      | Get_lock_err_cnt    -> to_req_get msg
                      | Get_delay_factor    -> to_req_get msg
                      | Set_method x        -> to_req_set_int8 msg (meth_to_int x)
                      | Set_enable x        -> to_req_set_bool msg x
                      | Set_fec_enable x    -> to_req_set_bool msg x
                      | Set_udp_port x      -> to_req_set_int16 msg x
                      | Set_delay x         -> to_req_set_int16 msg x
                      | Set_mcast_addr x    -> to_req_set_ipaddr msg x
                      | Set_rate_est_mode x -> to_req_set_int8 msg (rate_mode_to_int x))
      | Asi x     -> (match x with
                      | Get_packet_size   -> to_req_get msg
                      | Get_bitrate       -> to_req_get msg
                      | Set_packet_size x -> to_req_set_int8 msg (asi_packet_sz_to_int x)) in
    sender b

  let initial_timeout = -1

  let create sender push_state =
    ()

end
