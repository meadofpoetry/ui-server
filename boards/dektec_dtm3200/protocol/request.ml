open Board_dektec_dtm3200_types

type event =
  | Fec_delay of int
  | Fec_cols of int
  | Fec_rows of int
  | Jitter_tol of int
  | Lost_after_fec of int64
  | Lost_before_fec of int64
  | Tp_per_ip of int
  | Status of receiver_status
  | Protocol of protocol
  | Packet_size of packet_sz
  | Bitrate of int
  | Pcr_present of bool
  | Rate_change_cnt of int32
  | Jitter_err_cnt of int32
  | Lock_err_cnt of int32
  | Delay_factor of int32
  | Asi_bitrate of int [@@deriving show]

type 'a rw =
  [ `R
  | `W of 'a
  ]

type _ device =
  | FPGA_version : int device
  | Hardware_version : int device
  | Firmware_version : int device
  | Serial_number : int device
  | Type : int device

type _ configuration =
  | Mode : mode rw -> mode configuration
  | Application : application rw -> application configuration
  | Volatile_storage : storage rw -> storage configuration

type _ network =
  | IP_address : Ipaddr.V4.t rw -> Ipaddr.V4.t network
  | Subnet_mask : Ipaddr.V4.t rw -> Ipaddr.V4.t network
  | Gateway : Ipaddr.V4.t rw -> Ipaddr.V4.t network
  | DHCP : bool rw -> bool network
  | MAC_address: Macaddr.t network
  | Reboot : unit network

(* TODO remove event type *)
type _ ip_receive =
  | Addressing_method : meth rw -> meth ip_receive
  | Enable : bool rw -> bool ip_receive
  | FEC_delay : event ip_receive
  | FEC_enable : bool rw -> bool ip_receive
  | FEC_columns : event ip_receive
  | FEC_rows : event ip_receive
  | IP_jitter_tolerance : event ip_receive
  | IP_lost_after_FEC : event ip_receive
  | IP_lost_before_FEC : event ip_receive
  | UDP_port : int rw -> int ip_receive
  | IP_to_output_delay : int rw -> int ip_receive
  | Multicast_address : Ipaddr.V4.t rw -> Ipaddr.V4.t ip_receive
  | TP_per_IP : event ip_receive
  | Status : event ip_receive
  | Protocol : event ip_receive
  | Output_type : output ip_receive
  | Packet_size : event ip_receive
  | Bitrate : event ip_receive
  | PCR_present : event ip_receive
  | Rate_change_counter : event ip_receive
  | Rate_estimation_mode : rate_mode rw -> rate_mode ip_receive
  | Jitter_error_counter : event ip_receive
  | Lock_error_counter : event ip_receive
  | Delay_factor : event ip_receive

type _ asi_output =
  | Packet_size : asi_packet_sz rw -> asi_packet_sz asi_output
  | Bitrate : event asi_output

type _ t =
  | Device : 'a device -> 'a t
  | Configuration : 'a configuration -> 'a t
  | Network : 'a network -> 'a t
  | IP_receive : 'a ip_receive -> 'a t
  | ASI_output : 'a asi_output -> 'a t

let conv_rw = function
  | `R -> Message.R
  | `W _ -> Message.W

let code : type a. a t -> int * int * Message.rw = function
  | Device x ->
     let setting, rw = match x with
       | FPGA_version -> 0x01, Message.R
       | Hardware_version -> 0x02, R
       | Firmware_version -> 0x03, R
       | Serial_number -> 0x04, R
       | Type -> 0x05, R in
     0x01, setting, rw
  | Configuration x ->
     let setting, rw = match x with
       | Mode rw -> 0x01, conv_rw rw
       | Application rw -> 0x02, conv_rw rw
       | Volatile_storage rw -> 0x03, conv_rw rw in
     0x02, setting, rw
  | Network x ->
     let setting, rw = match x with
       | IP_address x -> 0x01, conv_rw x
       | Subnet_mask x -> 0x02, conv_rw x
       | Gateway x -> 0x03, conv_rw x
       | DHCP x -> 0x04, conv_rw x
       | Reboot -> 0x05, W
       | MAC_address -> 0x06, R in
     0x03, setting, rw
  | IP_receive x ->
     let setting, rw = match x with
       | Addressing_method x -> 0x01, conv_rw x
       | Enable x -> 0x02, conv_rw x
       | FEC_delay -> 0x03, R
       | FEC_enable x -> 0x04, conv_rw x
       | FEC_columns -> 0x05, R
       | FEC_rows -> 0x06, R
       | IP_jitter_tolerance -> 0x07, R
       | IP_lost_after_FEC -> 0x08, R
       | IP_lost_before_FEC -> 0x09, R
       | UDP_port x -> 0x0A, conv_rw x
       | IP_to_output_delay x -> 0x0B, conv_rw x
       | Multicast_address x -> 0x0C, conv_rw x
       | TP_per_IP -> 0x0D, R
       | Status -> 0x0E, R
       | Protocol -> 0x0F, R
       | Output_type -> 0x11, R
       | Packet_size -> 0x12, R
       | Bitrate -> 0x13, R
       | PCR_present -> 0x14, R
       | Rate_change_counter -> 0x15, R
       | Rate_estimation_mode x -> 0x16, conv_rw x
       | Jitter_error_counter -> 0x17, R
       | Lock_error_counter -> 0x18, R
       | Delay_factor -> 0x19, R in
     0x81, setting, rw
  | ASI_output x ->
     let setting, rw = match x with
       | Packet_size x -> 0x01, conv_rw x
       | Bitrate -> 0x03, R in
     0x84, setting, rw
