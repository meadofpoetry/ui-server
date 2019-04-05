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

type data =
  [ `B of bool
  | `I8 of int
  | `I16 of int
  | `I32 of int32
  ]

type access = R | W | E

type cmd =
  { cat : int
  ; set: int
  ; data : data option
  ; rw : access
  }

let make_cmd ?data ~cat ~set ~rw () =
  { cat; set; rw; data }

let access_of_rw = function
  | `R -> R
  | `W _ -> W

let data_of_rw f = function
  | `R -> None
  | `W x -> Some (f x)

module Device = struct

  type setting =
    [ `FPGA_version [@value 0x01]
    | `Hardware_version
    | `Firmware_version
    | `Serial_number
    | `Type
    ] [@@deriving enum]

  type _ t =
    | FPGA_version : int t
    | Hardware_version : int t
    | Firmware_version : int t
    | Serial_number : int t
    | Type : int t

  let category = 0x01

  let is_setting_valid x =
    x >= min_setting && x <= max_setting

  let to_cmd : type a. a t -> cmd = fun x ->
    let cat = category in
    match x with
    | FPGA_version ->
       let set = setting_to_enum `FPGA_version in
       make_cmd ~cat ~set ~rw:R ()
    | Hardware_version ->
       let set = setting_to_enum `Hardware_version in
       make_cmd ~cat ~set ~rw:R ()
    | Firmware_version ->
       let set = setting_to_enum `Firmware_version in
       make_cmd ~cat ~set ~rw:R ()
    | Serial_number ->
       let set = setting_to_enum `Serial_number in
       make_cmd ~cat ~set ~rw:R ()
    | Type ->
       let set = setting_to_enum `Type in
       make_cmd ~cat ~set ~rw:R ()
end

module Configuration = struct

  type setting =
    [ `Mode [@value 0x01]
    | `Application
    | `Volatile_storage
    ] [@@deriving enum]

  type _ t =
    | Mode : mode rw -> mode t
    | Application : application rw -> application t
    | Volatile_storage : storage rw -> storage t

  let category = 0x02

  let is_setting_valid x =
    x >= min_setting && x <= max_setting

  let to_cmd : type a. a t -> cmd = fun x ->
    let cat = category in
    match x with
    | Mode x ->
       let rw = access_of_rw x in
       let set = setting_to_enum `Mode in
       let data = data_of_rw (fun x -> `I8 (mode_to_int x)) x in
       make_cmd ?data ~cat ~set ~rw ()
    | Application x ->
       let rw = access_of_rw x in
       let set = setting_to_enum `Application in
       let data = data_of_rw (fun x -> `I8 (application_to_int x)) x in
       make_cmd ?data ~cat ~set ~rw ()
    | Volatile_storage x ->
       let rw = access_of_rw x in
       let set = setting_to_enum `Volatile_storage in
       let data = data_of_rw (fun x -> `I8 (storage_to_int x)) x in
       make_cmd ?data ~cat ~set ~rw ()
end

module Network = struct

  type setting =
    [ `IP_address [@value 0x01]
    | `Subnet_mask
    | `Gateway
    | `DHCP
    | `MAC_address
    | `Reboot
    ] [@@deriving enum]

  type _ t =
    | IP_address : Ipaddr.V4.t rw -> Ipaddr.V4.t t
    | Subnet_mask : Ipaddr.V4.t rw -> Ipaddr.V4.t t
    | Gateway : Ipaddr.V4.t rw -> Ipaddr.V4.t t
    | DHCP : bool rw -> bool t
    | MAC_address: Macaddr.t t
    | Reboot : unit t

  let category = 0x03

  let is_setting_valid x =
    x >= min_setting && x <= max_setting

  let to_cmd : type a. a t -> cmd = fun x ->
    let cat = category in
    match x with
    | IP_address rw ->
       let data = data_of_rw (fun x -> `I32 (Ipaddr.V4.to_int32 x)) rw in
       make_cmd ?data ~cat ~set:0x01 ~rw:(access_of_rw rw) ()
    | Subnet_mask rw ->
       let data = data_of_rw (fun x -> `I32 (Ipaddr.V4.to_int32 x)) rw in
       make_cmd ?data ~cat ~set:0x02 ~rw:(access_of_rw rw) ()
    | Gateway rw ->
       let data = data_of_rw (fun x -> `I32 (Ipaddr.V4.to_int32 x)) rw in
       make_cmd ?data ~cat ~set:0x03 ~rw:(access_of_rw rw) ()
    | DHCP rw ->
       let data = data_of_rw (fun x -> `B x) rw in
       make_cmd ?data ~cat ~set:0x04 ~rw:(access_of_rw rw) ()
    | Reboot -> make_cmd ~cat ~set:0x05 ~rw:W ()
    | MAC_address -> make_cmd ~cat ~set:0x06 ~rw:R ()
end

module Ip_receive = struct

  type setting =
    [ `Addressing_method [@value 0x01]
    | `Enable
    | `FEC_delay
    | `FEC_enable
    | `FEC_columns
    | `FEC_rows
    | `IP_jitter_tolerance
    | `IP_lost_after_FEC
    | `IP_lost_before_FEC
    | `UDP_port
    | `IP_to_output_delay
    | `Multicast_address
    | `TP_per_IP
    | `Status
    | `Protocol
    | `Index
    | `Output_type
    | `Packet_size
    | `Bitrate
    | `PCR_present
    | `Rate_change_counter
    | `Rate_estimation_mode
    | `Jitter_error_counter
    | `Lock_error_counter
    | `Delay_factor
    ] [@@deriving enum]

  (* TODO remove event type *)
  type _ t =
    | Addressing_method : meth rw -> meth t
    | Enable : bool rw -> bool t
    | FEC_delay : event t
    | FEC_enable : bool rw -> bool t
    | FEC_columns : event t
    | FEC_rows : event t
    | IP_jitter_tolerance : event t
    | IP_lost_after_FEC : event t
    | IP_lost_before_FEC : event t
    | UDP_port : int rw -> int t
    | IP_to_output_delay : int rw -> int t
    | Multicast_address : Ipaddr.V4.t rw -> Ipaddr.V4.t t
    | TP_per_IP : event t
    | Status : event t
    | Protocol : event t
    | Index : int32 t
    | Output_type : output t
    | Packet_size : event t
    | Bitrate : event t
    | PCR_present : event t
    | Rate_change_counter : event t
    | Rate_estimation_mode : rate_mode rw -> rate_mode t
    | Jitter_error_counter : event t
    | Lock_error_counter : event t
    | Delay_factor : event t

  let category = 0x81

  let is_setting_valid x =
    x > 0 && x <= 0x19

  let to_cmd : type a. a t -> cmd = fun x ->
    let cat = category in
    match x with
    | Addressing_method rw ->
       let data = data_of_rw (fun x -> `I8 (meth_to_int x)) rw in
       make_cmd ?data ~cat ~set:0x01 ~rw:(access_of_rw rw) ()
    | Enable rw ->
       let data = data_of_rw (fun x -> `B x) rw in
       make_cmd ?data ~cat ~set:0x02 ~rw:(access_of_rw rw) ()
    | FEC_delay -> make_cmd ~cat ~set:0x03 ~rw:R ()
    | FEC_enable rw ->
       let data = data_of_rw (fun x -> `B x) rw in
       make_cmd ?data ~cat ~set:0x04 ~rw:(access_of_rw rw) ()
    | FEC_columns -> make_cmd ~cat ~set:0x05 ~rw:R ()
    | FEC_rows -> make_cmd ~cat ~set:0x06 ~rw:R ()
    | IP_jitter_tolerance -> make_cmd ~cat ~set:0x07 ~rw:R ()
    | IP_lost_after_FEC -> make_cmd ~cat ~set:0x08 ~rw:R ()
    | IP_lost_before_FEC -> make_cmd ~cat ~set:0x09 ~rw:R ()
    | UDP_port rw ->
       let data = data_of_rw (fun x -> `I16 x) rw in
       make_cmd ?data ~cat ~set:0x0A ~rw:(access_of_rw rw) ()
    | IP_to_output_delay rw ->
       make_cmd ~cat ~set:0x0B ~rw:(access_of_rw rw) ()
    | Multicast_address rw ->
       let set = setting_to_enum `Multicast_address in
       let data = data_of_rw (fun x -> `I32 (Ipaddr.V4.to_int32 x)) rw in
       make_cmd ?data ~cat ~set ~rw:(access_of_rw rw) ()
    | TP_per_IP ->
       let set = setting_to_enum `TP_per_IP in
       make_cmd ~cat ~set ~rw:R ()
    | Status ->
       let set = setting_to_enum `Status in
       make_cmd ~cat ~set ~rw:R ()
    | Protocol ->
       make_cmd ~cat ~set:0x0F ~rw:R ()
    | Index ->
       let set = setting_to_enum `Index in
       make_cmd ~cat ~set ~rw:R ()
    | Output_type -> make_cmd ~cat ~set:0x11 ~rw:R ()
    | Packet_size -> make_cmd ~cat ~set:0x12 ~rw:R ()
    | Bitrate -> make_cmd ~cat ~set:0x13 ~rw:R ()
    | PCR_present -> make_cmd ~cat ~set:0x14 ~rw:R ()
    | Rate_change_counter -> make_cmd ~cat ~set:0x15 ~rw:R ()
    | Rate_estimation_mode rw ->
       let data = data_of_rw (fun x -> `I8 (rate_mode_to_int x)) rw in
       make_cmd ?data ~cat ~set:0x16 ~rw:(access_of_rw rw) ()
    | Jitter_error_counter -> make_cmd ~cat ~set:0x17 ~rw:R ()
    | Lock_error_counter -> make_cmd ~cat ~set:0x18 ~rw:R ()
    | Delay_factor -> make_cmd ~cat ~set:0x19 ~rw:R ()
end

module Asi_output = struct

  type setting =
    [ `Packet_size [@value 0x01]
    | `Physical_port
    | `Bitrate
    ] [@@deriving enum]

  type _ t =
    | Packet_size : asi_packet_sz rw -> asi_packet_sz t
    | Physical_port : int t
    | Bitrate : event t

  let category = 0x84

  let is_setting_valid x =
    x >= min_setting && x <= max_setting

  let to_cmd (type a) x =
    let set, rw, data = match (x : a t) with
      | Packet_size x ->
         `Packet_size, access_of_rw x,
         data_of_rw (fun x -> `I8 (asi_packet_sz_to_int x)) x
      | Physical_port -> `Physical_port, R, None
      | Bitrate -> `Bitrate, R, None in
    make_cmd ?data ~cat:category ~set:(setting_to_enum set) ~rw ()
end

type _ t =
  | Device : 'a Device.t -> 'a t
  | Configuration : 'a Configuration.t -> 'a t
  | Network : 'a Network.t -> 'a t
  | IP_receive : 'a Ip_receive.t -> 'a t
  | ASI_output : 'a Asi_output.t -> 'a t

let access_to_int = function
  | R -> int_of_char 'R'
  | W -> int_of_char 'W'
  | E -> int_of_char 'E'

let access_of_int x =
  match char_of_int x with
  | 'R' | 'r' -> Some R
  | 'W' | 'w' -> Some W
  | 'E' | 'e' -> Some E
  | _ -> None

let valid_categories =
  [ Device.category
  ; Configuration.category
  ; Network.category
  ; Ip_receive.category
  ; Asi_output.category
  ]

let to_cmd : type a. a t -> cmd = function
  | Device x -> Device.to_cmd x
  | Configuration x -> Configuration.to_cmd x
  | Network x -> Network.to_cmd x
  | IP_receive x -> Ip_receive.to_cmd x
  | ASI_output x -> Asi_output.to_cmd x

let is_message_valid = function
  | c, x when c = Device.category -> Device.is_setting_valid x
  | c, x when c = Configuration.category -> Configuration.is_setting_valid x
  | c, x when c = Network.category -> Network.is_setting_valid x
  | c, x when c = Ip_receive.category -> Ip_receive.is_setting_valid x
  | c, x when c = Asi_output.category -> Asi_output.is_setting_valid x
  | _ -> false
