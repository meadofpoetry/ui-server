open Board_dektec_dtm3200_types
open Netlib

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

type access = R | W | E [@@deriving eq]

type category =
  [ `Device [@value 0x01]
  | `Configuration
  | `Network
  | `IP_receive [@value 0x81]
  | `ASI_output [@value 0x84]
  ] [@@deriving eq, enum]

type 'a cmd =
  { category : category
  ; setting: int
  ; data : 'a
  ; rw : access
  }

type 'a rsp =
  [ `Value of 'a
  | `Error of string
  ]

let make_cmd ?data ~category ~setting ~rw () =
  { category; setting; rw; data }

let access_of_rw = function
  | `R -> R
  | `W _ -> W

let data_of_rw f = function
  | `R -> None
  | `W x -> Some (f x)

let category_to_string = function
  | `Device -> "Device"
  | `Configuration -> "Configuration"
  | `Network -> "Network"
  | `IP_receive -> "IP receive"
  | `ASI_output -> "ASI output"

let take n s =
  if n < String.length s
  then String.sub s 0 n
  else s

let drop n s =
  if n < String.length s
  then String.sub s n (String.length s - n)
  else ""

let take_drop n s = take n s, drop n s

let check_cmd (eq : 'a -> 'a -> bool)
      (_of : int -> 'a option)
      (x : Cstruct.t cmd)
      (r : 'b rw)
      (setting : 'a)
      (f : Cstruct.t -> 'c option) : 'c rsp option =
  match x.rw, _of x.setting with
  | E, _ -> Some (`Error "got error response")
  | _, None -> None
  | a, Some s when equal_access a (access_of_rw r) && eq s setting ->
     (match f x.data with
      | Some x -> Some (`Value x)
      | None ->
         let e = Printf.sprintf "data field parsing failure (%s)"
                 @@ Cstruct.to_string x.data in
         Some (`Error e))
  | _ -> None

module Device = struct

  type setting =
    [ `FPGA_version [@value 0x01]
    | `Hardware_version
    | `Firmware_version
    | `Serial_number
    | `Type
    ] [@@deriving eq, enum]

  type _ t =
    | FPGA_version : int t
    | Hardware_version : int t
    | Firmware_version : int t
    | Serial_number : int t
    | Type : int t

  let response_data_size : setting -> int = function
    | `FPGA_version | `Hardware_version -> Message.sizeof_setting8
    | `Firmware_version | `Serial_number | `Type -> Message.sizeof_setting32

  let to_cmd : type a. a t -> data option cmd = fun x ->
    let set, rw, data = match x with
      | FPGA_version -> `FPGA_version, R, None
      | Hardware_version -> `Hardware_version, R, None
      | Firmware_version -> `Firmware_version, R, None
      | Serial_number -> `Serial_number, R, None
      | Type -> `Type, R, None in
    let setting = setting_to_enum set in
    make_cmd ?data ~category:`Device ~setting ~rw ()

  let of_cmd : type a. a t -> Cstruct.t cmd -> a rsp option = fun req x ->
    let get r s f = check_cmd equal_setting setting_of_enum x r s f in
    match x.category with
    | `Device ->
       (match req with
        | FPGA_version -> get `R `FPGA_version Ascii.Int.get
        | Hardware_version -> get `R `Hardware_version Ascii.Int.get
        | Firmware_version -> get `R `Firmware_version Ascii.Int.get
        | Serial_number -> get `R `Serial_number Ascii.Int.get
        | Type -> get `R `Serial_number Ascii.Int.get)
    | _ -> None
end

module Configuration = struct

  type setting =
    [ `Mode [@value 0x01]
    | `Application
    | `Volatile_storage
    ] [@@deriving eq, enum]

  type _ t =
    | Mode : mode rw -> mode t
    | Application : application rw -> application t
    | Volatile_storage : storage rw -> storage t

  let response_data_size : setting -> int = function
    | `Mode | `Application | `Volatile_storage -> Message.sizeof_setting8

  let to_cmd : type a. a t -> data option cmd = fun x ->
    let set, rw, data = match x with
      | Mode x ->
         `Mode, access_of_rw x,
         data_of_rw (fun x -> `I8 (mode_to_int x)) x
      | Application x ->
         `Application, access_of_rw x,
         data_of_rw (fun x -> `I8 (application_to_int x)) x
      | Volatile_storage x ->
         `Volatile_storage, access_of_rw x,
         data_of_rw (fun x -> `I8 (storage_to_int x)) x in
    let setting = setting_to_enum set in
    make_cmd ?data ~category:`Configuration ~setting ~rw ()

  let of_cmd : type a. a t -> Cstruct.t cmd -> a rsp option = fun req x ->
    let ( % ) f g x = match (g x) with None -> None | Some x -> f x in
    let get r s f = check_cmd equal_setting setting_of_enum x r s f in
    match x.category with
    | `Configuration ->
       (match req with
        | Mode r -> get r `Mode (mode_of_int % Ascii.Int.get)
        | Application r -> get r `Application (application_of_int % Ascii.Int.get)
        | Volatile_storage r -> get r `Volatile_storage (storage_of_int % Ascii.Int.get))
    | _ -> None
end

module Network = struct

  type setting =
    [ `IP_address [@value 0x01]
    | `Subnet_mask
    | `Gateway
    | `DHCP
    | `Reboot
    | `MAC_address
    ] [@@deriving eq, enum]

  type _ t =
    | IP_address : Ipaddr.V4.t rw -> Ipaddr.V4.t t
    | Subnet_mask : Ipaddr.V4.t rw -> Ipaddr.V4.t t
    | Gateway : Ipaddr.V4.t rw -> Ipaddr.V4.t t
    | DHCP : bool rw -> bool t
    | MAC_address: Macaddr.t t
    | Reboot : unit t

  let response_data_size : setting -> int = function
    | `IP_address | `Subnet_mask | `Gateway -> Message.sizeof_setting32
    | `DHCP -> Message.sizeof_setting8
    | `MAC_address -> Message.sizeof_setting48
    | `Reboot -> Message.sizeof_setting8

  let to_cmd : type a. a t -> data option cmd = fun x ->
    let set, rw, data = match x with
      | IP_address x ->
         `IP_address, (access_of_rw x),
         data_of_rw (fun x -> `I32 (Ipaddr.V4.to_int32 x)) x
      | Subnet_mask x ->
         `Subnet_mask, access_of_rw x,
         data_of_rw (fun x -> `I32 (Ipaddr.V4.to_int32 x)) x
      | Gateway x ->
         `Gateway, access_of_rw x,
         data_of_rw (fun x -> `I32 (Ipaddr.V4.to_int32 x)) x
      | DHCP x -> `DHCP, access_of_rw x, data_of_rw (fun x -> `B x) x
      | Reboot -> `Reboot, W, Some (`B true)
      | MAC_address -> `MAC_address, R, None in
    let setting = setting_to_enum set in
    make_cmd ?data ~category:`Network ~setting ~rw ()

  let of_cmd : type a. a t -> Cstruct.t cmd -> a rsp option = fun req x ->
    let get r s f = check_cmd equal_setting setting_of_enum x r s f in
    match x.category with
    | `Network ->
       (match req with
        | IP_address r -> get r `IP_address Ascii.Ipaddr.get
        | Subnet_mask r -> get r `Subnet_mask Ascii.Ipaddr.get
        | Gateway r -> get r `Gateway Ascii.Ipaddr.get
        | DHCP r -> get r `DHCP Ascii.Bool.get
        | Reboot -> get (`W ()) `Reboot (fun _ -> Some ())
        | MAC_address ->
           let rec conv = fun acc s ->
             match take_drop 2 s with
             | (x, "")  -> (acc ^ x)
             | (x, res) -> conv (acc ^ x ^ ":") res in
           let f x =
             (conv "" (Cstruct.to_string x))
             |> Macaddr.of_string
             |> (function Ok x -> Some x | Error _ -> None) in
           get `R `MAC_address f)
    | _ -> None
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
    ] [@@deriving eq, enum]

  type _ t =
    | Addressing_method : meth rw -> meth t
    | Enable : bool rw -> bool t
    | FEC_delay : int t
    | FEC_enable : bool rw -> bool t
    | FEC_columns : int t
    | FEC_rows : int t
    | IP_jitter_tolerance : int t
    | IP_lost_after_FEC : int64 t
    | IP_lost_before_FEC : int64 t
    | UDP_port : int rw -> int t
    | IP_to_output_delay : int rw -> int t
    | Multicast_address : Ipaddr.V4.t rw -> Ipaddr.V4.t t
    | TP_per_IP : int t
    | Status : receiver_status t
    | Protocol : protocol t
    | Index : int32 t
    | Output_type : output t
    | Packet_size : packet_sz t
    | Bitrate : int t
    | PCR_present : bool t
    | Rate_change_counter : int32 t
    | Rate_estimation_mode : rate_mode rw -> rate_mode t
    | Jitter_error_counter : int32 t
    | Lock_error_counter : int32 t
    | Delay_factor : int32 t

  let response_data_size : setting -> int = function
    | `Addressing_method -> Message.sizeof_setting8
    | `Enable -> Message.sizeof_setting8
    | `FEC_delay -> Message.sizeof_setting32
    | `FEC_enable -> Message.sizeof_setting8
    | `FEC_columns -> Message.sizeof_setting16
    | `FEC_rows -> Message.sizeof_setting16
    | `IP_jitter_tolerance -> Message.sizeof_setting32
    | `IP_lost_after_FEC -> Message.sizeof_setting64
    | `IP_lost_before_FEC -> Message.sizeof_setting64
    | `UDP_port -> Message.sizeof_setting16
    | `IP_to_output_delay -> Message.sizeof_setting16
    | `Multicast_address -> Message.sizeof_setting32
    | `TP_per_IP -> Message.sizeof_setting8
    | `Status -> Message.sizeof_setting8
    | `Protocol -> Message.sizeof_setting8
    | `Index -> Message.sizeof_setting32
    | `Output_type -> Message.sizeof_setting8
    | `Packet_size -> Message.sizeof_setting8
    | `Bitrate -> Message.sizeof_setting32
    | `PCR_present -> Message.sizeof_setting8
    | `Rate_change_counter -> Message.sizeof_setting32
    | `Rate_estimation_mode -> Message.sizeof_setting8
    | `Jitter_error_counter -> Message.sizeof_setting32
    | `Lock_error_counter -> Message.sizeof_setting32
    | `Delay_factor -> Message.sizeof_setting32

  let to_cmd : type a. a t -> data option cmd = fun x ->
    let set, rw, data = match x with
      | Addressing_method x ->
         `Addressing_method, access_of_rw x,
         data_of_rw (fun x -> `I8 (meth_to_int x)) x
      | Enable x -> `Enable, access_of_rw x, data_of_rw (fun x -> `B x) x
      | FEC_delay -> `FEC_delay, R, None
      | FEC_enable x -> `FEC_enable, (access_of_rw x), data_of_rw (fun x -> `B x) x
      | FEC_columns -> `FEC_columns, R, None
      | FEC_rows -> `FEC_rows, R, None
      | IP_jitter_tolerance -> `IP_jitter_tolerance, R, None
      | IP_lost_after_FEC -> `IP_lost_after_FEC, R, None
      | IP_lost_before_FEC -> `IP_lost_before_FEC, R, None
      | UDP_port x -> `UDP_port, (access_of_rw x), data_of_rw (fun x -> `I16 x) x
      | IP_to_output_delay x ->
         `IP_to_output_delay, access_of_rw x,
         data_of_rw (fun x -> `I16 x) x
      | Multicast_address x ->
         `Multicast_address, access_of_rw x,
         data_of_rw (fun x -> `I32 (Ipaddr.V4.to_int32 x)) x
      | TP_per_IP -> `TP_per_IP, R, None
      | Status -> `Status, R, None
      | Protocol -> `Protocol, R, None
      | Index -> `Index, R, None
      | Output_type -> `Output_type, R, None
      | Packet_size -> `Packet_size, R, None
      | Bitrate -> `Bitrate, R, None
      | PCR_present -> `PCR_present, R, None
      | Rate_change_counter -> `Rate_change_counter, R, None
      | Rate_estimation_mode x ->
         `Rate_estimation_mode, (access_of_rw x),
         data_of_rw (fun x -> `I8 (rate_mode_to_int x)) x
      | Jitter_error_counter -> `Jitter_error_counter, R, None
      | Lock_error_counter -> `Lock_error_counter, R, None
      | Delay_factor -> `Delay_factor, R, None in
    let setting = setting_to_enum set in
    make_cmd ?data ~category:`IP_receive ~setting ~rw ()

  let of_cmd : type a. a t -> Cstruct.t cmd -> a rsp option = fun req x ->
    let ( % ) f g x = match (g x) with None -> None | Some x -> f x in
    let get r s f = check_cmd equal_setting setting_of_enum x r s f in
    match x.category with
    | `IP_receive ->
       (match req with
        | Addressing_method r ->
           get r `Addressing_method (meth_of_int % Ascii.Int.get)
        | Enable r -> get r `Enable Ascii.Bool.get
        | FEC_delay -> get `R `FEC_delay Ascii.Int.get
        | FEC_enable r -> get r `FEC_enable Ascii.Bool.get
        | FEC_columns -> get `R `FEC_columns Ascii.Int.get
        | FEC_rows -> get `R `FEC_rows Ascii.Int.get
        | IP_jitter_tolerance -> get `R `IP_jitter_tolerance Ascii.Int.get
        | IP_lost_after_FEC -> get `R `IP_lost_after_FEC Ascii.Int64.get
        | IP_lost_before_FEC -> get `R `IP_lost_before_FEC Ascii.Int64.get
        | UDP_port r -> get r `UDP_port Ascii.Int.get
        | IP_to_output_delay r -> get r `UDP_port Ascii.Int.get
        | Multicast_address r -> get r `Multicast_address Ascii.Ipaddr.get
        | TP_per_IP -> get `R `TP_per_IP Ascii.Int.get
        | Status -> get `R `Status (receiver_status_of_int % Ascii.Int.get)
        | Protocol -> get `R `Protocol (protocol_of_int % Ascii.Int.get)
        | Index -> get `R `Index Ascii.Int32.get
        | Output_type -> get `R `Output_type (output_of_int % Ascii.Int.get)
        | Packet_size -> get `R `Packet_size (packet_sz_of_int % Ascii.Int.get)
        | Bitrate -> get `R `Bitrate Ascii.Int.get
        | PCR_present -> get `R `PCR_present Ascii.Bool.get
        | Rate_change_counter -> get `R `Rate_change_counter Ascii.Int32.get
        | Rate_estimation_mode r ->
           get r `Rate_estimation_mode (rate_mode_of_int % Ascii.Int.get)
        | Jitter_error_counter -> get `R `Jitter_error_counter Ascii.Int32.get
        | Lock_error_counter -> get `R `Lock_error_counter Ascii.Int32.get
        | Delay_factor -> get `R `Delay_factor Ascii.Int32.get)
    | _ -> None
end

module Asi_output = struct

  type setting =
    [ `Packet_size [@value 0x01]
    | `Physical_port
    | `Bitrate
    ] [@@deriving eq, enum]

  type _ t =
    | Packet_size : asi_packet_sz rw -> asi_packet_sz t
    | Physical_port : int t
    | Bitrate : int t

  let response_data_size : setting -> int = function
    | `Packet_size | `Physical_port -> Message.sizeof_setting8
    | `Bitrate -> Message.sizeof_setting32

  let to_cmd (type a) x =
    let set, rw, data = match (x : a t) with
      | Packet_size x ->
         `Packet_size, access_of_rw x,
         data_of_rw (fun x -> `I8 (asi_packet_sz_to_int x)) x
      | Physical_port -> `Physical_port, R, None
      | Bitrate -> `Bitrate, R, None in
    let setting = setting_to_enum set in
    make_cmd ?data ~category:`ASI_output ~setting ~rw ()

  let of_cmd : type a. a t -> Cstruct.t cmd -> a rsp option = fun req x ->
    let ( % ) f g x = match (g x) with None -> None | Some x -> f x in
    let get r s f = check_cmd equal_setting setting_of_enum x r s f in
    match x.category with
    | `Configuration ->
       (match req with
        | Packet_size r ->  get r `Packet_size (asi_packet_sz_of_int % Ascii.Int.get)
        | Physical_port -> get `R `Physical_port Ascii.Int.get
        | Bitrate -> get `R `Bitrate Ascii.Int.get)
    | _ -> None
end

type _ t =
  | Device : 'a Device.t -> 'a t
  | Configuration : 'a Configuration.t -> 'a t
  | Network : 'a Network.t -> 'a t
  | IP_receive : 'a Ip_receive.t -> 'a t
  | ASI_output : 'a Asi_output.t -> 'a t

let equal_access a b = match a, b with
  | R, R | W, W | E, E -> true
  | _, _ -> false

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

let to_cmd : type a. a t -> data option cmd = function
  | Device x -> Device.to_cmd x
  | Configuration x -> Configuration.to_cmd x
  | Network x -> Network.to_cmd x
  | IP_receive x -> Ip_receive.to_cmd x
  | ASI_output x -> Asi_output.to_cmd x

let response_data_size = function
  | `Device, x ->
     (match Device.setting_of_enum x with
      | None -> None
      | Some x -> Some (Device.response_data_size x))
  | `Configuration, x ->
     (match Configuration.setting_of_enum x with
      | None -> None
      | Some x -> Some (Configuration.response_data_size x))
  | `Network, x ->
     (match Network.setting_of_enum x with
      | None -> None
      | Some x -> Some (Network.response_data_size x))
  | `IP_receive, x ->
     (match Ip_receive.setting_of_enum x with
      | None -> None
      | Some x -> Some (Ip_receive.response_data_size x))
  | `ASI_output, x ->
     (match Asi_output.setting_of_enum x with
      | None -> None
      | Some x -> Some (Asi_output.response_data_size x))
