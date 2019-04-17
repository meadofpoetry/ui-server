open Board_niitv_dvb_types
open Message

type tag =
  [ `Devinfo
  | `Mode of int
  | `Measure of int
  | `Params of int
  | `PLP_list of int
  | `Source_id
  | `Ack
  ] [@@deriving eq, show]

type 'a msg =
  { tag : tag
  ; data : 'a
  } [@@deriving show]

type error =
  | Timeout
  | Queue_overflow
  | Not_responding
  | Invalid_length
  | Invalid_payload

let error_to_string = function
  | Timeout -> "timeout"
  | Queue_overflow -> "queue overflow"
  | Not_responding -> "not responding"
  | Invalid_length -> "invalid length"
  | Invalid_payload -> "invalid payload"

type _ t =
  | Get_devinfo : Device.info t
  | Reset : Device.info t
  | Set_src_id : int -> int t
  | Set_mode : (int * Device.mode) -> (int * Device.mode_rsp) t
  | Get_measure : int -> (int * Measure.t) t
  | Get_params : int -> (int * Params.t) t
  | Get_plp_list : int -> (int * Plp_list.t) t

let timeout (type a) : a t -> float = function
  | Get_devinfo -> 3.
  | Reset -> 20.
  | Set_src_id _ -> 3.
  | Set_mode _ -> 3.
  | Get_measure _ -> 3.
  | Get_params _ -> 3.
  | Get_plp_list _ -> 3.

let value_to_string (type a) (t : a t) : a -> string =
  match t with
  | Get_devinfo -> Device.info_to_string
  | Reset -> Device.info_to_string
  | Set_src_id _ -> string_of_int
  | Set_mode _ ->
    (fun (id, rsp) ->
       Printf.sprintf "tuner: %d, %s"
         id (Device.mode_rsp_to_string rsp))
  | Get_measure _ ->
    (fun (id, rsp) ->
       Printf.sprintf "tuner: %d, %s"
         id (Measure.to_string rsp))
  | Get_params _ ->
    (fun (id, rsp) ->
       Printf.sprintf "tuner: %d, %s"
         id (Params.show rsp))
  | Get_plp_list _ ->
    (fun (id, rsp) ->
       Printf.sprintf "tuner: %d, %s"
         id (Plp_list.to_string rsp))

let to_string (type a) : a t -> string = function
  | Get_devinfo -> "Get device info"
  | Reset -> "Reset"
  | Set_src_id id -> Printf.sprintf "Set source ID (src_id=%d)" id
  | Set_mode (id, mode) ->
    Printf.sprintf "Set mode (id=%d, mode=%s)" id Device.(mode_to_string mode)
  | Get_measure id -> Printf.sprintf "Get measure (id=%d)" id
  | Get_params id -> Printf.sprintf "Get params (id=%d)" id
  | Get_plp_list id -> Printf.sprintf "Get PLP list (id=%d)" id

let to_tag (type a) : a t -> tag = function
  | Get_devinfo -> `Devinfo
  | Reset -> `Devinfo
  | Set_src_id _ -> `Source_id
  | Set_mode (id, _) -> `Mode id
  | Get_measure id -> `Measure id
  | Get_params id -> `Params id
  | Get_plp_list id -> `PLP_list id

let min_tuner_id = 0
let max_tuner_id = 3

let tag_to_enum = function
  | `Devinfo -> 0x10
  | `Mode id -> 0x20 lor id
  | `Measure id -> 0x30 lor id
  | `Params id -> 0x40 lor id
  | `PLP_list id -> 0x50 lor id
  | `Source_id -> 0xD0
  | `Ack -> 0xEE

let tag_of_enum = function
  | 0xEE -> Some `Ack
  | 0xD0 -> Some `Source_id
  | 0x10 -> Some `Devinfo
  | x ->
    let code, id = x land 0xF0, x land 0x0F in
    if id < min_tuner_id || id > max_tuner_id
    then None else begin match code with
      | 0x20 -> Some (`Mode id)
      | 0x30 -> Some (`Measure id)
      | 0x40 -> Some (`Params id)
      | 0x50 -> Some (`PLP_list id)
      | _ -> None
    end

let to_msg (type a) (t : a t) : Cstruct.t msg =
  let tag = to_tag t in
  let data = match t with
    | Get_devinfo -> Cstruct.create sizeof_cmd_devinfo
    | Reset ->
      let data = Cstruct.create sizeof_cmd_devinfo in
      set_cmd_devinfo_reset data 0xFF;
      data
    | Set_src_id id ->
      let data = Cstruct.create sizeof_cmd_src_id in
      set_cmd_src_id_source_id data id;
      data
    | Set_mode (_, mode) ->
      let data = Cstruct.create sizeof_mode in
      set_mode_standard data (Device.standard_to_enum mode.standard);
      set_mode_bw data (Device.bw_to_enum mode.channel.bw);
      set_mode_freq data @@ Int32.of_int mode.channel.freq;
      set_mode_plp data mode.channel.plp;
      data
    | Get_measure _ -> Cstruct.create 1
    | Get_params _ -> Cstruct.create 1
    | Get_plp_list _ -> Cstruct.create 1
  in
  { tag; data }
