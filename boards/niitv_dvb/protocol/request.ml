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
  ] [@@deriving eq]

type 'a msg =
  { tag : tag
  ; data : 'a
  }

type _ t =
  | Get_devinfo : Device.info t
  | Reset : Device.info t
  | Set_src_id : int -> int t
  | Set_mode : (int * Device.mode) -> (int * Device.mode_rsp) t
  | Get_measure : int -> (int * Measure.t) t
  | Get_params : int -> (int * Params.t) t
  | Get_plp_list : int -> (int * Plp_list.t) t

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
  | `Devinfo -> 0x01
  | `Mode id -> 0x02 lor id
  | `Measure id -> 0x03 lor id
  | `Params id -> 0x04 lor id
  | `PLP_list id -> 0x05 lor id
  | `Source_id -> 0xD0
  | `Ack -> 0xEE

let tag_of_enum = function
  | 0xEE -> Some `Ack
  | 0xD0 -> Some `Source_id
  | 0x01 -> Some `Devinfo
  | x ->
    let code, id = x land 0x0F, x lsr 4 in
    if id < min_tuner_id || id > max_tuner_id
    then None else begin match code with
      | 0x02 -> Some (`Mode id)
      | 0x03 -> Some (`Measure id)
      | 0x04 -> Some (`Params id)
      | 0x05 -> Some (`PLP_list id)
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
