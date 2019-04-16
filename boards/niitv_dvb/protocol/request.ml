open Board_niitv_dvb_types

type _ t =
  | Get_devinfo : Device.info t
  | Reset : unit t
  | Set_src_id : int -> int t
  | Set_mode : (int * Device.mode) -> (int * Device.mode_rsp) t
  | Get_measure : int -> (int * Measure.t) t
  | Get_params : int -> (int * Params.t) t
  | Get_plp_list : int -> (int * Plp_list.t) t
