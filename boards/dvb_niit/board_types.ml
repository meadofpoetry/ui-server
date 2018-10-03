module Device = struct

  type devinfo =
    { serial : int
    ; hw_ver : int
    ; fpga_ver : int
    ; soft_ver : int
    ; asi : bool
    ; receivers : int list
    } [@@deriving yojson]

  type standard =
    | T2 [@value 1]
    | T
    | C [@@deriving yojson, eq, show, enum]

  type bw =
    | Bw8 [@value 1]
    | Bw7
    | Bw6 [@@deriving yojson, eq, show, enum]

  type channel =
    { bw : bw
    ; freq : int
    ; plp  : int
    } [@@deriving yojson, eq, show]

  type mode =
    { standard : standard
    ; channel : channel
    } [@@deriving yojson, eq, show]

  type mode_rsp =
    { mode : mode
    ; hw_present : bool
    ; lock : bool
    } [@@deriving yojson, show]

  type config = (int * mode) list [@@deriving yojson]

end

module Plp_list = struct

  type t =
    { lock : bool
    ; plps : int list
    } [@@deriving yojson, show]

end

module Measure = struct

  type t =
    { lock : bool
    ; power : float option
    ; mer : float option
    ; ber : float option
    ; freq : int option
    ; bitrate : int option
    } [@@deriving yojson, show]

end

module Params = struct

  type dvb_t2 =
    { lock : bool
    ; fft : int
    ; gi : int
    ; bw_ext : bool
    ; papr : int
    ; l1_rep : bool
    ; l1_mod : int
    ; freq : int
    ; l1_post_sz : int
    ; l1_post_info_sz : int
    ; tr_fmt : int
    ; sys_id : int
    ; net_id : int
    ; cell_id : int
    ; t2_frames : int
    ; ofdm_syms : int
    ; pp : int
    ; plp_num : int
    ; tx_id_avail : int
    ; num_rf : int
    ; cur_rf_id : int
    ; cur_plp_id : int
    ; plp_type : int
    ; cr : int
    ; plp_mod : int
    ; rotation : bool
    ; fec_sz : int
    ; fec_block_num : int
    ; in_band_flag : bool
    } [@@deriving yojson, eq, show]

  type t = dvb_t2 [@@deriving yojson, eq, show]

end
