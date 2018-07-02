open Common
open Containers

type devinfo =
  { serial    : int
  ; hw_ver    : int
  ; fpga_ver  : int
  ; soft_ver  : int
  ; asi       : bool
  ; receivers : int list
  } [@@deriving yojson]

type standard = T2
              | T
              | C [@@deriving yojson, eq, show]

type bw = Bw8
        | Bw7
        | Bw6 [@@deriving yojson, eq, show]

type channel =
  { bw   : bw
  ; freq : int
  ; plp  : int
  } [@@deriving yojson, eq, show]

type mode =
  { standard : standard
  ; channel  : channel
  } [@@deriving yojson, eq, show]

type mode_rsp =
  { mode       : mode
  ; hw_present : bool
  ; lock       : bool
  } [@@deriving yojson, show]

type plp_list =
  { timestamp : Time.t
  ; lock      : bool
  ; plps      : int list
  } [@@deriving yojson, show]

type plp_set_req = int

type plp_set_rsp =
  { lock : bool
  ; plp  : int
  } [@@deriving yojson]

type measures =
  { timestamp : Time.t
  ; lock      : bool
  ; power     : float option
  ; mer       : float option
  ; ber       : float option
  ; freq      : int option
  ; bitrate   : int option
  } [@@deriving yojson, show]

type lock =
  { timestamp : Time.t
  ; lock      : bool
  } [@@deriving yojson]

type t2_params =
  { fft             : int
  ; gi              : int
  ; bw_ext          : bool
  ; papr            : int
  ; l1_rep          : bool
  ; l1_mod          : int
  ; freq            : int
  ; l1_post_sz      : int
  ; l1_post_info_sz : int
  ; tr_fmt          : int
  ; sys_id          : int
  ; net_id          : int
  ; cell_id         : int
  ; t2_frames       : int
  ; ofdm_syms       : int
  ; pp              : int
  ; plp_num         : int
  ; tx_id_avail     : int
  ; num_rf          : int
  ; cur_rf_id       : int
  ; cur_plp_id      : int
  ; plp_type        : int
  ; cr              : int
  ; plp_mod         : int
  ; rotation        : bool
  ; fec_sz          : int
  ; fec_block_num   : int
  ; in_band_flag    : bool
  } [@@deriving yojson, eq, show]

type params =
  { timestamp : Time.t
  ; params    : t2_params option
  } [@@deriving yojson, show]

type config = (int * config_item) list
and config_item =
  { standard : standard
  ; t2       : channel
  ; t        : channel
  ; c        : channel
  } [@@deriving yojson, eq]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c

let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let default ?(freq=586000000) ?(plp=0) () = { bw = Bw8; freq; plp }

let config_default : config =
  [ 0, { standard = T2
       ; t2       = default ()
       ; t        = default ()
       ; c        = default () }
  ; 1, { standard = T2
       ; t2       = default ~plp:1 ()
       ; t        = default ()
       ; c        = default () }
  ; 2, { standard = T2
       ; t2       = default ~plp:2 ()
       ; t        = default ()
       ; c        = default () }
  ; 3, { standard = T2
       ; t2       = default ~freq:666_000_000 ()
       ; t        = default ()
       ; c        = default () }
  ]
