type t = dvb_t2

and dvb_t2 = {
  lock : bool;
  fft : int;
  gi : int;
  bw_ext : bool;
  papr : int;
  l1_rep : bool;
  l1_mod : int;
  freq : int;
  l1_post_sz : int;
  l1_post_info_sz : int;
  tr_fmt : int;
  sys_id : int;
  net_id : int;
  cell_id : int;
  t2_frames : int;
  ofdm_syms : int;
  pp : int;
  plp_num : int;
  tx_id_avail : int;
  num_rf : int;
  cur_rf_id : int;
  cur_plp_id : int;
  plp_type : int;
  cr : int;
  plp_mod : int;
  rotation : bool;
  fec_sz : int;
  fec_block_num : int;
  in_band_flag : bool;
}
[@@deriving yojson, eq, show]
