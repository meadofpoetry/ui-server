open Board_types

include Common.Dvb_t2_types


(** L1 types **)

type l1_pre =
  { typ                : int
  ; preamble           : int
  ; fft                : int
  ; mixed_flag         : bool
  ; bwt_ext            : bool
  ; s1                 : int
  ; s2                 : int
  ; l1_repetition_flag : bool
  ; guard_interval     : int
  ; papr               : int
  ; l1_mod             : int
  ; l1_cod             : int
  ; l1_fec_type        : int
  ; l1_post_size       : int
  ; l1_post_info_size  : int
  ; pilot_pattern      : int
  ; tx_id_availability : int
  ; cell_id            : int
  ; network_id         : int
  ; t2_system_id       : int
  ; num_t2_frames      : int
  ; num_data_symbols   : int
  ; regen_flag         : int
  ; l1_post_extension  : bool
  ; num_rf             : int
  ; current_rf_idx     : int
  ; t2_version         : int
  ; l1_post_scrambled  : bool
  ; t2_base_lite       : bool
  ; reserved           : int
  } [@@deriving yojson]

type t2_l1_post_conf_rf =
  { rf_idx    : int
  ; frequency : int
  } [@@deriving yojson]

type t2_l1_post_conf_fef =
  { fef_type     : int
  ; fef_length   : int
  ; fef_interval : int
  } [@@deriving yojson]

type t2_l1_post_conf_plp =
  { plp_id              : int
  ; plp_type            : int
  ; plp_payload_type    : int
  ; ff_flag             : bool
  ; first_rf_idx        : int
  ; first_frame_idx     : int
  ; plp_group_id        : int
  ; plp_cod             : int
  ; plp_mod             : int
  ; plp_rotation        : bool
  ; plp_fec_type        : int
  ; plp_num_blocks_max  : int
  ; frame_interval      : int
  ; time_il_length      : int
  ; time_il_type        : bool
  ; in_band_a_flag      : bool
  ; in_band_b_flag      : bool
  ; reserved_1          : int
  ; plp_mode            : int
  ; static_flag         : bool
  ; static_padding_flag : bool
  } [@@deriving yojson]

type t2_l1_post_conf_aux =
  { aux_stream_type  : int
  ; aux_private_conf : int
  } [@@deriving yojson]

type l1_post_conf =
  { sub_slices_per_frame  : int
  ; aux_config_rfu        : int
  ; rf                    : t2_l1_post_conf_rf list
  ; fef                   : t2_l1_post_conf_fef option
  ; plp                   : t2_l1_post_conf_plp list
  ; fef_length_msb        : int
  ; reserved_2            : int
  ; aux                   : t2_l1_post_conf_aux list
  } [@@deriving yojson]

module type L1_parser = sig
  val l1_pre_of_string       : string -> l1_pre option
  val l1_post_conf_of_string : l1_pre -> string -> l1_post_conf option
end

module Parser : L1_parser = struct

  let l1_pre_of_string (msg:string) : l1_pre option =
    let bs = Bitstring.bitstring_of_string msg in
    match%bitstring bs with
    | {| typ : 8
       ; bwt_ext : 1; s1 : 3; s2 : 4; l1_repetition_flag : 1
       ; guard_interval : 3
       ; papr : 4
       ; l1_mod : 4
       ; l1_cod : 2
       ; l1_fec_type : 2
       ; l1_post_size : 18; l1_post_info_size : 18
       ; pilot_pattern : 4
       ; tx_id_availability : 8; cell_id : 16; network_id : 16; t2_system_id : 16
       ; num_t2_frames : 8; num_data_symbols : 12; regen_flag : 3; l1_post_extension : 1
       ; num_rf : 3; current_rf_idx : 3
       ; t2_version : 4
       ; l1_post_scrambled : 1; t2_base_lite : 1; reserved : 4
       |} -> Some { typ
                  ; preamble   = s1
                  ; fft        = s2 lsr 1
                  ; mixed_flag = s2 land 1 <> 0
                  ; bwt_ext; s1; s2; l1_repetition_flag; guard_interval
                  ; papr; l1_mod; l1_cod; l1_fec_type; l1_post_size; l1_post_info_size
                  ; pilot_pattern; tx_id_availability; cell_id; network_id; t2_system_id
                  ; num_t2_frames; num_data_symbols; regen_flag; l1_post_extension
                  ; num_rf; current_rf_idx; t2_version; l1_post_scrambled; t2_base_lite; reserved }
    | {| _ |} -> None

  let l1_post_conf_rf_of_bitstring (bs:Bitstring.t) =
    let rec f  = fun acc x -> if Bitstring.bitstring_length x = 0 then List.rev acc
                              else let rf = (match%bitstring x with
                                             | {| rf_idx    : 3
                                                ; frequency : 32 : map (fun x -> Int32.to_int x)
                                                |} -> { rf_idx; frequency }) in
                                   f (rf :: acc) (Bitstring.dropbits 35 x) in
    f [] bs

  let l1_post_conf_fef_of_bitstring (bs:Bitstring.t) =
    match%bitstring bs with
    | {| fef_type : 4; fef_length : 22; fef_interval : 8 |} -> Some { fef_type; fef_length; fef_interval }
    | {| _ |} -> None

  let l1_post_conf_plp_of_bitstring (bs:Bitstring.t) =
    let rec f  = fun acc x ->
      if Bitstring.bitstring_length x = 0 then List.rev acc
      else let plp = (match%bitstring x with
                      | {| plp_id : 8
                         ; plp_type : 3
                         ; plp_payload_type : 5
                         ; ff_flag : 1; first_rf_idx : 3; first_frame_idx : 8; plp_group_id : 8
                         ; plp_cod : 3
                         ; plp_mod : 3
                         ; plp_rotation : 1
                         ; plp_fec_type : 2
                         ; plp_num_blocks_max : 10; frame_interval : 8; time_il_length : 8
                         ; time_il_type : 1; in_band_a_flag : 1; in_band_b_flag : 1
                         ; reserved_1 : 11
                         ; plp_mode : 2
                         ; static_flag : 1; static_padding_flag : 1
                         |} -> { plp_id; plp_type; plp_payload_type; ff_flag; first_rf_idx; first_frame_idx;
                                 plp_group_id; plp_cod; plp_mod; plp_rotation; plp_fec_type; plp_num_blocks_max;
                                 frame_interval; time_il_length; time_il_type; in_band_a_flag; in_band_b_flag;
                                 reserved_1; plp_mode; static_flag; static_padding_flag }) in
           f (plp :: acc) (Bitstring.dropbits 89 x) in
    f [] bs

  let l1_post_conf_aux_of_bitstring (bs:Bitstring.t) =
    let rec f = fun acc x -> if Bitstring.bitstring_length x = 0 then List.rev acc
                             else let aux = (match%bitstring x with
                                             | {| aux_stream_type  : 4
                                                ; aux_private_conf : 28
                                                |} -> { aux_stream_type; aux_private_conf }) in
                                  f (aux :: acc) (Bitstring.dropbits 32 x) in
    f [] bs

  let l1_post_conf_of_string (l1_pre:l1_pre) (msg:string) : l1_post_conf option =
    let bs = Bitstring.bitstring_of_string msg in
    match%bitstring bs with
    | {| sub_slices_per_frame : 15; num_plp : 8; num_aux : 4; aux_config_rfu : 8
       ; rf : (l1_pre.num_rf * 35) : bitstring
       ; fef : if l1_pre.s2 land 1 <> 0 then 34 else 0 : bitstring
       ; plp : num_plp * 89 : bitstring
       ; fef_length_msb : 2; reserved_2 : 30
       ; aux : num_aux * 32 : bitstring
       |} -> (try
                Some { sub_slices_per_frame
                     ; aux_config_rfu
                     ; fef_length_msb; reserved_2
                     ; rf  = l1_post_conf_rf_of_bitstring  rf
                     ; fef = l1_post_conf_fef_of_bitstring fef
                     ; plp = l1_post_conf_plp_of_bitstring plp
                     ; aux = l1_post_conf_aux_of_bitstring aux
                     }
              with _ -> None)
    | {| _ |} -> None

end

(* FFT *)

let t2_fft_of_int t2_profile = function
  | 0b000      -> FFT_2K
  | 0b001      -> FFT_8K
  | 0b010      -> FFT_4K
  | 0b011      -> (match t2_profile with
                   | Base _ -> FFT_1K
                   | Lite _ -> FFT_16K)
  | 0b100      -> FFT_16K
  | 0b101 as x -> (match t2_profile with
                   | Base _ -> FFT_32K
                   | Lite _ -> Unknown x)
  | 0b110      -> FFT_8K
  | 0b111 as x -> (match t2_profile with
                   | Base _ -> FFT_32K
                   | Lite _ -> Unknown x)
  | x          -> Unknown x

(* L1 preamble *)

let l1_preamble_of_int = function
  | 0b000 -> T2 (Base SISO) | 0b001 -> T2 (Base MISO) | 0b010 -> Non_t2
  | 0b011 -> T2 (Lite SISO) | 0b100 -> T2 (Lite MISO) | x -> Unknown x

let l1_preamble_to_int = function
  | T2 (Base SISO) -> 0b000 | T2 (Base MISO) -> 0b001 | Non_t2 -> 0b010
  | T2 (Lite SISO) -> 0b011 | T2 (Lite MISO) -> 0b100 | Unknown x -> x

(* Streams type *)

let t2_streams_type_of_int : int -> t2_streams_type = function
  | 0x00 -> TS | 0x01 -> GS | 0x02 -> Both | x -> Unknown x

let t2_streams_type_to_int : t2_streams_type -> int = function
  | TS -> 0x00 | GS -> 0x01 | Both -> 0x02 | Unknown x -> x

(* Guard interval *)

let t2_gi_of_int = function
  | 0b000 -> GI_1_32   | 0b001 -> GI_1_16   | 0b010 -> GI_1_8
  | 0b011 -> GI_1_4    | 0b100 -> GI_1_128  | 0b101 -> GI_19_128
  | 0b110 -> GI_19_256 | x     -> Unknown x

let t2_gi_to_int = function
  | GI_1_32   -> 0b000 | GI_1_16   -> 0b001 | GI_1_8    -> 0b010
  | GI_1_4    -> 0b011 | GI_1_128  -> 0b100 | GI_19_128 -> 0b101
  | GI_19_256 -> 0b110 | Unknown x -> x

(* PAPR *)

let t2_papr_legacy_of_int = function
  | 0b0000 -> Off | 0b0001 -> ACE | 0b0010 -> TR | 0b0011 -> ACE_TR | x -> Unknown x

let t2_papr_legacy_to_int = function
  | Off -> 0b0000 | ACE -> 0b0001 | TR -> 0b0010 | ACE_TR -> 0b0011 | Unknown x -> x

let t2_papr_modern_of_int = function
  | 0b0000 -> L1_ACE_TR_P2 | 0b0001 -> L1_ACE_ACE | 0b0010 -> L1_ACE_TR | 0b0011 -> L1_ACE_ACE_TR | x -> Unknown x

let t2_papr_modern_to_int = function
  | L1_ACE_TR_P2 -> 0b0000 | L1_ACE_ACE -> 0b0001 | L1_ACE_TR -> 0b0010 | L1_ACE_ACE_TR -> 0b0011 | Unknown x -> x

let t2_papr_of_int version x =
  match version with
  | V1_1_1 -> Legacy (t2_papr_legacy_of_int x)
  | _      -> Modern (t2_papr_modern_of_int x)

let t2_papr_to_int = function
  | Legacy x -> t2_papr_legacy_to_int x
  | Modern x -> t2_papr_modern_to_int x

(* L1 modulation *)

let t2_l1_mod_of_int = function
  | 0b0000 -> BPSK | 0b0001 -> QPSK | 0b0010 -> QAM16 | 0b0011 -> QAM64 | x -> Unknown x

let t2_l1_mod_to_int = function
  | BPSK -> 0b0000 | QPSK -> 0b0001 | QAM16 -> 0b0010 | QAM64 -> 0b0011 | Unknown x -> x

(* L1 code rate *)

let t2_l1_cod_of_int : int -> t2_l1_cod = function
  | 0b00 -> CR_1_2 | x -> Unknown x

let t2_l1_cod_to_int : t2_l1_cod -> int = function
  | CR_1_2 -> 0b00 | Unknown x -> x

(* L1 fec *)

let t2_l1_fec_of_int : int -> t2_l1_fec= function
  | 0b00 -> LDPC_16K | x -> Unknown x

let t2_l1_fec_to_int : t2_l1_fec -> int = function
  | LDPC_16K -> 0b00 | Unknown x -> x


(* Pilot pattern *)

let t2_pp_of_int = function
  | 0b0000 -> PP1 | 0b0001 -> PP2 | 0b0010 -> PP3 | 0b0011 -> PP4
  | 0b0100 -> PP5 | 0b0101 -> PP6 | 0b0110 -> PP7 | 0b0111 -> PP8
  | x -> Unknown x

let t2_pp_to_int = function
  | PP1 -> 0b0000 | PP2 -> 0b0001 | PP3 -> 0b0010 | PP4 -> 0b0011
  | PP5 -> 0b0100 | PP6 -> 0b0101 | PP7 -> 0b0110 | PP8 -> 0b0111
  | Unknown x -> x

(* T2 version *)

let t2_version_of_int = function
  | 0b0000 -> V1_1_1 | 0b0001 -> V1_2_1 | 0b0010 -> V1_3_1 | x -> Unknown x

let t2_version_to_int = function
  | V1_1_1 -> 0b0000 | V1_2_1 -> 0b0001 | V1_3_1 -> 0b0010 | Unknown x -> x


(* PLP type *)

let t2_plp_type_of_int = function
  | 0b000 -> Common | 0b001 -> Data_type_1 | 0b010 -> Data_type_2 | x -> Unknown x

let t2_plp_type_to_int = function
  | Common -> 0b000 | Data_type_1 -> 0b001 | Data_type_2 -> 0b010 | Unknown x -> x

(* PLP payload type *)

let t2_plp_payload_type_of_int = function
  | 0b00000 -> GFPS | 0b00001 -> GCS | 0b00010 -> GSE | 0b00011 -> TS | x -> Unknown x

let t2_plp_payload_type_to_int = function
  | GFPS -> 0b00000 | GCS -> 0b00001 | GSE -> 0b00010 | TS -> 0b00011 | Unknown x -> x

(* PLP code rate *)

let t2_plp_cod_of_int t2_profile = function
  | 0b000      -> CR_1_2
  | 0b001      -> CR_3_5
  | 0b010      -> CR_2_3
  | 0b011      -> CR_3_4
  | 0b100 as x -> (match t2_profile with
                   | Base _ -> CR_4_5
                   | Lite _ -> Unknown x)
  | 0b101 as x -> (match t2_profile with
                   | Base _ -> CR_5_6
                   | Lite _ -> Unknown x)
  | 0b110 as x -> (match t2_profile with
                   | Lite _ -> CR_1_3
                   | Base _ -> Unknown x)
  | 0b111 as x -> (match t2_profile with
                   | Lite _ -> CR_2_5
                   | Base _ -> Unknown x)
  | x          -> Unknown x

let t2_plp_cod_to_int = function
  | CR_1_2 -> 0b000 | CR_3_5 -> 0b001 | CR_2_3 -> 0b010
  | CR_3_4 -> 0b011 | CR_4_5 -> 0b100 | CR_5_6 -> 0b101
  | CR_1_3 -> 0b110 | CR_2_5 -> 0b111 | Unknown x -> x


(* PLP modulation *)

let t2_plp_mod_of_int = function
  | 0b000 -> QPSK | 0b001 -> QAM16 | 0b010 -> QAM64 | 0b011 -> QAM256 | x -> Unknown x

let t2_plp_mod_to_int = function
  | QPSK -> 0b000 | QAM16 -> 0b001 | QAM64 -> 0b010 | QAM256 -> 0b011 | Unknown x -> x

(* PLP fec type *)

let t2_plp_fec_of_int t2_profile = function
  | 0b00      -> LDPC_16K
  | 0b01 as x -> (match t2_profile with
                  | Base _ -> LDPC_64K
                  | Lite _ -> Unknown x)
  | x          -> Unknown x

let t2_plp_fec_to_int = function
  | LDPC_16K -> 0b00 | LDPC_64K -> 0b01 | Unknown x -> x


(* PLP mode *)

let t2_plp_mode_of_int = function
  | 0b00 -> Not_specified | 0b01 -> NM | 0b10 -> HEM | x -> Unknown x

let t2_plp_mode_to_int = function
  | Not_specified -> 0b00 | NM -> 0b01 | HEM -> 0b10 | Unknown x -> x

(* Aux stream type *)

let aux_stream_type_of_int = function
  | 0b0000 -> TX_SIG | x -> Unknown x

let aux_stream_type_to_int = function
  | TX_SIG -> 0xb0000 | Unknown x -> x
