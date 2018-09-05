include Common.Dvb_t2_types

(* FFT *)

let t2_fft_of_int t2_profile = function
  | 0b000 -> FFT_2K
  | 0b001 -> FFT_8K
  | 0b010 -> FFT_4K
  | 0b011 ->
     begin match t2_profile with
     | Base _ -> FFT_1K
     | Lite _ -> FFT_16K
     end
  | 0b100 -> FFT_16K
  | 0b101 as x ->
     begin match t2_profile with
     | Base _ -> FFT_32K
     | Lite _ -> Unknown x
     end
  | 0b110 -> FFT_8K
  | 0b111 as x ->
     begin match t2_profile with
     | Base _ -> FFT_32K
     | Lite _ -> Unknown x
     end
  | x -> Unknown x

(* L1 preamble *)

let l1_preamble_of_int = function
  | 0b000 -> T2 (Base SISO)
  | 0b001 -> T2 (Base MISO)
  | 0b010 -> Non_t2
  | 0b011 -> T2 (Lite SISO)
  | 0b100 -> T2 (Lite MISO)
  | x -> Unknown x

let l1_preamble_to_int = function
  | T2 (Base SISO) -> 0b000
  | T2 (Base MISO) -> 0b001
  | Non_t2 -> 0b010
  | T2 (Lite SISO) -> 0b011
  | T2 (Lite MISO) -> 0b100
  | Unknown x -> x

(* Streams type *)

let t2_streams_type_of_int : int -> t2_streams_type = function
  | 0x00 -> TS
  | 0x01 -> GS
  | 0x02 -> Both
  | x -> Unknown x

let t2_streams_type_to_int : t2_streams_type -> int = function
  | TS -> 0x00
  | GS -> 0x01
  | Both -> 0x02
  | Unknown x -> x

(* Guard interval *)

let t2_gi_of_int = function
  | 0b000 -> GI_1_32
  | 0b001 -> GI_1_16
  | 0b010 -> GI_1_8
  | 0b011 -> GI_1_4
  | 0b100 -> GI_1_128
  | 0b101 -> GI_19_128
  | 0b110 -> GI_19_256
  | x -> Unknown x

let t2_gi_to_int = function
  | GI_1_32 -> 0b000
  | GI_1_16 -> 0b001
  | GI_1_8 -> 0b010
  | GI_1_4 -> 0b011
  | GI_1_128 -> 0b100
  | GI_19_128 -> 0b101
  | GI_19_256 -> 0b110
  | Unknown x -> x

(* PAPR *)

let t2_papr_legacy_of_int = function
  | 0b0000 -> Off
  | 0b0001 -> ACE
  | 0b0010 -> TR
  | 0b0011 -> ACE_TR
  | x -> Unknown x

let t2_papr_legacy_to_int = function
  | Off -> 0b0000
  | ACE -> 0b0001
  | TR -> 0b0010
  | ACE_TR -> 0b0011
  | Unknown x -> x

let t2_papr_modern_of_int = function
  | 0b0000 -> L1_ACE_TR_P2
  | 0b0001 -> L1_ACE_ACE
  | 0b0010 -> L1_ACE_TR
  | 0b0011 -> L1_ACE_ACE_TR
  | x -> Unknown x

let t2_papr_modern_to_int = function
  | L1_ACE_TR_P2 -> 0b0000
  | L1_ACE_ACE -> 0b0001
  | L1_ACE_TR -> 0b0010
  | L1_ACE_ACE_TR -> 0b0011
  | Unknown x -> x

let t2_papr_of_int version x =
  match version with
  | V1_1_1 -> Legacy (t2_papr_legacy_of_int x)
  | _ -> Modern (t2_papr_modern_of_int x)

let t2_papr_to_int = function
  | Legacy x -> t2_papr_legacy_to_int x
  | Modern x -> t2_papr_modern_to_int x

(* L1 modulation *)

let t2_l1_mod_of_int = function
  | 0b0000 -> BPSK
  | 0b0001 -> QPSK
  | 0b0010 -> QAM16
  | 0b0011 -> QAM64
  | x -> Unknown x

let t2_l1_mod_to_int = function
  | BPSK -> 0b0000
  | QPSK -> 0b0001
  | QAM16 -> 0b0010
  | QAM64 -> 0b0011
  | Unknown x -> x

(* L1 code rate *)

let t2_l1_cod_of_int : int -> t2_l1_cod = function
  | 0b00 -> CR_1_2
  | x -> Unknown x

let t2_l1_cod_to_int : t2_l1_cod -> int = function
  | CR_1_2 -> 0b00
  | Unknown x -> x

(* L1 fec *)

let t2_l1_fec_of_int : int -> t2_l1_fec= function
  | 0b00 -> LDPC_16K
  | x -> Unknown x

let t2_l1_fec_to_int : t2_l1_fec -> int = function
  | LDPC_16K -> 0b00
  | Unknown x -> x


(* Pilot pattern *)

let t2_pp_of_int = function
  | 0b0000 -> PP1
  | 0b0001 -> PP2
  | 0b0010 -> PP3
  | 0b0011 -> PP4
  | 0b0100 -> PP5
  | 0b0101 -> PP6
  | 0b0110 -> PP7
  | 0b0111 -> PP8
  | x -> Unknown x

let t2_pp_to_int = function
  | PP1 -> 0b0000
  | PP2 -> 0b0001
  | PP3 -> 0b0010
  | PP4 -> 0b0011
  | PP5 -> 0b0100
  | PP6 -> 0b0101
  | PP7 -> 0b0110
  | PP8 -> 0b0111
  | Unknown x -> x

(* T2 version *)

let t2_version_of_int = function
  | 0b0000 -> V1_1_1
  | 0b0001 -> V1_2_1
  | 0b0010 -> V1_3_1
  | x -> Unknown x

let t2_version_to_int = function
  | V1_1_1 -> 0b0000
  | V1_2_1 -> 0b0001
  | V1_3_1 -> 0b0010
  | Unknown x -> x


(* PLP type *)

let t2_plp_type_of_int = function
  | 0b000 -> Common
  | 0b001 -> Data_type_1
  | 0b010 -> Data_type_2
  | x -> Unknown x

let t2_plp_type_to_int = function
  | Common -> 0b000
  | Data_type_1 -> 0b001
  | Data_type_2 -> 0b010
  | Unknown x -> x

(* PLP payload type *)

let t2_plp_payload_type_of_int = function
  | 0b00000 -> GFPS
  | 0b00001 -> GCS
  | 0b00010 -> GSE
  | 0b00011 -> TS
  | x -> Unknown x

let t2_plp_payload_type_to_int = function
  | GFPS -> 0b00000
  | GCS -> 0b00001
  | GSE -> 0b00010
  | TS -> 0b00011
  | Unknown x -> x

(* PLP code rate *)

let t2_plp_cod_of_int t2_profile = function
  | 0b000 -> CR_1_2
  | 0b001 -> CR_3_5
  | 0b010 -> CR_2_3
  | 0b011 -> CR_3_4
  | 0b100 as x ->
     begin match t2_profile with
     | Base _ -> CR_4_5
     | Lite _ -> Unknown x
     end
  | 0b101 as x ->
     begin match t2_profile with
     | Base _ -> CR_5_6
     | Lite _ -> Unknown x
     end
  | 0b110 as x ->
     begin match t2_profile with
     | Lite _ -> CR_1_3
     | Base _ -> Unknown x
     end
  | 0b111 as x ->
     begin match t2_profile with
     | Lite _ -> CR_2_5
     | Base _ -> Unknown x
     end
  | x -> Unknown x

let t2_plp_cod_to_int = function
  | CR_1_2 -> 0b000
  | CR_3_5 -> 0b001
  | CR_2_3 -> 0b010
  | CR_3_4 -> 0b011
  | CR_4_5 -> 0b100
  | CR_5_6 -> 0b101
  | CR_1_3 -> 0b110
  | CR_2_5 -> 0b111
  | Unknown x -> x


(* PLP modulation *)

let t2_plp_mod_of_int = function
  | 0b000 -> QPSK
  | 0b001 -> QAM16
  | 0b010 -> QAM64
  | 0b011 -> QAM256
  | x -> Unknown x

let t2_plp_mod_to_int = function
  | QPSK -> 0b000
  | QAM16 -> 0b001
  | QAM64 -> 0b010
  | QAM256 -> 0b011
  | Unknown x -> x

(* PLP fec type *)

let t2_plp_fec_of_int t2_profile = function
  | 0b00 -> LDPC_16K
  | 0b01 as x ->
     begin match t2_profile with
     | Base _ -> LDPC_64K
     | Lite _ -> Unknown x
     end
  | x -> Unknown x

let t2_plp_fec_to_int = function
  | LDPC_16K -> 0b00
  | LDPC_64K -> 0b01
  | Unknown x -> x

(* PLP mode *)

let t2_plp_mode_of_int = function
  | 0b00 -> Not_specified
  | 0b01 -> NM
  | 0b10 -> HEM
  | x -> Unknown x

let t2_plp_mode_to_int = function
  | Not_specified -> 0b00
  | NM -> 0b01
  | HEM -> 0b10
  | Unknown x -> x

(* Aux stream type *)

let aux_stream_type_of_int = function
  | 0b0000 -> TX_SIG
  | x -> Unknown x

let aux_stream_type_to_int = function
  | TX_SIG -> 0xb0000
  | Unknown x -> x
