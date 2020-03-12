type t2_transmittion_format = SISO | MISO [@@deriving yojson]

type t2_profile =
  | Base of t2_transmittion_format
  | Lite of t2_transmittion_format
[@@deriving yojson]

type l1_preamble = T2 of t2_profile | Non_t2 | Unknown of int
[@@deriving yojson]

type t2_fft =
  | FFT_1K
  | FFT_2K
  | FFT_4K
  | FFT_8K
  | FFT_16K
  | FFT_32K
  | Unknown of int
[@@deriving yojson]

type t2_streams_type = TS | GS | Both | Unknown of int [@@deriving yojson]

type t2_gi =
  | GI_1_32
  | GI_1_16
  | GI_1_8
  | GI_1_4
  | GI_1_128
  | GI_19_128
  | GI_19_256
  | Unknown of int
[@@deriving yojson]

type t2_papr_legacy = Off | ACE | TR | ACE_TR | Unknown of int
[@@deriving yojson]

type t2_papr_modern =
  | L1_ACE_TR_P2
  | L1_ACE_ACE
  | L1_ACE_TR
  | L1_ACE_ACE_TR
  | Unknown of int
[@@deriving yojson]

type t2_papr = Legacy of t2_papr_legacy | Modern of t2_papr_modern
[@@deriving yojson]

type t2_l1_mod = BPSK | QPSK | QAM16 | QAM64 | Unknown of int
[@@deriving yojson]

type t2_l1_cod = CR_1_2 | Unknown of int [@@deriving yojson]

type t2_l1_fec = LDPC_16K | Unknown of int [@@deriving yojson]

type t2_pp = PP1 | PP2 | PP3 | PP4 | PP5 | PP6 | PP7 | PP8 | Unknown of int
[@@deriving yojson]

type t2_version = V1_1_1 | V1_2_1 | V1_3_1 | Unknown of int
[@@deriving yojson]

type t2_plp_type = Common | Data_type_1 | Data_type_2 | Unknown of int
[@@deriving yojson]

type t2_plp_payload_type = GFPS | GCS | GSE | TS | Unknown of int
[@@deriving yojson]

type t2_plp_cod =
  | CR_1_2
  | CR_3_5
  | CR_2_3
  | CR_3_4
  | CR_4_5
  | CR_5_6
  | CR_1_3
  | CR_2_5
  | Unknown of int
[@@deriving yojson]

type t2_plp_mod = QPSK | QAM16 | QAM64 | QAM256 | Unknown of int
[@@deriving yojson]

type t2_plp_fec = LDPC_16K | LDPC_64K | Unknown of int [@@deriving yojson]

type t2_plp_mode = Not_specified | NM | HEM | Unknown of int
[@@deriving yojson]

type t2_aux_stream_type = TX_SIG | Unknown of int [@@deriving yojson]

let rfu_to_string x = Printf.sprintf "Reserved for future use (%d)" x

let t2_streams_type_to_string : t2_streams_type -> string = function
  | TS -> "Transport Stream only"
  | GS -> "Generic Stream but not TS"
  | Both -> "Both TS and Generic Stream"
  | Unknown x -> rfu_to_string x

let t2_gi_to_string = function
  | GI_1_32 -> "1/32"
  | GI_1_16 -> "1/16"
  | GI_1_8 -> "1/8"
  | GI_1_4 -> "1/4"
  | GI_1_128 -> "1/128"
  | GI_19_128 -> "19/128"
  | GI_19_256 -> "19/256"
  | Unknown x -> rfu_to_string x

let t2_papr_legacy_to_string = function
  | Off -> "No PAPR"
  | ACE -> "ACE-PAPR"
  | TR -> "TR-PAPR"
  | ACE_TR -> "ACE and TR PAPR"
  | Unknown x -> rfu_to_string x

let t2_papr_modern_to_string = function
  | L1_ACE_TR_P2 -> "L1-ACE and TR on P2 symbols only"
  | L1_ACE_ACE -> "L1-ACE and ACE only"
  | L1_ACE_TR -> "L1-ACE and TR only"
  | L1_ACE_ACE_TR -> "L1-ACE, ACE and TR"
  | Unknown x -> rfu_to_string x

let t2_papr_to_string = function
  | Legacy x -> t2_papr_legacy_to_string x
  | Modern x -> t2_papr_modern_to_string x

let t2_l1_mod_to_string = function
  | BPSK -> "BPSK"
  | QPSK -> "QPSK"
  | QAM16 -> "16-QAM"
  | QAM64 -> "64-QAM"
  | Unknown x -> rfu_to_string x

let t2_l1_cod_to_string : t2_l1_cod -> string = function
  | CR_1_2 -> "1/2"
  | Unknown x -> rfu_to_string x

let t2_l1_fec_to_string : t2_l1_fec -> string = function
  | LDPC_16K -> "LDPC 16K"
  | Unknown x -> rfu_to_string x

let t2_pp_to_string = function
  | PP1 -> "PP1"
  | PP2 -> "PP2"
  | PP3 -> "PP3"
  | PP4 -> "PP4"
  | PP5 -> "PP5"
  | PP6 -> "PP6"
  | PP7 -> "PP7"
  | PP8 -> "PP8"
  | Unknown x -> rfu_to_string x

let t2_version_to_string = function
  | V1_1_1 -> "1.1.1."
  | V1_2_1 -> "1.2.1"
  | V1_3_1 -> "1.3.1"
  | Unknown x -> rfu_to_string x

let t2_plp_type_to_string = function
  | Common -> "Common PLP"
  | Data_type_1 -> "Data PLP Type 1"
  | Data_type_2 -> "Data PLP Type 2"
  | Unknown x -> rfu_to_string x

let t2_plp_payload_type_to_string = function
  | GFPS -> "GFPS"
  | GCS -> "GCS"
  | GSE -> "GSE"
  | TS -> "TS"
  | Unknown x -> rfu_to_string x

let t2_plp_cod_to_string = function
  | CR_1_2 -> "1/2"
  | CR_3_5 -> "3/5"
  | CR_2_3 -> "2/3"
  | CR_3_4 -> "3/4"
  | CR_4_5 -> "4/5"
  | CR_5_6 -> "5/6"
  | CR_1_3 -> "1/3"
  | CR_2_5 -> "2/5"
  | Unknown x -> rfu_to_string x

let t2_plp_mod_to_string = function
  | QPSK -> "QPSK"
  | QAM16 -> "16-QAM"
  | QAM64 -> "64-QAM"
  | QAM256 -> "256-QAM"
  | Unknown x -> rfu_to_string x

let t2_plp_fec_to_string = function
  | LDPC_16K -> "16K LDPC"
  | LDPC_64K -> "64K LDPC"
  | Unknown x -> rfu_to_string x

let t2_plp_mode_to_string = function
  | Not_specified -> "Not specified"
  | NM -> "Normal mode"
  | HEM -> "High efficiency mode"
  | Unknown x -> rfu_to_string x

let aux_stream_type_to_string = function
  | TX_SIG -> "TX-SIG"
  | Unknown x -> rfu_to_string x
