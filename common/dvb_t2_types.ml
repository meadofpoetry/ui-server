type t2_fft = FFT_1K
            | FFT_2K
            | FFT_4K
            | FFT_8K
            | FFT_16K
            | FFT_32K

type t2_streams_type = TS
                     | GS
                     | Both
                     | Unknown of int [@@deriving yojson]

type t2_gi = GI_1_32
           | GI_1_16
           | GI_1_8
           | GI_1_4
           | GI_1_128
           | GI_19_128
           | GI_19_256
           | Unknown of int [@@deriving yojson]

type t2_papr_legacy = Off
                    | ACE
                    | TR
                    | ACE_TR
                    | Unknown of int [@@deriving yojson]

type t2_papr_modern = L1_ACE_TR_P2
                    | L1_ACE_ACE
                    | L1_ACE_TR
                    | L1_ACE_ACE_TR
                    | Unknown of int [@@deriving yojson]

type t2_papr = Legacy of t2_papr_legacy
             | Modern of t2_papr_modern [@@deriving yojson]

type t2_l1_mod = BPSK
               | QPSK
               | QAM16
               | QAM64
               | Unknown of int [@@deriving yojson]

type t2_l1_cod = CR_1_2
               | Unknown of int [@@deriving yojson]

type t2_l1_fec = LDPC_16K
               | Unknown of int [@@deriving yojson]

type t2_pp = PP1
           | PP2
           | PP3
           | PP4
           | PP5
           | PP6
           | PP7
           | PP8
           | Unknown of int [@@deriving yojson]

type t2_version = V1_1_1
                | V1_2_1
                | V1_3_1
                | Unknown of int [@@deriving yojson]

type t2_plp_type = Common
                 | Data_type_1
                 | Data_type_2
                 | Unknown of int [@@deriving yojson]

type t2_plp_payload_type = GFPS
                         | GCS
                         | GSE
                         | TS
                         | Unknown of int [@@deriving yojson]

type t2_plp_cod = CR_1_2
                | CR_3_5
                | CR_2_3
                | CR_3_4
                | CR_4_5
                | CR_5_6
                | CR_1_3
                | CR_2_5
                | Unknown of int [@@deriving yojson]

type t2_plp_mod = QPSK
                | QAM16
                | QAM64
                | QAM256
                | Unknown of int [@@deriving yojson]

type t2_plp_fec = LDPC_16K
                | LDPC_64K
                | Unknown of int [@@deriving yojson]

type t2_plp_mode = Not_specified
                 | NM
                 | HEM
                 | Unknown of int [@@deriving yojson]

let rfu_to_string x = Printf.sprintf "Reserved for future use (%d)" x

let t2_gi_of_int = function
  | 0b000 -> GI_1_32   | 0b001 -> GI_1_16   | 0b010 -> GI_1_8
  | 0b011 -> GI_1_4    | 0b100 -> GI_1_128  | 0b101 -> GI_19_128
  | 0b110 -> GI_19_256 | x     -> Unknown x

let t2_gi_to_int = function
  | GI_1_32   -> 0b000 | GI_1_16   -> 0b001 | GI_1_8    -> 0b010
  | GI_1_4    -> 0b011 | GI_1_128  -> 0b100 | GI_19_128 -> 0b101
  | GI_19_256 -> 0b110 | Unknown x -> x

let t2_gi_to_string = function
  | GI_1_32   -> "1/32"   | GI_1_16   -> "1/16"          | GI_1_8    -> "1/8"
  | GI_1_4    -> "1/4"    | GI_1_128  -> "1/128"         | GI_19_128 -> "19/128"
  | GI_19_256 -> "19/256" | Unknown x -> rfu_to_string x

let t2_papr_legacy_of_int = function
  | 0b0000 -> Off | 0b0001 -> ACE | 0b0010 -> TR | 0b0011 -> ACE_TR | x -> Unknown x

let t2_papr_legacy_to_int = function
  | Off -> 0b0000 | ACE -> 0b0001 | TR -> 0b0010 | ACE_TR -> 0b0011 | Unknown x -> x

let t2_papr_legacy_to_string = function
  | Off -> "No PAPR"            | ACE -> "ACE-PAPR"            | TR -> "TR-PAPR"
  | ACE_TR -> "ACE and TR PAPR" | Unknown x -> rfu_to_string x

let t2_papr_modern_of_int = function
  | 0b0000 -> L1_ACE_TR_P2 | 0b0001 -> L1_ACE_ACE | 0b0010 -> L1_ACE_TR | 0b0011 -> L1_ACE_ACE_TR | x -> Unknown x

let t2_papr_modern_to_int = function
  | L1_ACE_TR_P2 -> 0b0000 | L1_ACE_ACE -> 0b0001 | L1_ACE_TR -> 0b0010 | L1_ACE_ACE_TR -> 0b0011 | Unknown x -> x

let t2_papr_modern_to_string = function
  | L1_ACE_TR_P2  -> "L1-ACE and TR on P2 symbols only"
  | L1_ACE_ACE    -> "L1-ACE and ACE only"
  | L1_ACE_TR     -> "L1-ACE and TR only"
  | L1_ACE_ACE_TR -> "L1-ACE, ACE and TR"
  | Unknown x     -> rfu_to_string x

let t2_papr_of_int version x =
  match version with
  | V1_1_1 -> Legacy (t2_papr_legacy_of_int x)
  | _      -> Modern (t2_papr_modern_of_int x)

let t2_papr_to_int = function
  | Legacy x -> t2_papr_legacy_to_int x
  | Modern x -> t2_papr_modern_to_int x

let t2_papr_to_string = function
  | Legacy x -> t2_papr_legacy_to_string x
  | Modern x -> t2_papr_modern_to_string x


let t2_l1_mod_of_int = function
  | 0b0000 -> BPSK | 0b0001 -> QPSK | 0b0010 -> QAM16 | 0b0011 -> QAM64 | x -> Unknown x

let t2_l1_mod_to_int = function
  | BPSK -> 0b0000 | QPSK -> 0b0001 | QAM16 -> 0b0010 | QAM64 -> 0b0011 | Unknown x -> x

let t2_l1_mod_to_string = function
  | BPSK -> "BPSK" | QPSK -> "QPSK" | QAM16 -> "16-QAM" | QAM64 -> "64-QAM" | Unknown x -> rfu_to_string x

let t2_l1_cod_of_int : int -> t2_l1_cod = function
  | 0b00 -> CR_1_2 | x -> Unknown x

let t2_l1_cod_to_int : t2_l1_cod -> int = function
  | CR_1_2 -> 0b00 | Unknown x -> x

let t2_l1_cod_to_string : t2_l1_cod -> string = function
  | CR_1_2 -> "1/2" | Unknown x -> rfu_to_string x

let t2_l1_fec_of_int : int -> t2_l1_fec= function
  | 0b00 -> LDPC_16K | x -> Unknown x

let t2_l1_fec_to_int : t2_l1_fec -> int = function
  | LDPC_16K -> 0b00 | Unknown x -> x

let t2_l1_fec_to_string : t2_l1_fec -> string = function
  | LDPC_16K -> "LDPC 16K" | Unknown x -> rfu_to_string x

let t2_pp_of_int = function
  | 0b0000 -> PP1 | 0b0001 -> PP2 | 0b0010 -> PP3 | 0b0011 -> PP4
  | 0b0100 -> PP5 | 0b0101 -> PP6 | 0b0110 -> PP7 | 0b0111 -> PP8
  | x -> Unknown x

let t2_pp_to_int = function
  | PP1 -> 0b0000 | PP2 -> 0b0001 | PP3 -> 0b0010 | PP4 -> 0b0011
  | PP5 -> 0b0100 | PP6 -> 0b0101 | PP7 -> 0b0110 | PP8 -> 0b0111
  | Unknown x -> x

let t2_pp_to_string = function
  | PP1 -> "PP1" | PP2 -> "PP2" | PP3 -> "PP3" | PP4 -> "PP4"
  | PP5 -> "PP5" | PP6 -> "PP6" | PP7 -> "PP7" | PP8 -> "PP8"
  | Unknown x -> rfu_to_string x

let t2_version_of_int = function
  | 0b0000 -> V1_1_1 | 0b0001 -> V1_2_1 | 0b0010 -> V1_3_1 | x -> Unknown x

let t2_version_to_int = function
  | V1_1_1 -> 0b0000 | V1_2_1 -> 0b0001 | V1_3_1 -> 0b0010 | Unknown x -> x

let t2_version_to_string = function
  | V1_1_1 -> "1.1.1." | V1_2_1 -> "1.2.1" | V1_3_1 -> "1.3.1" | Unknown x -> rfu_to_string x

let t2_plp_type_of_int = function
  | 0b000 -> Common | 0b001 -> Data_type_1 | 0b010 -> Data_type_2 | x -> Unknown x

let t2_plp_type_to_int = function
  | Common -> 0b000 | Data_type_1 -> 0b001 | Data_type_2 -> 0b010 | Unknown x -> x

let t2_plp_type_to_string = function
  | Common      -> "Common PLP"      | Data_type_1 -> "Data PLP Type 1"
  | Data_type_2 -> "Data PLP Type 2" | Unknown x   -> rfu_to_string x

let t2_plp_payload_type_of_int = function
  | 0b00000 -> GFPS | 0b00001 -> GCS | 0b00010 -> GSE | 0b00011 -> TS | x -> Unknown x

let t2_plp_payload_type_to_int = function
  | GFPS -> 0b00000 | GCS -> 0b00001 | GSE -> 0b00010 | TS -> 0b00011 | Unknown x -> x

let t2_plp_payload_type_to_string = function
  | GFPS -> "GFPS" | GCS -> "GCS" | GSE -> "GSE" | TS -> "TS" | Unknown x -> rfu_to_string x
