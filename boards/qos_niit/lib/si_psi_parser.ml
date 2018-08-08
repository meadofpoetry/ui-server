open Containers

let value_to_string name x = Printf.sprintf "%s (%d)" name x
let rfu_to_string          = value_to_string "Reserved"

type parsed = node list
and node =
  { offset : int
  ; length : int
  ; name   : string
	; value  : value
  }
and value =
  | List    of node list
  | Flag    of bool
  | Bytes   of string
  | String  of string
  | Int     of int
  | Int32   of int32
  | Int64   of int64
  | Name    of string * int
  | Val_hex of int [@@deriving yojson]

let to_node ~offset length name value =
  { offset; length; name; value }

let parsed_length = List.fold_left (fun acc x -> x.length + acc) 0

module Descriptor = struct

  module type Descriptor_base = sig

    type t [@@deriving yojson]
    val name   : string
    val decode : Bitstring.t -> parsed

  end

  (* module Video_stream : Descriptor_base = struct
   * 
   *   type frame_rate = Forbidden
   *                   | FR_23_976
   *                   | FR_24
   *                   | FR_25
   *                   | FR_29_97
   *                   | FR_30
   *                   | FR_50
   *                   | FR_59_94
   *                   | FR_60
   *                   | Reserved of int [@@deriving yojson]
   * 
   *   type chroma_format = CHROMA_4_2_0
   *                      | CHROMA_4_2_2
   *                      | CHROMA_4_4_4
   *                      | Reserved of int [@@deriving yojson]
   * 
   *   type mpeg_1_only =
   *     { profile_and_level_indication : int
   *     ; chroma_format                : chroma_format
   *     ; frame_rate_extension_flag    : bool
   *     ; reserved                     : int
   *     } [@@deriving yojson]
   * 
   *   type t =
   *     { mfr_flag                   : bool
   *     ; frame_rate                 : frame_rate
   *     ; mpeg_1_only_flag           : bool
   *     ; constrained_parameter_flag : bool
   *     ; still_picture_flag         : bool
   *     ; mpeg_1_only                : mpeg_1_only option
   *     } [@@deriving yojson]
   * 
   *   let name = "video_stream_descriptor"
   * 
   *   let frame_rate_of_int = function
   *     | 0b0000 -> Forbidden  | 0b0001 -> FR_23_976  | 0b0010 -> FR_24
   *     | 0b0011 -> FR_25      | 0b0100 -> FR_29_97   | 0b0101 -> FR_30
   *     | 0b0110 -> FR_50      | 0b0111 -> FR_59_94   | 0b1000 -> FR_60
   *     | x      -> Reserved x
   * 
   *   let frame_rate_to_int = function
   *     | Forbidden  -> 0b0000 | FR_23_976 -> 0b0001 | FR_24 -> 0b0010
   *     | FR_25      -> 0b0011 | FR_29_97  -> 0b0100 | FR_30 -> 0b0101
   *     | FR_50      -> 0b0110 | FR_59_94  -> 0b0111 | FR_60 -> 0b1000
   *     | Reserved x -> x
   * 
   *   let frame_rate_to_string = function
   *     | Forbidden  -> "Forbidden" | FR_23_976 -> "23.976" | FR_24 -> "24"
   *     | FR_25      -> "25"        | FR_29_97  -> "29.97"  | FR_30 -> "30"
   *     | FR_50      -> "50"        | FR_59_94  -> "59.94"  | FR_60 -> "60"
   *     | Reserved x -> rfu_to_string x
   * 
   *   let frame_rate_mfr_to_string mfr fr =
   *     let another = if not mfr then []
   *                   else (match fr with
   *                         | FR_24    -> [FR_23_976]
   *                         | FR_29_97 -> [FR_23_976]
   *                         | FR_30    -> [FR_23_976; FR_24; FR_29_97]
   *                         | FR_50    -> [FR_25]
   *                         | FR_59_94 -> [FR_23_976; FR_29_97]
   *                         | FR_60    -> [FR_23_976; FR_24; FR_29_97; FR_30; FR_59_94]
   *                         | _        -> []) in
   *     frame_rate_to_string fr
   *     |> fun x -> match another with
   *                 | [] -> x
   *                 | l  -> let s = String.concat ", " (List.map (fun x -> frame_rate_to_string x) l) in
   *                         x ^ (Printf.sprintf " (Also includes %s)" s)
   * 
   * 
   *   let chroma_format_of_int = function
   *     | 0b01 -> CHROMA_4_2_0 | 0b10 -> CHROMA_4_2_2
   *     | 0b11 -> CHROMA_4_4_4 | x    -> Reserved x
   * 
   *   let chroma_format_to_int = function
   *     | CHROMA_4_2_0 -> 0b01 | CHROMA_4_2_2 -> 0b10
   *     | CHROMA_4_4_4 -> 0b11 | Reserved x   -> x
   * 
   *   let chroma_format_to_string = function
   *     | CHROMA_4_2_0 -> "4:2:0" | CHROMA_4_2_2 -> "4:2:2"
   *     | CHROMA_4_4_4 -> "4:4:4" | Reserved x   -> rfu_to_string x
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| mfr_flag : 1
   *        ; frame_rate : 4
   *        ; mpeg_1_only_flag : 1
   *        ; constrained_parameter_flag : 1
   *        ; still_picture_flag : 1
   *        |} when not mpeg_1_only_flag ->
   *        { mfr_flag; frame_rate = frame_rate_of_int frame_rate;
   *          mpeg_1_only_flag; constrained_parameter_flag;
   *          still_picture_flag; mpeg_1_only = None }
   *     | {| mfr_flag : 1
   *        ; frame_rate : 4
   *        ; mpeg_1_only_flag : 1
   *        ; constrained_parameter_flag : 1
   *        ; still_picture_flag : 1
   *        ; profile_and_level_indication : 8
   *        ; chroma_format: 2
   *        ; frame_rate_extension_flag : 1
   *        ; reserved : 5
   *        |} ->
   *        { mfr_flag; frame_rate = frame_rate_of_int frame_rate;
   *          mpeg_1_only_flag; constrained_parameter_flag; still_picture_flag;
   *          mpeg_1_only = Some { profile_and_level_indication
   *                             ; chroma_format = chroma_format_of_int chroma_format
   *                             ; frame_rate_extension_flag
   *                             ; reserved }}
   * 
   * end
   * 
   * module Audio_stream : Descriptor_base = struct
   * 
   *   type t =
   *     { free_format_flag : bool
   *     ; id               : bool
   *     ; layer            : int
   *     ; variable_rate    : bool
   *     ; reserved         : int
   *     } [@@deriving yojson]
   * 
   *   let name = "audio_stream_descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| free_format_flag : 1
   *        ; id               : 1
   *        ; layer            : 2
   *        ; variable_rate    : 1
   *        ; reserved         : 3
   *        |} -> { free_format_flag; id; layer; variable_rate; reserved }
   * 
   * end
   * 
   * module Hierarchy : Descriptor_base = struct
   * 
   *   type hierarchy = Reserved of int
   *                  | Spatial_scalability
   *                  | SNR_scalability
   *                  | Temporal_scalability
   *                  | Data_partitioning
   *                  | Extension_bitstream
   *                  | Private_stream
   * 
   * 
   * 
   *                  | Multi_view_profile
   *                  | Combined_scalability
   *                  | MVC_or_MVCD
   *                  | Base_layer_etc [@@deriving yojson]
   * 
   *   type t =
   *     { reserved_1                     : bool
   *     ; temporal_scalability           : bool
   *     ; spatial_scalability            : bool
   *     ; quality_scalability            : bool
   *     ; hierarchy_type                 : hierarchy
   *     ; reserved_2                     : int
   *     ; hierarchy_layer_index          : int
   *     ; tref_present_flag              : bool
   *     ; reserved_3                     : bool
   *     ; hierarchy_embedded_layer_index : int
   *     ; reserved_4                     : int
   *     ; hierarchy_channel              : int
   *     } [@@deriving yojson]
   * 
   *   let name = "Hierarchy descriptor"
   * 
   *   let hierarchy_of_int = function
   *     | 1  -> Spatial_scalability | 2  -> SNR_scalability      | 3  -> Temporal_scalability
   *     | 4  -> Data_partitioning   | 5  -> Extension_bitstream  | 6  -> Private_stream
   * 
   *     | 7  -> Multi_view_profile  | 8  -> Combined_scalability | 9  -> MVC_or_MVCD
   *     | 15 -> Base_layer_etc      | x  -> Reserved x
   * 
   *   let hierarchy_to_int = function
   *     | Reserved x           -> x | Spatial_scalability -> 1 | SNR_scalability -> 2 | Temporal_scalability -> 3
   *     | Data_partitioning    -> 4 | Extension_bitstream -> 5 | Private_stream  -> 6 | Multi_view_profile   -> 7
   *     | Combined_scalability -> 8 | MVC_or_MVCD -> 9         | Base_layer_etc  -> 15
   * 
   *   let hierarchy_to_string = function
   *     | Reserved x           -> rfu_to_string x
   *     | Spatial_scalability  -> "Spatial Scalability"
   * 
   *     | SNR_scalability      -> "SNR Scalability"
   *     | Temporal_scalability -> "Temporal Scalability"
   *     | Data_partitioning    -> "Data partitioning"
   * 
   *     | Extension_bitstream  -> "Extension bitstream"
   *     | Private_stream       -> "Private Stream"
   *     | Multi_view_profile   -> "Multi-view Profile"
   *     | Combined_scalability -> "Combined Scalability"
   *     | MVC_or_MVCD          -> "MVC video sub-bitstream or MVCD video sub-bitstream"
   *     | Base_layer_etc       -> "Base layer or MVC base view sub-bitstream or AVC video \
   *                                sub-bitstream of MVC or HEVC temporal video sub-bitstream or Base \
   *                                layer of MVCD base view sub-bitstream or AVC video sub-bitstream \
   *                                of MVCD"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| reserved_1                     : 1
   *        ; temporal_scalability           : 1
   *        ; spatial_scalability            : 1
   *        ; quality_scalability            : 1
   *        ; hierarchy_type                 : 4
   *        ; reserved_2                     : 2
   *        ; hierarchy_layer_index          : 6
   *        ; tref_present_flag              : 1
   *        ; reserved_3                     : 1
   *        ; hierarchy_embedded_layer_index : 6
   *        ; reserved_4                     : 2
   *        ; hierarchy_channel              : 6
   *        |} ->
   *        { reserved_1; temporal_scalability; spatial_scalability;
   *          quality_scalability; hierarchy_type = hierarchy_of_int hierarchy_type;
   *          reserved_2; hierarchy_layer_index; tref_present_flag; reserved_3;
   *          hierarchy_embedded_layer_index;
   *          reserved_4; hierarchy_channel }
   * 
   * end
   * 
   * module Registration : Descriptor_base = struct
   * 
   *   type t =
   *     { format_identifier              : int32
   *     ; additional_identification_info : string
   *     } [@@deriving yojson]
   * 
   *   let name = "Registration descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| format_identifier : 32
   *        ; rest              : -1 : bitstring
   *        |} -> { format_identifier; additional_identification_info = Bitstring.string_of_bitstring rest }
   * 
   * end
   * 
   * module Data_stream_alignment : Descriptor_base = struct
   * 
   *   type t = { alignment_type : int } [@@deriving yojson]
   * 
   *   let name = "Data stream alignment descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| alignment_type : 8 |} -> { alignment_type }
   * 
   * end
   * 
   * module Target_background_grid : Descriptor_base = struct
   * 
   *   type t =
   *     { horizontal_size          : int
   *     ; vertical_size            : int
   *     ; aspect_ratio_information : int
   *     } [@@deriving yojson]
   * 
   *   let name = "Target background grid descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| horizontal_size          : 14
   *        ; vertical_size            : 14
   *        ; aspect_ratio_information : 4
   *        |} -> { horizontal_size; vertical_size; aspect_ratio_information }
   * 
   * end
   * 
   * module Video_window : Descriptor_base = struct
   * 
   *   type t =
   *     { horizontal_offset : int
   *     ; vertical_offset   : int
   *     ; window_priority   : int
   *     } [@@deriving yojson]
   * 
   *   let name = "Video window descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| horizontal_offset : 14
   *        ; vertical_offset   : 14
   *        ; window_priority   : 4
   *        |} -> { horizontal_offset; vertical_offset; window_priority }
   * 
   * end
   * 
   * module CA : Descriptor_base = struct
   * 
   *   type t =
   *     { ca_system_id : int
   *     ; reserved     : int
   *     ; ca_pid       : int
   *     ; private_data : string
   *     } [@@deriving yojson]
   * 
   *   let name = "Conditional access descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| ca_system_id : 16
   *        ; reserved     : 3
   *        ; ca_pid       : 13
   *        ; private_data : -1 : bitstring
   *        |} -> { ca_system_id; reserved; ca_pid; private_data = Bitstring.string_of_bitstring private_data }
   * 
   * end
   * 
   * module ISO_639_language : Descriptor_base = struct
   * 
   *   type audio_type = Undefined
   *                   | Clean_effects
   *                   | Hearing_impaired
   *                   | Visual_impaired_commentary
   *                   | User_private of int
   *                   | Reserved of int [@@deriving yojson]
   * 
   *   type audio_language =
   *     { iso_639_language_code : int
   *     ; audio_type            : audio_type
   *     } [@@deriving yojson]
   * 
   *   type t = audio_language list [@@deriving yojson]
   * 
   *   let audio_type_of_int = function
   *     | 0x00                          -> Undefined
   *     | 0x01                          -> Clean_effects
   *     | 0x02                          -> Hearing_impaired
   *     | 0x03                          -> Visual_impaired_commentary
   *     | x when x >= 0x04 && x <= 0x7F -> User_private x
   *     | x                             -> Reserved x
   * 
   *   let audio_type_to_int = function
   *     | Undefined      -> 0 | Clean_effects -> 1 | Hearing_impaired -> 2 | Visual_impaired_commentary -> 3
   *     | User_private x -> x | Reserved x    -> x
   * 
   *   let audio_type_to_string = function
   *     | Undefined                  -> "Undefined"
   *     | Clean_effects              -> "Clean effects"
   *     | Hearing_impaired           -> "Hearing impaired"
   *     | Visual_impaired_commentary -> "Visual impaired commentary"
   *     | User_private _             -> "User private"
   *     | Reserved _                 -> "Reserved"
   * 
   *   let name = "ISO 639 language descriptor"
   * 
   *   let lang_code_to_string x =
   *     List.map char_of_int [ (x lsr 16) land 0xFF; (x lsr 8) land 0xFF; x land 0xFF ] |> String.of_list
   * 
   *   let decode bs =
   *     let rec f  = fun acc x ->
   *       if Bitstring.bitstring_length x = 0 then List.rev acc
   *       else (match%bitstring bs with
   *             | {| iso_639_language_code : 24
   *                ; audio_type            : 8
   *                ; rest                  : -1 : bitstring
   *                |} ->
   *                f ({ iso_639_language_code
   *                   ; audio_type = audio_type_of_int audio_type }
   *                   :: acc) rest) in
   *     f [] bs
   * 
   * end
   * 
   * (\* 11 *\)
   * module System_clock : Descriptor_base = struct
   * 
   *   let name = "System clock descriptor"
   * 
   *   let external_clock_ref_indicator_of_bool ecri =
   *     if ecri then "system clock has derived" else "system clock has not derived"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| external_clock_ref_indicator : 1 : save_offset_to (off_1)
   *        ; reserved_1                   : 1 : save_offset_to (off_2)
   *        ; clock_accuracy_integer       : 6 : save_offset_to (off_3)
   *        ; clock_accuracy_exponent      : 3 : save_offset_to (off_4)
   *        ; reserved_2                   : 5 : save_offset_to (off_5)
   *        |} -> [ { offset = off_1
   *                ; length = 1
   *                ; name   = "external clock reference indicator"
   *                ; value  = Flag external_clock_ref_indicator
   *                ; nested = []
	 *                }
   *              ; { offset = off_2
   *                ; length = 1
   *                ; name   = "reserved"
   *                ; value  = Flag reserved_1
   *                ; nested = []
	 *                }
   *              ; { offset = off_3
   *                ; length = 6
   *                ; name   =  "clock_accuracy_integer"
   *                ; value  = Int clock_accuracy_integer
   *                ; nested = []
	 *                }
   *              ; { offset = off_4
   *                ; length = 3
   *                ; name   = "clock_accuracy_exponent"
   *                ; value  = Int clock_accuracy_exponent
   *                ; nested = []
	 *                }
   *              ; { offset = off_1
   *                ; length = 5
   *                ; name   = "reserved"
   *                ; value  = Int reserved_2
   *                ; nested = []
	 *                }
	 *              ]
   * 
   * end
   * 
   * (\* 12 *\)
   * module Multiplex_buffer_utilization : Descriptor_base = struct
   * 
   *   type t =
   *     { bound_valid_flag                   : bool
   *     ; ltw_offset_lower_bound             : int
   *     ; reserved                           : bool
   *     ; ltw_offset_upper_bound             : int
   *     } [@@deriving yojson]
   * 
   *   let name = "Multiplex buffer utilization"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| bound_valid_flag       : 1  : save_offset_to (off_1)
   *        ; ltw_offset_lower_bound : 15 : save_offset_to (off_2)
   *        ; reserved               : 1  : save_offset_to (off_3)
   *        ; ltw_offset_upper_bound : 15 : save_offset_to (off_4)
   *        |} -> 
   *     { (bound_valid_flag, off_1) ; (ltw_offset_lower_bound, off_2)
   *     ; (reserved, off_3)         ; (ltw_offset_upper_bound, off_4)
   *     }
   * 
   * end		 
   * 
   * (\* 13 *\)
   * module Copyright : Descriptor_base = struct
   * 
   *   type t =
   *     { copyright_identifier      : int32
   *     ; additional_copyright_info : bool
   *     } [@@deriving yojson]
   * 
   *   let name = "Copyright"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| copyright_identifier : 32 : save_offset_to (off_1)
   *        ; rest                 : -1 : bitstring
   *        |} -> { (copyright_identifier, off_1)
	 *        ; additional_identification_info = Bitstring.string_of_bitstring rest
	 *        }
   * 
   * end
   * 
   * (\* 14 *\)
   * module Maximum_bitrate : Descriptor_base = struct
   * 
   *   type t =
   *     { reserved        : bool
   *     ; maximum_bitrate : int
   *     } [@@deriving yojson]
   * 
   *   let name = "Maximum bitrate"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| reserved        : 2  : save_offset_to (off1)
   *        ; maximum_bitrate : 22 : save_offset_to (off2)
   *        |} -> 
   *     { (reserved, off1) ; (maximum_bitrate, off2) }
   * 
   * end	       
   * 
   * (\* 15 *\)
   * module Private_data_indicator : Descriptor_base = struct
   * 
   *   type t =
   *     { private_data_indicator : int32
   *     } [@@deriving yojson]
   * 
   *   let name = "Private data indicator"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| private_data_indicator : 32 : save_offset_to (off_1)
   *        |} -> 
   *     { (private_data_indicator, off_1) }
   * 
   * end
   * 
   * (\* 16 *\)
   * module Smoothing_buffer : Descriptor_base = struct
   * 
   *   type t =
   *     { reserved_1   : bool
   *     ; sb_leak_rate : int
   *     ; reserved_2   : bool
   *     ; sb_size      : int
   *     } [@@deriving yojson]
   * 
   *   let name = "Smoothing buffer"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| reserved_1   : 2  : save_offset_to (off_1)
   *        ; sb_leak_rate : 22 : save_offset_to (off_2)
   *        ; reserved_2   : 2  : save_offset_to (off_3)
   *        ; sb_size      : 22 : save_offset_to (off_4)
   *        |} -> 
   *     { (reserved_1, off_1) ; (sb_leak_rate, off_2) ; (reserved_2, off_3) ; (sb_size, off_4) }
   * 
   * end
   * 
   * (\* 17 *\)
   * module STD_descriptor : Descriptor_base = struct
   * 
   *   type t =
   *     { reserved        : bool
   *     ; leak_valid_flag : bool
   *     } [@@deriving yojson]
   * 
   *   let name = "STD descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| reserved        : 7 : save_offset_to (off_1)
   *        ; leak_valid_flag : 1 : save_offset_to (off_2)
   *        |} -> 
   *     { (reserved, off_1) ; (leak_valid_flag, off_2) }
   * 
   * end
   * 
   * (\* 18 *\)
   * module IBP_descriptor : Descriptor_base = struct
   * 
   *   type t =
   *     { closed_gop_flag    : int
   *     ; identical_gop_flag : int
   *     ; max_gop_length     : int
   *     } [@@deriving yojson]
   * 
   *   let name = "IBP descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| closed_gop_flag    : 1  : save_offset_to (off_1)
   *        ; identical_gop_flag : 1  : save_offset_to (off_2)
   *        ; max_gop_length     : 14 : save_offset_to (off_3)
   *        |} -> 
   *     { (closed_gop_flag, off_1) ; (identical_gop_flag, off_2) ; (max_gop_length, off_3) }
   * 
   * end
   * 
   * (\* 27 *\)
   * module MPEG_4_video : Descriptor_base = struct
   * 
   *   type t =
   *     { mpeg_4_visual : int
   *     } [@@deriving yojson]
   * 
   *   let name = "MPEG 4 video"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| mpeg_4_visual : 8 : save_offset_to (off_1)
   *        |} -> 
   *     { (mpeg_4_visual, off_1) }
   * 
   * end
   * 
   * (\* 28 *\)
   * module MPEG_4_audio : Descriptor_base = struct
   * 
   *   let name = "MPEG 4 audio"
   * 
   *   let audio_value_of_int = function
   *     | 0x0F -> "No_audio_profile"
   *     | 0x10 -> "Main_1"
   *     | 0x11 -> "Main_2"
   *     | 0x12 -> "Main_3"
   *     | 0x13 -> "Main_4"
   *     | 0x18 -> "Scalable_1"
   *     | 0x19 -> "Scalable_2"
   *     | 0x1A -> "Scalable_3"
   *     | 0x1B -> "Scalable_4"
   *     | 0x20 -> "Speech_1"
   *     | 0x21 -> "Speech_2"
   *     | 0x28 -> "Synthesis_1"
   *     | 0x29 -> "Synthesis_2"
   *     | 0x2A -> "Synthesis_3"
   *     | 0x30 -> "HQ_1"
   *     | 0x31 -> "HQ_2"
   *     | 0x32 -> "HQ_3"
   *     | 0x33 -> "HQ_4"
   *     | 0x34 -> "HQ_5"
   *     | 0x35 -> "HQ_6"
   *     | 0x36 -> "HQ_7"
   *     | 0x37 -> "HQ_8"
   *     | 0x38 -> "Low_delay_1"
   *     | 0x39 -> "Low_delay_2"
   *     | 0x3A -> "Low_delay_3"
   *     | 0x3B -> "Low_delay_4"
   *     | 0x3C -> "Low_delay_5"
   *     | 0x3D -> "Low_delay_6"
   *     | 0x3E -> "Low_delay_7"
   *     | 0x3F -> "Low_delay_8"
   *     | 0x40 -> "Natural_1"
   *     | 0x41 -> "Natural_2"
   *     | 0x42 -> "Natural_3"
   *     | 0x43 -> "Natural_4"
   *     | 0x48 -> "Mobile_1"
   *     | 0x49 -> "Mobile_2"
   *     | 0x4A -> "Mobile_3"
   *     | 0x4B -> "Mobile_4"
   *     | 0x4C -> "Mobile_5"
   *     | 0x4D -> "Mobile_6"
   *     | 0x50 -> "AAC_1"
   *     | 0x51 -> "AAC_2"
   *     | 0x52 -> "AAC_4"
   *     | 0x53 -> "AAC_5"
   *     | 0x58 -> "HE_AAC_2"
   *     | 0x59 -> "HE_AAC_3"
   *     | 0x5A -> "HE_AAC_4"
   *     | 0x5B -> "HE_AAC_5"
   *     | 0x60 -> "HE_AAC_v2_2"
   *     | 0x61 -> "HE_AAC_v2_3"
   *     | 0x62 -> "HE_AAC_v2_4"
   *     | 0x63 -> "HE_AAC_v2_5"
   *     | 0xFF -> "Not_specified"
   *     | x    -> "Reserved x"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| mpeg_4_audio : 8 : save_offset_to (off_1)
   *        |} -> 
   *     { ((mpeg_4_audio = audio_value_of_int mpeg_4_audio), off_1) }
   * 
   * end
   * 
   * (\* 29 *\)
   * module IOD_descriptor : Descriptor_base = struct
   * 
   *   type t =
   *     { scope_of_iod_label        : int
   *     ; iod_label                 :
   *     ; initial_object_descriptor :
   *     } [@@deriving yojson]
   * 
   *   let name = "IOD descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| scope_of_iod_label        : 8 : save_offset_to (off_1)
   *        ; iod_label                 : 8 : save_offset_to (off_2) 
   *        ; initial_object_descriptor : 8 : save_offset_to (off_3) 
   *        |} -> 
   *     { (scope_of_iod_label, off_1) ; (iod_label, off_2) ; (initial_object_descriptor, off_3 }
   * 
   * end
   * 
   * (\* 30 *\)
   * module SL_descriptor : Descriptor_base = struct
   * 
   *   type t =
   *     { es_id : int
   *     } [@@deriving yojson]
   * 
   *   let name = "SL descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| es_id : 16 : save_offset_to (off_1)
   *        |} -> 
   *     { (es_id, off_1) }
   * 
   * end
   * 
   * (\* 31 *\)
   * module FMC_descriptor : Descriptor_base = struct
   * 
   *   type t =
   *     { es_id            : int
   *     ; flex_mux_channel : 
   *     } [@@deriving yojson]
   * 
   *   let name = "FMC descriptor"
   * 
   *   let decode bs =
   *     let rec f  = fun acc x ->
   *       if Bitstring.bitstring_length x = 0 then List.rev acc
   *       else (match%bitstring bs with
   *             | {| es_id            : 16 : save_offset_to (off_1)
   *                ; flex_mux_channel : 8  : save_offset_to (off_2)
   *                ; rest             : -1 : bitstring
   *                |} ->
   *                f ({ (es_id, off_1)
   *                   ; (flex_mux_channel, off_2) }
   *                   :: acc) rest) in
   *     f [] bs
   * 
   * end     
   * 
   * (\* 32 *\)
   * module ES_ID_descriptor : Descriptor_base = struct
   * 
   *   type t =
   *     { external_es_id : int
   *     } [@@deriving yojson]
   * 
   *   let name = "External_ES_ID descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| external_es_id : 16 : save_offset_to (off_1)
   *        |} -> 
   *     { (external_es_id, off_1) }
   * 
   * end
   * 
   * (\* 35 *\)
   * module Multiplex_buffer : Descriptor_base = struct
   * 
   *   type t =
   *     { mb_buffer_size : int
   *     ; tb_leak_rate   : 
   *     } [@@deriving yojson]
   * 
   *   let name = "MultiplexBuffer descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| md_buffer_size : 24 : save_offset_to (off_1)
   *        ; tb_leak_rate   : 24 : save_offset_to (off_2)
   *        |} -> 
   *     { (md_buffer_size, off_1) ; (tb_leak_rate, off_2) }
   * 
   * end
   * 
   * (\* 36 *\)
   * module Flex_mux_timing : Descriptor_base = struct
   * 
   *   type t =
   *     { fcr_es_id       :
   *     ; fcr_resolution  : 
   *     ; fcr_length      : 
   *     ; fmx_rate_length : 
   *     } [@@deriving yojson]
   * 
   *   let name = "FlexMuxTiming descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| fcr_es_id       : 16 : save_offset_to (off_1)
   *        ; fcr_resolution  : 32 : save_offset_to (off_2)
   *        ; fcr_length      : 8  : save_offset_to (off_3)
   *        ; fmx_rate_length : 8  : save_offset_to (off_4)
   *        |} -> 
	 *  { (fcr_es_id, off_1) ; (fcr_resolution, off_2)
	 *  ; (fcr_length, off_3) ; (fmx_rate_length, off_4)
	 *  }
   * 
   * end
   * 
   * (\* 36 *\)
   * module Flex_mux_timing : Descriptor_base = struct
   * 
   *   type t =
   *     { fcr_es_id       :
   *     ; fcr_resolution  : 
   *     ; fcr_length      : 
   *     ; fmx_rate_length : 
   *     } [@@deriving yojson]
   * 
   *   let name = "FlexMuxTiming descriptor"
   * 
   *   let decode bs =
   *     match%bitstring bs with
   *     | {| fcr_es_id       : 16 : save_offset_to (off_1)
   *        ; fcr_resolution  : 32 : save_offset_to (off_2)
   *        ; fcr_length      : 8  : save_offset_to (off_3)
   *        ; fmx_rate_length : 8  : save_offset_to (off_4)
   *        |} -> 
	 *  { (fcr_es_id, off_1)  ; (fcr_resolution, off_2)
	 *  ; (fcr_length, off_3) ; (fmx_rate_length, off_4)
	 *  }
   * 
   * end	  *)

  module Unknown : Descriptor_base = struct

    type t = string [@@deriving yojson]

    let name = "unknown descriptor"

    let decode _ = []

  end

  type t =
    { tag     : int
    ; length  : int
    ; name    : string
    ; content : parsed
    } [@@deriving yojson]

  let decode tag length body =
    let name,content =
      (match tag with
       (* | 02 -> Video_stream.name, Video_stream.decode body
        * | 03 -> Audio_stream.name, Audio_stream.decode body
        * | 04 -> Hierarchy.name, Hierarchy.decode body
        * | 05 -> Registration.name, Registration.decode body
        * | 06 -> Data_stream_alignment.name, Data_stream_alignment.decode body
        * | 07 -> Target_background_grid.name,Target_background_grid.decode body
        * | 08 -> Video_window.name, Video_window.decode body
        * | 09 -> CA.name, CA.decode body
        * | 10 -> ISO_639_language.name, ISO_639_language.decode body
        * | 11 -> System_clock.name, System_clock.decode body
        * | 12 -> Multiplex_buffer_utilization.name, Multiplex_buffer_utilization.decode body
        * | 13 -> Copyright.name, Copyright.decode body
        * | 14 -> Maximum_bitrate.name, Maximum_bitrate.decode body
        * | 15 -> Private_data_indicator.name, Private_data_indicator.decode body
        * | 16 -> Smoothing_buffer.name, Smoothing_buffer.decode body
        * | 17 -> STD_descriptor.name, STD_descriptor.decode body
        * | 18 -> IBP_descriptor.name, IBP_descriptor.decode body
        * | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 -> Unknown.name, Unknown.decode body
        * | 27 -> MPEG_4_video.name, MPEG_4_video.decode body
        * | 28 -> MPEG_4_audio.name, MPEG_4_audio.decode body
        * | 29 -> IOD_descriptor.name, IOD_descriptor.decode body
        * | 30 -> SL_descriptor.name, SL_descriptor.decode body
        * | 31 -> FMC_descriptor.name, FMC_descriptor.decode body
        * | 32 -> ES_ID_descriptor.name, ES_ID_descriptor.decode body
        * | 33 -> MuxCode.name, MuxCode.decode body
        * | 34 -> Fmx_buffer_size.name, Fmx_buffer_size.decode body
        * | 35 -> Multiplex_buffer.name, Multiplex_buffer.decode body
        * | 36 -> Content_labeling.name, Content_labeling.decode body
        * | 37 -> Metadata_pointer.name, Metadata_pointer.decode body
        * | 38 -> Metadata.name, Metadata.decode body
        * | 39 -> Metadata_STD.name, Metadata_std.decode body
        * | 40 -> Avc_video.name, Avc_video.decode body
        * | 41 -> Ipmp.name, Ipmp.decode body
        * | 42 -> Avc_hrd.name, Avc_hrd.decode body
        * | 43 -> Mpeg2_aac_audio.name, Mpeg2_aac_audio.decode body
        * | 44 -> Flex_mux_timing.name, Flex_mux_timing.decode body
        * | 45 -> Mpeg4_text.name, Mpeg4_text.decode body
        * | 46 -> Mpeg4_audio_ext.name, Mpeg4_audio_ext.decode body
        * | 47 -> Aux_video_stream.name, Aux_video_stream.decode body
        * | 48 -> Svc_ext.name, Svc_ext.decode body
        * | 49 -> Mvc_ext.name, Mvc_ext.decode body
        * | 50 -> J2k_video.name, J2k_video.decode body
        * | 51 -> Mvc_operation_point.name, Mvc_operation_point.decode body
        * | 52 -> Mpeg2_stereo_format.name, Mpeg2_stereo_format.decode body
        * | 53 -> Stereo_program_info.name, Stereo_program_info.decode body
        * | 54 -> Stereo_video_info.name, Stereo_video_info.decode body
        * | 55 -> Transport_profile.name, Transport_profile.decode body
        * | 56 -> Hevc_video.name, Hevc_video.decode body
        * | 57 | 58 | 59 | 60 | 61 | 62 -> Unknown.name, Unknown.decode body
        * | 63 -> Extension_descriptor.name, Extension_descriptor.decode body *)
       | _  -> Unknown.name, Unknown.decode body) in
    { tag; length; name; content }

  let of_bitstring bs =
    match%bitstring bs with
    | {| tag    : 8
       ; length : 8
       ; body   : length * 8 : bitstring
       ; rest   : -1 : bitstring
       |} -> (decode tag length body),rest

end

module Table_common = struct

  let parse_header bs =
    match%bitstring bs with
    | {| table_id       : 8  : save_offset_to (off_1)
       ; ssi            : 1  : save_offset_to (off_2)
       ; rfu            : 1  : save_offset_to (off_3)
       ; reserved_1     : 2  : save_offset_to (off_4)
       ; section_length : 12 : save_offset_to (off_5)
       |} ->
       [ to_node ~offset:off_1 8  "table_id"        (Val_hex table_id)
       ; to_node ~offset:off_2 1  "section_syntax_indicator" (Flag ssi)
       ; to_node ~offset:off_3 1  "'0'"             (Flag rfu)
       ; to_node ~offset:off_4 2  "reserved"        (Val_hex reserved_1)
       ; to_node ~offset:off_5 12 "section_length"  (Int section_length)
       ]

  let rec parse_descriptors = fun _ _ _ -> []
    (* fun acc x ->
     * if Bitstring.bitstring_length x = 0 then List.rev acc
     * else let descr,rest = Descriptor.of_bitstring x in
     *      parse_descriptors (descr :: acc) rest *)

  let rec parse_ts = fun off acc x ->
    if Bitstring.bitstring_length x = 0 then List.rev acc
    else
      match%bitstring x with
      | {| ts_id       : 16 : save_offset_to (off_1)
         ; on_id       : 16 : save_offset_to (off_2)
         ; rfu         : 4  : save_offset_to (off_3)
         ; length      : 12 : save_offset_to (off_4)
         ; descriptors : length * 8 : save_offset_to (off_5), bitstring
         ; rest        : -1 : save_offset_to (off_6), bitstring
         |} ->
         let descriptors = parse_descriptors off_6 [] descriptors in
         let nodes =
           [ to_node ~offset:(off_1 + off) 16 "transport_stream_id" (Int ts_id)
           ; to_node ~offset:(off_2 + off) 16 "original_network_id" (Int on_id)
           ; to_node ~offset:(off_3 + off) 4  "reserved_future_use" (Int rfu)
           ; to_node ~offset:(off_4 + off) 12 "transport_descriptors_length" (Int length)
           ; to_node ~offset:(off_5 + off) (length * 8) "descriptors" (List descriptors)
           ] in
         (* FIXME *)
         parse_ts (off_6 + off) (nodes @ acc) rest

end

module PAT = struct

  open Table_common
  open Bitstring

  let rec parse_programs = fun off acc x ->
    let to_name = function
      | 0 -> "network_PID"
      | _ -> "program_map_PID" in
    if bitstring_length x = 0 then List.rev acc
    else
      match%bitstring x with
      | {| id       : 16
         ; reserved : 3  : save_offset_to (off_1)
         ; pid      : 13 : save_offset_to (off_2)
         ; rest     : -1 : save_offset_to (off_3), bitstring
         |} ->
         let nodes =
           [ to_node ~offset:off 16 "program_number" (Int id)
           ; to_node ~offset:(off_1 + off) 3 "reserved" (Val_hex reserved)
           ; to_node ~offset:(off_2 + off) 13 (to_name id) (Val_hex pid)
           ] in
         let name = match id with
           | 0 -> "network"
           | x -> Printf.sprintf "program %d" x in
         let node = to_node ~offset:off (parsed_length nodes) name (List nodes) in
         parse_programs (off_3 + off) (node :: acc) rest

  let parse buf =
    let bs = bitstring_of_string buf in
    let progs_length off = bitstring_length bs - off - 8 - 32 in
    match%bitstring bs with
    | {| header                 : 24 : bitstring
       ; transport_stream_id    : 16 : save_offset_to (off_1)
       ; reserved               : 2  : save_offset_to (off_2)
       ; version_number         : 5  : save_offset_to (off_3)
       ; current_next_indicator : 1  : save_offset_to (off_4)
       ; section_number         : 8  : save_offset_to (off_5)
       ; last_section_number    : 8  : save_offset_to (off_6)
       ; programs               : progs_length off_6 : save_offset_to (off_7), bitstring
       ; crc32                  : 32 : save_offset_to (off_8)
       |} ->
       let progs  = parse_programs off_7 [] programs in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 16 "transport_stream_id" (Val_hex transport_stream_id)
         ; to_node ~offset:off_2 2 "reserved" (Val_hex reserved)
         ; to_node ~offset:off_3 5 "version_number" (Int version_number)
         ; to_node ~offset:off_4 1 "current_next_indicator" (Flag current_next_indicator)
         ; to_node ~offset:off_5 8 "section_number" (Val_hex section_number)
         ; to_node ~offset:off_6 8 "last_section_number" (Val_hex last_section_number)
         ; to_node ~offset:off_7 (progs_length off_6) "programs" (List progs)
         ; to_node ~offset:off_8 32 "CRC_32" (Int32 crc32)
         ] in
       header @ nodes

end

module PMT = struct
  open Table_common

  let rec parse_streams = fun off acc x ->
    if Bitstring.bitstring_length x = 0 then List.rev acc
    else
      (match%bitstring x with
       | {| stream_type    : 8  : save_offset_to (off_1)
          ; reserved_1     : 3  : save_offset_to (off_2)
          ; elementary_pid : 13 : save_offset_to (off_3)
          ; reserved_2     : 4  : save_offset_to (off_4)
          ; es_info_length : 12 : save_offset_to (off_5)
          ; descriptors    : es_info_length * 8 : save_offset_to (off_6), bitstring
          ; rest           : -1 : save_offset_to (off_7), bitstring
          |} ->
          let dscrs = parse_descriptors off_6 [] descriptors in
          let nodes =
            [ to_node ~offset:(off_1 + off) 8 "stream_type" (Val_hex stream_type)
            ; to_node ~offset:(off_2 + off) 3 "reserved" (Val_hex reserved_1)
            ; to_node ~offset:(off_3 + off) 13 "elementary_PID" (Val_hex elementary_pid)
            ; to_node ~offset:(off_4 + off) 4 "reserved" (Val_hex reserved_2)
            ; to_node ~offset:(off_5 + off) 12 "ES_info_length" (Val_hex es_info_length)
            ; to_node ~offset:(off_6 + off) (es_info_length * 8) "descriptors" (List dscrs)
            ] in
          parse_streams (off_7 + off) (nodes @ acc) rest)

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    let streams_len prog_length off = Bitstring.bitstring_length bs - (prog_length * 8) - off - 32 in
    match%bitstring bs with
    | {| header                 : 24 : bitstring
       ; program_number         : 16 : save_offset_to (off_1)
       ; reserved_1             : 2  : save_offset_to (off_2)
       ; version_number         : 5  : save_offset_to (off_3)
       ; current_next_indicator : 1  : save_offset_to (off_4)
       ; section_number         : 8  : save_offset_to (off_5)
       ; last_section_number    : 8  : save_offset_to (off_6)
       ; reserved_2             : 3  : save_offset_to (off_7)
       ; pcr_pid                : 13 : save_offset_to (off_8)
       ; reserved_3             : 4  : save_offset_to (off_9)
       ; prog_inf_len           : 12 : save_offset_to (off_10)
       ; descriptors            : prog_inf_len * 8 : bitstring, save_offset_to (off_11)
       ; streams                : streams_len prog_inf_len off_11 : bitstring, save_offset_to (off_12)
       ; crc32                  : 32 : save_offset_to (off_13)
       |} ->
       let dscrs   = parse_descriptors off_11 [] descriptors in
       let header  = parse_header header in
       let streams = parse_streams off_12 [] streams in
       let nodes   =
         [ to_node ~offset:off_1 16 "program_number" (Val_hex program_number)
         ; to_node ~offset:off_2 2 "reserved" (Val_hex reserved_1)
         ; to_node ~offset:off_3 5 "version_number" (Val_hex version_number)
         ; to_node ~offset:off_4 1 "current_next_indicator" (Flag current_next_indicator)
         ; to_node ~offset:off_5 8 "section_number" (Val_hex section_number)
         ; to_node ~offset:off_6 8 "last_section_number" (Val_hex last_section_number)
         ; to_node ~offset:off_7 3 "reserved" (Val_hex reserved_2)
         ; to_node ~offset:off_8 13 "PCR_PID" (Val_hex pcr_pid)
         ; to_node ~offset:off_9 4 "reserved" (Val_hex reserved_3)
         ; to_node ~offset:off_10 12 "program_info_length" (Val_hex prog_inf_len)
         ; to_node ~offset:off_11 (prog_inf_len * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_12 (streams_len prog_inf_len off_11) "streams" (List streams)
         ; to_node ~offset:off_13 32 "CRC_32" (Int32 crc32)
         ]
       in
       header @ nodes

end

module CAT = struct

  open Table_common
  open Bitstring

  let parse buf =
    let bs = bitstring_of_string buf in
    let descriptors_length off = bitstring_length bs - off - 8 - 32 in
    match%bitstring bs with
    | {| header                 : 24 : bitstring
       ; reserved               : 18 : save_offset_to (off_1)
       ; version_number         : 5  : save_offset_to (off_2)
       ; current_next_indicator : 1  : save_offset_to (off_3)
       ; section_number         : 8  : save_offset_to (off_4)
       ; last_section_number    : 8  : save_offset_to (off_5)
       ; descriptors            : descriptors_length off_5 : save_offset_to (off_6), bitstring
       ; crc32                  : 32 : save_offset_to (off_7)
       |} ->
       let dscrs  = parse_descriptors off_6 [] descriptors in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 18 "reserved" (Val_hex reserved)
         ; to_node ~offset:off_2 5 "version_number" (Val_hex version_number)
         ; to_node ~offset:off_3 1 "current_next_indicator" (Flag current_next_indicator)
         ; to_node ~offset:off_4 8 "section_number" (Val_hex section_number)
         ; to_node ~offset:off_5 8 "last_section_number" (Val_hex last_section_number)
         ; to_node ~offset:off_6 (descriptors_length off_5) "descriptors" (List dscrs)
         ; to_node ~offset:off_7 32 "CRC_32" (Int32 crc32)
         ]
       in
       header @ nodes

end

module TSDT = struct

  include CAT

end


module NIT = struct

  open Table_common

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    match%bitstring bs with
    | {| header                 : 24 : bitstring
       ; network_id             : 16 : save_offset_to (off_1)
       ; reserved               : 2  : save_offset_to (off_2)
       ; version_number         : 5  : save_offset_to (off_3)
       ; current_next_indicator : 1  : save_offset_to (off_4)
       ; section_number         : 8  : save_offset_to (off_5)
       ; last_section_number    : 8  : save_offset_to (off_6)
       ; rfu_1                  : 4  : save_offset_to (off_7)
       ; network_desc_length    : 12 : save_offset_to (off_8)
       ; descriptors            : network_desc_length * 8 : save_offset_to (off_9), bitstring
       ; rfu_2                  : 4  : save_offset_to (off_10)
       ; ts_loop_len            : 12 : save_offset_to (off_11)
       ; transport_streams      : ts_loop_len * 8 : save_offset_to (off_12),bitstring
       ; crc32                  : 32 : save_offset_to (off_13)
       |} ->
       let ts     = parse_ts off_12 [] transport_streams in
       let dscrs  = parse_descriptors off_9 [] descriptors in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 16 "network_id" (Val_hex network_id)
         ; to_node ~offset:off_2 2 "reserved" (Val_hex reserved)
         ; to_node ~offset:off_3 5 "version_number" (Val_hex version_number)
         ; to_node ~offset:off_4 1 "current_next_indicator" (Flag current_next_indicator)
         ; to_node ~offset:off_5 8 "section_number" (Val_hex section_number)
         ; to_node ~offset:off_6 8 "last_section_number" (Val_hex last_section_number)
         ; to_node ~offset:off_7 4 "reserved_future_use" (Val_hex rfu_1)
         ; to_node ~offset:off_8 12 "network_desc_length" (Val_hex network_desc_length)
         ; to_node ~offset:off_9 (network_desc_length * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_10 4 "reserved_future_use" (Val_hex rfu_2)
         ; to_node ~offset:off_11 12 "transport_stream_loop_length" (Val_hex ts_loop_len)
         ; to_node ~offset:off_12 (ts_loop_len * 8) "transport_streams" (List ts)
         ; to_node ~offset:off_13 32 "CRC_32" (Int32 crc32)
         ]
       in
       header @ nodes
end

module BAT = struct
  open Table_common

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    match%bitstring bs with
    | {| header                 : 24 : bitstring
       ; bouquet_id             : 16 : save_offset_to (off_1)
       ; reserved               : 2  : save_offset_to (off_2)
       ; version_number         : 5  : save_offset_to (off_3)
       ; current_next_indicator : 1  : save_offset_to (off_4)
       ; section_number         : 8  : save_offset_to (off_5)
       ; last_section_number    : 8  : save_offset_to (off_6)
       ; rfu_1                  : 4  : save_offset_to (off_7)
       ; bouquet_desc_length    : 12 : save_offset_to (off_8)
       ; descriptors            : bouquet_desc_length * 8 : save_offset_to (off_9), bitstring
       ; rfu_2                  : 4  : save_offset_to (off_10)
       ; ts_loop_len            : 12 : save_offset_to (off_11)
       ; transport_streams      : ts_loop_len * 8 : save_offset_to (off_12), bitstring
       ; crc32                  : 32 : save_offset_to (off_13)
       |} ->
       let ts     = parse_ts off_12 [] transport_streams in
       let dscrs  = parse_descriptors off_9 [] descriptors in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 16 "bouquet_id" (Val_hex bouquet_id)
         ; to_node ~offset:off_2 2 "reserved" (Val_hex reserved)
         ; to_node ~offset:off_3 5 "version_number" (Val_hex version_number)
         ; to_node ~offset:off_4 1 "current_next_indicator" (Flag current_next_indicator)
         ; to_node ~offset:off_5 8 "section_number" (Val_hex section_number)
         ; to_node ~offset:off_6 8 "last_section_number" (Val_hex last_section_number)
         ; to_node ~offset:off_7 4 "reserved_future_use" (Val_hex rfu_1)
         ; to_node ~offset:off_8 12 "bouquet_descriptors_length" (Val_hex bouquet_desc_length)
         ; to_node ~offset:off_9 (bouquet_desc_length * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_10 4 "reserved_future_use" (Val_hex rfu_2)
         ; to_node ~offset:off_11 12 "transport_stream_loop_length" (Val_hex ts_loop_len)
         ; to_node ~offset:off_12 (ts_loop_len * 8) "transport_streams" (List ts)
         ; to_node ~offset:off_13 32 "CRC_32" (Int32 crc32)
         ]
       in
       header @ nodes
 end

module SDT = struct
  open Table_common

  let parse buf =
    let rec parse_services = fun acc x ->
      if Bitstring.bitstring_length x = 0 then List.rev acc
      else
        (match%bitstring x with
         | {| service_id                 : 16 : save_offset_to (off_1)
            ; rfu                        : 6  : save_offset_to (off_2)
            ; eit_schedule_flag          : 1  : save_offset_to (off_3)
            ; eit_present_following_flag : 1  : save_offset_to (off_4)
            ; running_status             : 3  : save_offset_to (off_5)
            ; free_ca_mode               : 1  : save_offset_to (off_6)
            ; desc_loop_length           : 12 : save_offset_to (off_7)
            ; descriptors                : desc_loop_length * 8 : save_offset_to (off_8), bitstring
            ; rest                       : -1 : bitstring
            |} ->
            let dscrs = parse_descriptors off_8
                          [] descriptors in
            let nodes =
              [ to_node ~offset:off_1 16 "service_id" (Val_hex service_id)
              ; to_node ~offset:off_2  6 "reserved_fuure_use" (Val_hex rfu)
              ; to_node ~offset:off_3  1 "EIT_schedule_flag" (Flag eit_schedule_flag)
              ; to_node ~offset:off_4  1 "EIT_present_following_flag" (Flag eit_present_following_flag)
              ; to_node ~offset:off_5  3 "runnning_status" (Val_hex running_status)
              ; to_node ~offset:off_6  1 "free_CA_mode" (Flag free_ca_mode)
              ; to_node ~offset:off_7 12 "descriptors_loop_length" (Val_hex desc_loop_length)
              ; to_node ~offset:off_8 (desc_loop_length * 8) "descriptors" (List dscrs)
              ]
            in
            parse_services (nodes @ acc) rest)
    in
    let bs = Bitstring.bitstring_of_string buf in
    let services_length off = Bitstring.bitstring_length bs - off - 8 - 32 in
    match%bitstring bs with
    | {| header                 : 24 : bitstring
       ; transport_stream_id    : 16 : save_offset_to (off_1)
       ; reserved               : 2  : save_offset_to (off_2)
       ; version_number         : 5  : save_offset_to (off_3)
       ; current_next_indicator : 1  : save_offset_to (off_4)
       ; section_number         : 8  : save_offset_to (off_5)
       ; last_section_number    : 8  : save_offset_to (off_6)
       ; original_network_id    : 16 : save_offset_to (off_7)
       ; rfu                    : 8  : save_offset_to (off_8)
       ; services               : services_length off_8 : save_offset_to (off_9), bitstring
       ; crc32                  : 32 : save_offset_to (off_10)
       |} ->
       let services = parse_services [] services in
       let header   = parse_header header in
       let nodes =
         [ to_node ~offset:off_1 16 "transport_stream_id" (Val_hex transport_stream_id)
         ; to_node ~offset:off_2 2 "reserved" (Val_hex reserved)
         ; to_node ~offset:off_3 5 "version_number" (Val_hex version_number)
         ; to_node ~offset:off_4 1 "current_next_indicator" (Flag current_next_indicator)
         ; to_node ~offset:off_5 8 "section_number" (Val_hex section_number)
         ; to_node ~offset:off_6 8 "last_section_number" (Val_hex last_section_number)
         ; to_node ~offset:off_7 16 "original_network_id" (Val_hex original_network_id)
         ; to_node ~offset:off_8 8 "reserved_future_use" (Val_hex rfu)
         ; to_node ~offset:off_9 (services_length off_8) "services" (List services)
         ; to_node ~offset:off_10 32 "CRC_32" (Int32 crc32)
         ]
       in
       header @ nodes
end

module EIT = struct

  open Table_common

  let rec parse_events = fun off acc x ->
    if Bitstring.bitstring_length x = 0 then List.rev acc
    else
      (match%bitstring x with
       | {| event_id             : 16 : save_offset_to (off_1)
          ; start_time           : 40 : save_offset_to (off_2)
          ; duration             : 24 : save_offset_to (off_3)
          ; running_status       : 3  : save_offset_to (off_4)
          ; free_ca_mode         : 1  : save_offset_to (off_5)
          ; desc_loop_length     : 12 : save_offset_to (off_6)
          ; descriptors          : desc_loop_length * 8 : save_offset_to (off_7), bitstring
          ; rest                 : -1 : save_offset_to (off_8), bitstring
          |} ->
          let dscrs = parse_descriptors off_7 [] descriptors in
          let nodes =
            [ to_node ~offset:(off_1 + off) 16 "event_id" (Val_hex event_id)
            ; to_node ~offset:(off_2 + off) 40 "start_time" (Int64 start_time)
            ; to_node ~offset:(off_3 + off) 24 "duration" (Val_hex duration)
            ; to_node ~offset:(off_4 + off)  3 "running_status" (Val_hex running_status)
            ; to_node ~offset:(off_5 + off)  1 "free_CA_mode" (Flag free_ca_mode)
            ; to_node ~offset:(off_6 + off) 12 "decriptors_loop_length" (Val_hex desc_loop_length)
            ; to_node ~offset:(off_7 + off) (desc_loop_length * 8) "descriptors" (List dscrs)
            ]
          in
          parse_events (off_8 + off) (nodes @ acc) rest)

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    let events_length off = Bitstring.bitstring_length bs - off - 8 - 32 in
    match%bitstring bs with
    | {| header                 : 24 : bitstring
       ; service_id             : 16 : save_offset_to (off_1)
       ; reserved               : 2  : save_offset_to (off_2)
       ; version_number         : 5  : save_offset_to (off_3)
       ; current_next_indicator : 1  : save_offset_to (off_4)
       ; section_number         : 8  : save_offset_to (off_5)
       ; last_section_number    : 8  : save_offset_to (off_6)
       ; transport_stream_id    : 16 : save_offset_to (off_7)
       ; original_network_id    : 16 : save_offset_to (off_8)
       ; seg_last_section_num   : 8  : save_offset_to (off_9)
       ; last_table_id          : 8  : save_offset_to (off_10)
       ; events                 : events_length off_10 : save_offset_to (off_11), bitstring
       ; crc32                  : 32 : save_offset_to (off_12)
       |} ->
       let events = parse_events off_11 [] events in
       let header = parse_header header in
       let nodes =
         [ to_node ~offset:off_1 16 "service_id" (Val_hex service_id)
         ; to_node ~offset:off_2 2 "reserved" (Val_hex reserved)
         ; to_node ~offset:off_3 5 "version_number" (Val_hex version_number)
         ; to_node ~offset:off_4 1 "current_next_indicator" (Flag current_next_indicator)
         ; to_node ~offset:off_5 8 "section_number" (Val_hex section_number)
         ; to_node ~offset:off_6 8 "last_section_number" (Val_hex last_section_number)
         ; to_node ~offset:off_7 16 "transport_stream_id" (Val_hex original_network_id)
         ; to_node ~offset:off_8 16 "original_network_id" (Val_hex original_network_id)
         ; to_node ~offset:off_9 8 "segment_last_section_number" (Val_hex seg_last_section_num)
         ; to_node ~offset:off_10 8 "last_table_id" (Val_hex last_table_id)
         ; to_node ~offset:off_11 (events_length off_10) "events" (List events)
         ; to_node ~offset:off_12 32 "CRC_32" (Int32 crc32)
         ]
       in
       header @ nodes

end

module TDT = struct

  open Table_common

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    match%bitstring bs with
    | {| header   : 24 : bitstring
       ; utc_time : 40 : save_offset_to (off) |} ->
       let node =
         [ to_node ~offset:off 40 "utc_time" (Int64 utc_time)
         ]
       in
       let header = parse_header header in
       header @ node

end

module TOT = struct

  open Table_common

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    match%bitstring bs with
    | {| header      : 24 : bitstring
       ; utc_time    : 40 : save_offset_to (off_1)
       ; reserved    : 4  : save_offset_to (off_2)
       ; loop_length : 12 : save_offset_to (off_3)
       ; descriptors : loop_length * 8 : save_offset_to (off_4), bitstring
       ; crc32       : 32 : save_offset_to (off_5)
       |} ->
       let dscrs  = parse_descriptors off_4 [] descriptors in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 40 "utc_time" (Int64 utc_time)
         ; to_node ~offset:off_2 4  "reserved" (Val_hex reserved)
         ; to_node ~offset:off_3 12 "descriptors_loop_length" (Val_hex loop_length)
         ; to_node ~offset:off_4 (loop_length * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_5 32 "CRC_32" (Int32 crc32)
         ]
       in
       header @ nodes

end

module RST = struct

  open Table_common

  let rec parse_events = fun acc x ->
    if Bitstring.bitstring_length x = 0 then List.rev acc
    else
      (match%bitstring x with
       | {| ts_id          : 16 : save_offset_to (off_1)
          ; on_id          : 16 : save_offset_to (off_2)
          ; service_id     : 16 : save_offset_to (off_3)
          ; event_id       : 16 : save_offset_to (off_4)
          ; rfu            : 5  : save_offset_to (off_5)
          ; running_status : 3  : save_offset_to (off_6)
          ; rest           : -1 :  bitstring
          |} ->
          let nodes =
            [ to_node ~offset:off_1 16 "transport_stream_id" (Val_hex ts_id)
            ; to_node ~offset:off_2 16 "original_network_id" (Val_hex on_id)
            ; to_node ~offset:off_3 16 "service_id" (Val_hex service_id)
            ; to_node ~offset:off_4 16 "event_id" (Val_hex event_id)
            ; to_node ~offset:off_5  5 "reserved_future_use" (Val_hex rfu)
            ; to_node ~offset:off_6  3 "running_status" (Val_hex running_status)
            ]
          in
          parse_events (nodes @ acc) rest)

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    match%bitstring bs with
       | {| header : 24 : bitstring
          ; rest   : -1 : bitstring
          |} ->
          (* FIXME *)
          let header = parse_header header in
          header @ ( parse_events [] rest)

end

module ST = struct

  open Bitstring
  open Table_common

  let parse buf =
    let bs = bitstring_of_string buf in
    match%bitstring bs with
       | {| header : 24 : bitstring
          ; rest   : -1 : bitstring
          |} ->
          (* FIXME *)
          let bytes =
            to_node
              ~offset:24
              (bitstring_length rest)
              "bytes"
              (Bytes (string_of_bitstring rest))
          in
          let header = parse_header header in
          header @ [ bytes ]

end

module DIT = struct

  open Table_common

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    match%bitstring bs with
    | {| header          : 24 : bitstring
       ; transition_flag : 1  : save_offset_to (off_1)
       ; rfu             : 7  : save_offset_to (off_2)
       ; rest            : -1 : bitstring
       |} when Bitstring.bitstring_length rest = 0 ->
       let nodes =
         [ to_node ~offset:off_1 1 "transition_flag" (Flag transition_flag)
         ; to_node ~offset:off_2 7 "reserved_future_use" (Val_hex rfu)
         ]
       in
       let header = parse_header header in
       header @ nodes

end

module SIT = struct

  open Table_common
  open Bitstring

  let rec parse_services = fun off acc x ->
    if Bitstring.bitstring_length x = 0 then List.rev acc
    else
      (match%bitstring x with
       | {| service_id          : 16 : save_offset_to (off_1)
          ; dvb_rfu             : 1  : save_offset_to (off_2)
          ; running_status      : 3  : save_offset_to (off_3)
          ; service_loop_length : 12 : save_offset_to (off_4)
          ; descriptors         : service_loop_length * 8 : save_offset_to (off_5), bitstring
          ; rest                : -1 : save_offset_to (off_6), bitstring
          |} ->
          let dscrs = parse_descriptors off_5 [] descriptors in
          let nodes =
            [ to_node ~offset:(off_1 + off) 16 "service_id" (Val_hex service_id)
            ; to_node ~offset:(off_2 + off)  1 "reserved_future_use" (Flag dvb_rfu)
            ; to_node ~offset:(off_3 + off)  3 "running_status" (Val_hex running_status)
            ; to_node ~offset:(off_4 + off) 12 "service_loop_length" (Val_hex service_loop_length)
            ; to_node ~offset:(off_5 + off) (service_loop_length * 8) "descriptors" (List dscrs) ]
          in
          parse_services (off_6 + off) (nodes @ acc) rest)

  let parse buf =
    let bs = bitstring_of_string buf in
    let services_length off till = bitstring_length bs - off - (till * 8) - 32 in
    match%bitstring bs with
    | {| header                 : 24 : bitstring
       ; dvb_rfu_1              : 16 : save_offset_to (off_1)
       ; iso_reserved           : 2  : save_offset_to (off_2)
       ; version_number         : 5  : save_offset_to (off_3)
       ; current_next_indicator : 1  : save_offset_to (off_4)
       ; section_number         : 8  : save_offset_to (off_5)
       ; last_section_number    : 8  : save_offset_to (off_6)
       ; dvb_rfu_2              : 4  : save_offset_to (off_7)
       ; loop_length            : 12 : save_offset_to (off_8)
       ; descriptors            : loop_length * 8 : bitstring, save_offset_to (off_9)
       ; services               : services_length off_9 loop_length : save_offset_to (off_10), bitstring
       ; crc32                  : 32 : save_offset_to (off_11)
       ; rest                   : -1 : bitstring
       |} when bitstring_length rest = 0 ->
       let services = parse_services off_10 [] services in
       let dscrs = parse_descriptors off_9 [] descriptors in
       let nodes =
         [ to_node ~offset:off_1 16 "dvb_rfu_1" (Val_hex dvb_rfu_1)
         ; to_node ~offset:off_2 2 "ISO_reserved" (Int iso_reserved)
         ; to_node ~offset:off_3 5 "version_number" (Val_hex version_number)
         ; to_node ~offset:off_4 1 "current_next_indicator" (Flag current_next_indicator)
         ; to_node ~offset:off_5 8 "section_number" (Val_hex section_number)
         ; to_node ~offset:off_6 8 "last_section_number" (Val_hex last_section_number)
         ; to_node ~offset:off_7 4 "dvb_rfu_2" (Val_hex dvb_rfu_2)
         ; to_node ~offset:off_8 12 "transmission_info_loop_length" (Val_hex loop_length)
         ; to_node ~offset:off_9 (loop_length * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_10 (services_length off_9 loop_length) "services" (List services)
         ; to_node ~offset:off_11 32 "CRC_32" (Int32 crc32)
         ]
       in
       let header = parse_header header in
       header @ nodes

end

let table_to_yojson : string ->
                      Common.Mpeg_ts.table ->
                      Yojson.Safe.json option =
  fun buf tbl ->
  try
    (match tbl with
     | `PAT   -> PAT.parse buf
     | `CAT   -> CAT.parse buf
     | `PMT   -> PMT.parse buf
     | `TSDT  -> TSDT.parse buf
     | `NIT _ -> NIT.parse buf
     | `SDT _ -> SDT.parse buf
     | `BAT   -> BAT.parse buf
     | `EIT _ -> EIT.parse buf
     | `TDT   -> TDT.parse buf
     | `RST   -> RST.parse buf
     | `ST    -> ST.parse buf
     | `TOT   -> TOT.parse buf
     | `DIT   -> DIT.parse buf
     | `SIT   -> SIT.parse buf
     | _      -> [])
    |> parsed_to_yojson
    |> Option.return
  with _ -> None


