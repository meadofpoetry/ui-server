open Containers

let value_to_string name x = Printf.sprintf "%s (%d)" name x
let rfu_to_string          = value_to_string "Reserved"

module Descriptor = struct

  module type Descriptor_base = sig

    type t [@@deriving yojson]
    val name   : string
    val decode : Bitstring.t -> t

  end

  module Video_stream : Descriptor_base = struct

    type frame_rate = Forbidden
                    | FR_23_976
                    | FR_24
                    | FR_25
                    | FR_29_97
                    | FR_30
                    | FR_50
                    | FR_59_94
                    | FR_60
                    | Reserved of int [@@deriving yojson]

    type chroma_format = CHROMA_4_2_0
                       | CHROMA_4_2_2
                       | CHROMA_4_4_4
                       | Reserved of int [@@deriving yojson]

    type mpeg_1_only =
      { profile_and_level_indication : int
      ; chroma_format                : chroma_format
      ; frame_rate_extension_flag    : bool
      ; reserved                     : int
      } [@@deriving yojson]

    type t =
      { mfr_flag                   : bool
      ; frame_rate                 : frame_rate
      ; mpeg_1_only_flag           : bool
      ; constrained_parameter_flag : bool
      ; still_picture_flag         : bool
      ; mpeg_1_only                : mpeg_1_only option
      } [@@deriving yojson]

    let name = "video_stream_descriptor"

    let frame_rate_of_int = function
      | 0b0000 -> Forbidden  | 0b0001 -> FR_23_976  | 0b0010 -> FR_24
      | 0b0011 -> FR_25      | 0b0100 -> FR_29_97   | 0b0101 -> FR_30
      | 0b0110 -> FR_50      | 0b0111 -> FR_59_94   | 0b1000 -> FR_60
      | x      -> Reserved x

    let frame_rate_to_int = function
      | Forbidden  -> 0b0000 | FR_23_976 -> 0b0001 | FR_24 -> 0b0010
      | FR_25      -> 0b0011 | FR_29_97  -> 0b0100 | FR_30 -> 0b0101
      | FR_50      -> 0b0110 | FR_59_94  -> 0b0111 | FR_60 -> 0b1000
      | Reserved x -> x

    let frame_rate_to_string = function
      | Forbidden  -> "Forbidden" | FR_23_976 -> "23.976" | FR_24 -> "24"
      | FR_25      -> "25"        | FR_29_97  -> "29.97"  | FR_30 -> "30"
      | FR_50      -> "50"        | FR_59_94  -> "59.94"  | FR_60 -> "60"
      | Reserved x -> rfu_to_string x

    let frame_rate_mfr_to_string mfr fr =
      let another = if not mfr then []
                    else (match fr with
                          | FR_24    -> [FR_23_976]
                          | FR_29_97 -> [FR_23_976]
                          | FR_30    -> [FR_23_976; FR_24; FR_29_97]
                          | FR_50    -> [FR_25]
                          | FR_59_94 -> [FR_23_976; FR_29_97]
                          | FR_60    -> [FR_23_976; FR_24; FR_29_97; FR_30; FR_59_94]
                          | _        -> []) in
      frame_rate_to_string fr
      |> fun x -> match another with
                  | [] -> x
                  | l  -> let s = String.concat ", " (List.map (fun x -> frame_rate_to_string x) l) in
                          x ^ (Printf.sprintf " (Also includes %s)" s)

    let chroma_format_of_int = function
      | 0b01 -> CHROMA_4_2_0 | 0b10 -> CHROMA_4_2_2
      | 0b11 -> CHROMA_4_4_4 | x    -> Reserved x

    let chroma_format_to_int = function
      | CHROMA_4_2_0 -> 0b01 | CHROMA_4_2_2 -> 0b10
      | CHROMA_4_4_4 -> 0b11 | Reserved x   -> x

    let chroma_format_to_string = function
      | CHROMA_4_2_0 -> "4:2:0" | CHROMA_4_2_2 -> "4:2:2"
      | CHROMA_4_4_4 -> "4:4:4" | Reserved x   -> rfu_to_string x

    let decode bs =
      match%bitstring bs with
      | {| mfr_flag : 1
         ; frame_rate : 4 : map (fun x -> frame_rate_of_int x)
         ; mpeg_1_only_flag : 1
         ; constrained_parameter_flag : 1
         ; still_picture_flag : 1
         |} when not mpeg_1_only_flag -> { mfr_flag; frame_rate; mpeg_1_only_flag; constrained_parameter_flag;
                                           still_picture_flag; mpeg_1_only = None }
      | {| mfr_flag : 1
         ; frame_rate : 4 : map (fun x -> frame_rate_of_int x)
         ; mpeg_1_only_flag : 1
         ; constrained_parameter_flag : 1
         ; still_picture_flag : 1
         ; profile_and_level_indication : 8
         ; chroma_format: 2 : map (fun x -> chroma_format_of_int x)
         ; frame_rate_extension_flag : 1
         ; reserved : 5
         |} -> { mfr_flag; frame_rate; mpeg_1_only_flag; constrained_parameter_flag; still_picture_flag;
                 mpeg_1_only = Some { profile_and_level_indication
                                    ; chroma_format
                                    ; frame_rate_extension_flag
                                    ; reserved }}

  end

  module Audio_stream : Descriptor_base = struct

    type t =
      { free_format_flag : bool
      ; id               : bool
      ; layer            : int
      ; variable_rate    : bool
      ; reserved         : int
      } [@@deriving yojson]

    let name = "audio_stream_descriptor"

    let decode bs =
      match%bitstring bs with
      | {| free_format_flag : 1
         ; id               : 1
         ; layer            : 2
         ; variable_rate    : 1
         ; reserved         : 3
         |} -> { free_format_flag; id; layer; variable_rate; reserved }

  end

  module Hierarchy : Descriptor_base = struct

    type hierarchy = Reserved of int
                   | Spatial_scalability
                   | SNR_scalability
                   | Temporal_scalability
                   | Data_partitioning
                   | Extension_bitstream
                   | Private_stream
                   | Multi_view_profile
                   | Combined_scalability
                   | MVC_or_MVCD
                   | Base_layer_etc [@@deriving yojson]

    type t =
      { reserved_1                     : bool
      ; temporal_scalability           : bool
      ; spatial_scalability            : bool
      ; quality_scalability            : bool
      ; hierarchy_type                 : hierarchy
      ; reserved_2                     : int
      ; hierarchy_layer_index          : int
      ; tref_present_flag              : bool
      ; reserved_3                     : bool
      ; hierarchy_embedded_layer_index : int
      ; reserved_4                     : int
      ; hierarchy_channel              : int
      } [@@deriving yojson]

    let name = "Hierarchy descriptor"

    let hierarchy_of_int = function
      | 1  -> Spatial_scalability | 2  -> SNR_scalability      | 3  -> Temporal_scalability
      | 4  -> Data_partitioning   | 5  -> Extension_bitstream  | 6  -> Private_stream
      | 7  -> Multi_view_profile  | 8  -> Combined_scalability | 9  -> MVC_or_MVCD
      | 15 -> Base_layer_etc      | x  -> Reserved x

    let hierarchy_to_int = function
      | Reserved x           -> x | Spatial_scalability -> 1 | SNR_scalability -> 2 | Temporal_scalability -> 3
      | Data_partitioning    -> 4 | Extension_bitstream -> 5 | Private_stream  -> 6 | Multi_view_profile   -> 7
      | Combined_scalability -> 8 | MVC_or_MVCD -> 9         | Base_layer_etc  -> 15

    let hierarchy_to_string = function
      | Reserved x           -> rfu_to_string x
      | Spatial_scalability  -> "Spatial Scalability"
      | SNR_scalability      -> "SNR Scalability"
      | Temporal_scalability -> "Temporal Scalability"
      | Data_partitioning    -> "Data partitioning"
      | Extension_bitstream  -> "Extension bitstream"
      | Private_stream       -> "Private Stream"
      | Multi_view_profile   -> "Multi-view Profile"
      | Combined_scalability -> "Combined Scalability"
      | MVC_or_MVCD          -> "MVC video sub-bitstream or MVCD video sub-bitstream"
      | Base_layer_etc       -> "Base layer or MVC base view sub-bitstream or AVC video \
                                 sub-bitstream of MVC or HEVC temporal video sub-bitstream or Base \
                                 layer of MVCD base view sub-bitstream or AVC video sub-bitstream \
                                 of MVCD"

    let decode bs =
      match%bitstring bs with
      | {| reserved_1                     : 1
         ; temporal_scalability           : 1
         ; spatial_scalability            : 1
         ; quality_scalability            : 1
         ; hierarchy_type                 : 4 : map (fun x -> hierarchy_of_int x)
         ; reserved_2                     : 2
         ; hierarchy_layer_index          : 6
         ; tref_present_flag              : 1
         ; reserved_3                     : 1
         ; hierarchy_embedded_layer_index : 6
         ; reserved_4                     : 2
         ; hierarchy_channel              : 6
         |} -> { reserved_1; temporal_scalability; spatial_scalability; quality_scalability; hierarchy_type;
                 reserved_2; hierarchy_layer_index; tref_present_flag; reserved_3; hierarchy_embedded_layer_index;
                 reserved_4; hierarchy_channel }

  end

  module Registration : Descriptor_base = struct

    type t =
      { format_identifier              : int32
      ; additional_identification_info : string
      } [@@deriving yojson]

    let name = "Registration descriptor"

    let decode bs =
      match%bitstring bs with
      | {| format_identifier : 32
         ; rest              : -1 : bitstring
         |} -> { format_identifier; additional_identification_info = Bitstring.string_of_bitstring rest }

  end

  module Data_stream_alignment : Descriptor_base = struct

    type t = { alignment_type : int } [@@deriving yojson]

    let name = "Data stream alignment descriptor"

    let decode bs =
      match%bitstring bs with
      | {| alignment_type : 8 |} -> { alignment_type }

  end

  module Target_background_grid : Descriptor_base = struct

    type t =
      { horizontal_size          : int
      ; vertical_size            : int
      ; aspect_ratio_information : int
      } [@@deriving yojson]

    let name = "Target background grid descriptor"

    let decode bs =
      match%bitstring bs with
      | {| horizontal_size          : 14
         ; vertical_size            : 14
         ; aspect_ratio_information : 4
         |} -> { horizontal_size; vertical_size; aspect_ratio_information }

  end

  module Video_window : Descriptor_base = struct

    type t =
      { horizontal_offset : int
      ; vertical_offset   : int
      ; window_priority   : int
      } [@@deriving yojson]

    let name = "Video window descriptor"

    let decode bs =
      match%bitstring bs with
      | {| horizontal_offset : 14
         ; vertical_offset   : 14
         ; window_priority   : 4
         |} -> { horizontal_offset; vertical_offset; window_priority }

  end

  module CA : Descriptor_base = struct

    type t =
      { ca_system_id : int
      ; reserved     : int
      ; ca_pid       : int
      ; private_data : string
      } [@@deriving yojson]

    let name = "Conditional access descriptor"

    let decode bs =
      match%bitstring bs with
      | {| ca_system_id : 16
         ; reserved     : 3
         ; ca_pid       : 13
         ; private_data : -1 : bitstring
         |} -> { ca_system_id; reserved; ca_pid; private_data = Bitstring.string_of_bitstring private_data }

  end

  module ISO_639_language : Descriptor_base = struct

    type audio_type = Undefined
                    | Clean_effects
                    | Hearing_impaired
                    | Visual_impaired_commentary
                    | User_private of int
                    | Reserved of int [@@deriving yojson]

    type audio_language =
      { iso_639_language_code : int
      ; audio_type            : audio_type
      } [@@deriving yojson]

    type t = audio_language list [@@deriving yojson]

    let audio_type_of_int = function
      | 0x00                          -> Undefined
      | 0x01                          -> Clean_effects
      | 0x02                          -> Hearing_impaired
      | 0x03                          -> Visual_impaired_commentary
      | x when x >= 0x04 && x <= 0x7F -> User_private x
      | x                             -> Reserved x

    let audio_type_to_int = function
      | Undefined      -> 0 | Clean_effects -> 1 | Hearing_impaired -> 2 | Visual_impaired_commentary -> 3
      | User_private x -> x | Reserved x    -> x

    let audio_type_to_string = function
      | Undefined                  -> "Undefined"
      | Clean_effects              -> "Clean effects"
      | Hearing_impaired           -> "Hearing impaired"
      | Visual_impaired_commentary -> "Visual impaired commentary"
      | User_private _             -> "User private"
      | Reserved _                 -> "Reserved"

    let name = "ISO 639 language descriptor"

    let lang_code_to_string x =
      List.map char_of_int [ (x lsr 16) land 0xFF; (x lsr 8) land 0xFF; x land 0xFF ] |> String.of_list

    let decode bs =
      let rec f  = fun acc x ->
        if Bitstring.bitstring_length x = 0 then List.rev acc
        else (match%bitstring bs with
              | {| iso_639_language_code : 24
                 ; audio_type            : 8  : map (fun x -> audio_type_of_int x)
                 ; rest                  : -1 : bitstring
                 |} -> f ({ iso_639_language_code; audio_type } :: acc) rest) in
      f [] bs

  end

  module System_clock : Descriptor_base = struct

    type t =
      { external_clock_reference_indicator : bool
      ; reserved_1                         : bool
      ; clock_accuracy_integer             : int
      ; clock_accuracy_exponent            : int
      ; reserved_2                         : int
      } [@@deriving yojson]

    let name = "System clock descriptor"

    let decode bs =
      match%bitstring bs with
      | {| external_clock_reference_indicator : 1
         ; reserved_1                         : 1
         ; clock_accuracy_integer             : 6
         ; clock_accuracy_exponent            : 3
         ; reserved_2                         : 5
         |} -> { external_clock_reference_indicator; reserved_1; clock_accuracy_integer;
                 clock_accuracy_exponent; reserved_2 }

  end

  (* module Multiplex_buffer_utitization = struct *)

  (* end *)

  (* module Copyright = struct *)

  (* end *)

  (* module Maximum_bitrate = struct *)

  (* end *)

  (* module Private_data_indicator = struct *)

  (* end *)

  (* module Smoothing_buffer = struct *)

  (* end *)

  (* module STD_descriptor = struct *)

  (* end *)

  (* module IBP_descriptor = struct *)

  (* end *)

  (* module MPEG_4_video = struct *)

  (* end *)

  (* module MPEG_4_audio = struct *)

  (* end *)

  module Unknown : Descriptor_base = struct

    type t = string [@@deriving yojson]

    let name = "unknown descriptor"

    let decode = Bitstring.string_of_bitstring

  end

                                       [%%cstruct
                                        type descriptor =
                                          { tag    : uint8_t
                                          ; length : uint8_t
                                          } [@@big_endian]]

  type descriptor = Video_stream           of Video_stream.t
                  | Audio_stream           of Audio_stream.t
                  | Hierarchy              of Hierarchy.t
                  | Registration           of Registration.t
                  | Data_stream_alignment  of Data_stream_alignment.t
                  | Target_background_grid of Target_background_grid.t
                  | Video_window           of Video_window.t
                  | CA                     of CA.t
                  | ISO_639_language       of ISO_639_language.t
                  | System_clock           of System_clock.t
                  | Unknown                of Unknown.t [@@deriving yojson]

  type t =
    { tag     : int
    ; length  : int
    ; name    : string
    ; content : descriptor
    } [@@deriving yojson]

  let decode tag length body =
    let name,content =
      (match tag with
       | 02 -> Video_stream.name,Video_stream (Video_stream.decode body)
       | 03 -> Audio_stream.name,Audio_stream (Audio_stream.decode body)
       | 04 -> Hierarchy.name,Hierarchy (Hierarchy.decode body)
       | 05 -> Registration.name,Registration (Registration.decode body)
       | 06 -> Data_stream_alignment.name,Data_stream_alignment (Data_stream_alignment.decode body)
       | 07 -> Target_background_grid.name,Target_background_grid (Target_background_grid.decode body)
       | 08 -> Video_window.name,Video_window (Video_window.decode body)
       | 09 -> CA.name,CA (CA.decode body)
       | 10 -> ISO_639_language.name, ISO_639_language (ISO_639_language.decode body)
       | 11 -> System_clock.name, System_clock (System_clock.decode body)
       | _    -> Unknown.name,Unknown (Unknown.decode body)) in
    { tag; length; name; content }

  let of_bitstring bs =
    Lwt_io.printf "Total length %d\n" (Bitstring.bitstring_length bs) |> ignore;
    match%bitstring bs with
    | {| tag    : 8
       ; length : 8
       ; body   : length * 8 : bitstring
       ; rest   : -1 : bitstring
       |} -> Lwt_io.printf "Length is %d\n" length |> ignore; (decode tag length body),rest

end

module Table_common = struct

  type header =
    { table_id                 : int
    ; section_syntax_indicator : bool
    ; rfu                      : bool
    ; reserved_1               : int
    ; section_length           : int
    } [@@deriving yojson]

  type ts =
    { transport_stream_id          : int
    ; original_network_id          : int
    ; rfu                          : int
    ; transport_descriptors_length : int
    ; descriptors                  : Descriptor.t list
    } [@@deriving yojson]

  let parse_header bs =
    match%bitstring bs with
    | {| table_id                 : 8
       ; section_syntax_indicator : 1
       ; rfu                      : 1
       ; reserved_1               : 2
       ; section_length           : 12
       ; rest                     : -1 : bitstring
       |} -> { table_id; section_syntax_indicator; rfu; reserved_1; section_length }, rest

  let rec parse_descriptors = (fun acc x ->
      if Bitstring.bitstring_length x = 0 then List.rev acc
      else let descr,rest = Descriptor.of_bitstring x in
           parse_descriptors (descr :: acc) rest)

  let rec parse_ts = (fun acc x ->
      if Bitstring.bitstring_length x = 0 then List.rev acc
      else (match%bitstring x with
            | {| transport_stream_id          : 16
               ; original_network_id          : 16
               ; rfu                          : 4
               ; transport_descriptors_length : 12
               ; descriptors                  : transport_descriptors_length * 8 : bitstring
               ; rest                         : -1 : bitstring
               |} -> parse_ts ({ transport_stream_id; original_network_id;
                                 rfu; transport_descriptors_length;
                                 descriptors = parse_descriptors [] descriptors} :: acc)
                       rest))

end

module PAT = struct

  open Table_common
  open Bitstring

  type program =
    { program_number : int
    ; reserved       : int
    ; pid            : int
    } [@@deriving yojson]

  type t =
    { header                 : header
    ; transport_stream_id    : int
    ; reserved               : int
    ; version_number         : int
    ; current_next_indicator : bool
    ; section_number         : int
    ; last_section_number    : int
    ; programs               : program list
    ; crc32                  : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = bitstring_of_string @@ Cbuffer.to_string buf in
    let rec f = fun acc x ->
      if bitstring_length x = 0 then List.rev acc
      else (match%bitstring x with
            | {| program_number : 16
               ; reserved       : 3
               ; pid            : 13
               ; rest           : -1 : bitstring
               |} -> f ({ program_number; reserved; pid } :: acc) rest) in
    let header,rest = parse_header bs in
    match%bitstring rest with
    | {| transport_stream_id    : 16
       ; reserved               : 2
       ; version_number         : 5
       ; current_next_indicator : 1
       ; section_number         : 8
       ; last_section_number    : 8 : save_offset_to (off)
       ; programs               : bitstring_length bs - off - 8 - 32 : bitstring
       ; crc32                  : 32
       |} -> { header; transport_stream_id; reserved; version_number; current_next_indicator;
               section_number; last_section_number; programs = f [] programs; crc32 }

end

module PMT = struct

  open Table_common

  type stream =
    { stream_type    : int
    ; reserved_1     : int
    ; elementary_pid : int
    ; reserved_2     : int
    ; es_info_length : int
    ; descriptors    : Descriptor.t list
    } [@@deriving yojson]

  type t =
    { header                 : header
    ; program_number         : int
    ; reserved_1             : int
    ; version_number         : int
    ; current_next_indicator : bool
    ; section_number         : int
    ; last_section_number    : int
    ; reserved_2             : int
    ; pcr_pid                : int
    ; reserved_3             : int
    ; program_info_length    : int
    ; descriptors            : Descriptor.t list
    ; streams                : stream list
    ; crc32                  : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let rec parse_streams = (fun acc x ->
        if Bitstring.bitstring_length x = 0 then List.rev acc
        else (match%bitstring x with
              | {| stream_type    : 8
                 ; reserved_1     : 3
                 ; elementary_pid : 13
                 ; reserved_2     : 4
                 ; es_info_length : 12
                 ; descriptors    : es_info_length * 8 : bitstring
                 ; rest           : -1 : bitstring
                 |} -> parse_streams ({ stream_type; reserved_1; elementary_pid;
                                        reserved_2; es_info_length;
                                        descriptors = parse_descriptors [] descriptors} :: acc)
                         rest)) in
    let header,rest = parse_header bs in
    let len = Bitstring.bitstring_length rest in
    match%bitstring rest with
    | {| program_number         : 16
       ; reserved_1             : 2
       ; version_number         : 5
       ; current_next_indicator : 1
       ; section_number         : 8
       ; last_section_number    : 8
       ; reserved_2             : 3
       ; pcr_pid                : 13
       ; reserved_3             : 4
       ; program_info_length    : 12
       ; descriptors            : program_info_length * 8 : bitstring, save_offset_to (off)
       ; streams                : len - (program_info_length * 8) - off - 32 : bitstring
       ; crc32                  : 32
       |} -> { header; program_number; reserved_1; version_number; current_next_indicator; section_number;
               last_section_number; reserved_2; pcr_pid; reserved_3; program_info_length;
               descriptors = parse_descriptors [] descriptors;
               streams = parse_streams [] streams; crc32 }

end

module CAT = struct

  open Table_common
  open Bitstring

  type t =
    { header                 : header
    ; reserved               : int
    ; version_number         : int
    ; current_next_indicator : bool
    ; section_number         : int
    ; last_section_number    : int
    ; descriptors            : Descriptor.t list
    ; crc32                  : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    match%bitstring rest with
    | {| reserved               : 18
       ; version_number         : 5
       ; current_next_indicator : 1
       ; section_number         : 8
       ; last_section_number    : 8 : save_offset_to (off)
       ; descriptors            : bitstring_length rest - off - 8 - 32 : bitstring
       ; crc32                  : 32
       |} -> { header; reserved; version_number; current_next_indicator; section_number;
               last_section_number; descriptors = parse_descriptors [] descriptors; crc32 }

end

module TSDT = struct

  include CAT

end

module NIT = struct

  open Table_common

  type t =
    { header                       : header
    ; network_id                   : int
    ; reserved                     : int
    ; version_number               : int
    ; current_next_indicator       : bool
    ; section_number               : int
    ; last_section_number          : int
    ; rfu_1                        : int
    ; network_descriptors_length   : int
    ; descriptors                  : Descriptor.t list
    ; rfu_2                        : int
    ; transport_stream_loop_length : int
    ; transport_streams            : ts list
    ; crc32                        : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    match%bitstring rest with
    | {| network_id                   : 16
       ; reserved                     : 2
       ; version_number               : 5
       ; current_next_indicator       : 1
       ; section_number               : 8
       ; last_section_number          : 8
       ; rfu_1                        : 4
       ; network_descriptors_length   : 12
       ; descriptors                  : network_descriptors_length * 8 : bitstring
       ; rfu_2                        : 4
       ; transport_stream_loop_length : 12
       ; transport_streams            : transport_stream_loop_length * 8 : bitstring
       ; crc32                        : 32
       |} -> { header; network_id; reserved; version_number; current_next_indicator; section_number;
               last_section_number; rfu_1; network_descriptors_length;
               descriptors = parse_descriptors [] descriptors;
               rfu_2; transport_stream_loop_length;
               transport_streams = parse_ts [] transport_streams; crc32 }

end

module BAT = struct

  open Table_common

  type t =
    { header                       : header
    ; bouquet_id                   : int
    ; reserved                     : int
    ; version_number               : int
    ; current_next_indicator       : bool
    ; section_number               : int
    ; last_section_number          : int
    ; rfu_1                        : int
    ; bouquet_descriptors_length   : int
    ; descriptors                  : Descriptor.t list
    ; rfu_2                        : int
    ; transport_stream_loop_length : int
    ; transport_streams            : ts list
    ; crc32                        : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    match%bitstring rest with
    | {| bouquet_id                   : 16
       ; reserved                     : 2
       ; version_number               : 5
       ; current_next_indicator       : 1
       ; section_number               : 8
       ; last_section_number          : 8
       ; rfu_1                        : 4
       ; bouquet_descriptors_length   : 12
       ; descriptors                  : bouquet_descriptors_length * 8 : bitstring
       ; rfu_2                        : 4
       ; transport_stream_loop_length : 12
       ; transport_streams            : transport_stream_loop_length * 8 : bitstring
       ; crc32                        : 32
       |} -> { header; bouquet_id; reserved; version_number; current_next_indicator; section_number;
               last_section_number; rfu_1; bouquet_descriptors_length;
               descriptors = Table_common.parse_descriptors [] descriptors;
               rfu_2; transport_stream_loop_length;
               transport_streams = Table_common.parse_ts [] transport_streams; crc32 }

end

module SDT = struct

  open Table_common

  type service =
    { service_id                 : int
    ; rfu                        : int
    ; eit_schedule_flag          : bool
    ; eit_present_following_flag : bool
    ; running_status             : int
    ; free_ca_mode               : bool
    ; descriptors_loop_length    : int
    ; descriptors                : Descriptor.t list
    } [@@deriving yojson]

  type t =
    { header                 : header
    ; transport_stream_id    : int
    ; reserved               : int
    ; version_number         : int
    ; current_next_indicator : bool
    ; section_number         : int
    ; last_section_number    : int
    ; original_network_id    : int
    ; rfu                    : int
    ; services               : service list
    ; crc32                  : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let rec parse_services = (fun acc x ->
        if Bitstring.bitstring_length x = 0 then List.rev acc
        else (match%bitstring x with
              | {| service_id                 : 16
                 ; rfu                        : 6
                 ; eit_schedule_flag          : 1
                 ; eit_present_following_flag : 1
                 ; running_status             : 3
                 ; free_ca_mode               : 1
                 ; descriptors_loop_length    : 12
                 ; descriptors                : descriptors_loop_length * 8 : bitstring
                 ; rest                       : -1 : bitstring
                 |} -> parse_services ({ service_id; rfu; eit_schedule_flag; eit_present_following_flag;
                                         running_status; free_ca_mode; descriptors_loop_length;
                                         descriptors = parse_descriptors [] descriptors} :: acc)
                         rest)) in
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    let len         = Bitstring.bitstring_length rest in
    match%bitstring rest with
    | {| transport_stream_id    : 16
       ; reserved               : 2
       ; version_number         : 5
       ; current_next_indicator : 1
       ; section_number         : 8
       ; last_section_number    : 8
       ; original_network_id    : 16
       ; rfu                    : 8 : save_offset_to (off)
       ; services               : len - off - 8 - 32 : bitstring
       ; crc32                  : 32
       |} -> { header; transport_stream_id; reserved; version_number; current_next_indicator;
               section_number; last_section_number; original_network_id; rfu;
               services = parse_services [] services; crc32 }

end

module EIT = struct

  open Table_common

  type event =
    { event_id                : int
    ; start_time              : int64
    ; duration                : int
    ; running_status          : int
    ; free_ca_mode            : bool
    ; descriptors_loop_length : int
    ; descriptors             : Descriptor.t list
    } [@@deriving yojson]

  type t =
    { header                      : header
    ; service_id                  : int
    ; reserved                    : int
    ; version_number              : int
    ; current_next_indicator      : bool
    ; section_number              : int
    ; last_section_number         : int
    ; transport_stream_id         : int
    ; original_network_id         : int
    ; segment_last_section_number : int
    ; last_table_id               : int
    ; events                      : event list
    ; crc32                       : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let rec parse_events = (fun acc x ->
        if Bitstring.bitstring_length x = 0 then List.rev acc
        else (match%bitstring x with
              | {| event_id                   : 16
                 ; start_time                 : 40
                 ; duration                   : 24
                 ; running_status             : 3
                 ; free_ca_mode               : 1
                 ; descriptors_loop_length    : 12
                 ; descriptors                : descriptors_loop_length * 8 : bitstring
                 ; rest                       : -1 : bitstring
                 |} -> parse_events ({ event_id; start_time; duration; running_status; free_ca_mode;
                                       descriptors_loop_length;
                                       descriptors = parse_descriptors [] descriptors} :: acc)
                         rest)) in
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    match%bitstring rest with
    | {| service_id                  : 16
       ; reserved                    : 2
       ; version_number              : 5
       ; current_next_indicator      : 1
       ; section_number              : 8
       ; last_section_number         : 8
       ; transport_stream_id         : 16
       ; original_network_id         : 16
       ; segment_last_section_number : 8
       ; last_table_id               : 8 : save_offset_to (off)
       ; events                      : Bitstring.bitstring_length rest - off - 8 - 32 : bitstring
       ; crc32                       : 32
       |} -> { header; service_id; reserved; version_number; current_next_indicator; section_number;
               last_section_number; transport_stream_id; original_network_id; segment_last_section_number;
               last_table_id; events = parse_events [] events; crc32 }

end

module TDT = struct

  open Table_common

  type t =
    { header   : header
    ; utc_time : int64
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    match%bitstring rest with
    | {| utc_time : 40 |} -> { header; utc_time }

end

module TOT = struct

  open Table_common

  type t =
    { header                  : header
    ; utc_time                : int64
    ; reserved                : int
    ; descriptors_loop_length : int
    ; descriptors             : Descriptor.t list
    ; crc32                   : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    match%bitstring rest with
    | {| utc_time               : 40
       ; reserved               : 4
       ; descriptors_loop_length : 12
       ; descriptors            : descriptors_loop_length * 8 : bitstring
       ; crc32                  : 32
       |} -> { header; utc_time; reserved; descriptors_loop_length;
               descriptors = parse_descriptors [] descriptors; crc32 }

end

module RST = struct

  open Table_common

  type event =
    { transport_stream_id : int
    ; original_network_id : int
    ; service_id          : int
    ; event_id            : int
    ; rfu                 : int
    ; running_status      : int
    } [@@deriving yojson]

  type t =
    { header : header
    ; events : event list
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    let rec parse_events = fun acc x ->
      if Bitstring.bitstring_length x = 0 then List.rev acc
      else (match%bitstring x with
            | {| transport_stream_id        : 16
               ; original_network_id        : 16
               ; service_id                 : 16
               ; event_id                   : 16
               ; rfu                        : 5
               ; running_status             : 3
               ; rest                       : -1 : bitstring
               |} -> parse_events ({ transport_stream_id; original_network_id; service_id; event_id;
                                     rfu; running_status} :: acc) rest) in
    { header; events = parse_events [] rest }


end

module ST = struct

  open Table_common

  type t =
    { header : header
    ; bytes  : string
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    { header; bytes = Bitstring.string_of_bitstring rest }

end

module DIT = struct

  open Table_common

  type t =
    { header          : header
    ; transition_flag : bool
    ; rfu             : int
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    match%bitstring rest with
    | {| transition_flag : 1
       ; rfu             : 7
       ; rest            : -1 : bitstring
       |} when Bitstring.bitstring_length rest = 0 -> { header; transition_flag; rfu }

end

module SIT = struct

  open Table_common
  open Bitstring

  type service =
    { service_id          : int
    ; dvb_rfu             : bool
    ; running_status      : int
    ; service_loop_length : int
    ; descriptors         : Descriptor.t list
    } [@@deriving yojson]

  type t =
    { header                        : header
    ; dvb_rfu_1                     : int
    ; iso_reserved                  : int
    ; version_number                : int
    ; current_next_indicator        : bool
    ; section_number                : int
    ; last_section_number           : int
    ; dvb_rfu_2                     : int
    ; transmission_info_loop_length : int
    ; descriptors                   : Descriptor.t list
    ; services                      : service list
    ; crc32                         : int32
    } [@@deriving yojson]

  let of_cbuffer buf =
    let bs = bitstring_of_string @@ Cbuffer.to_string buf in
    let header,rest = parse_header bs in
    let len = bitstring_length rest in
    let rec parse_services = fun acc x ->
      if Bitstring.bitstring_length x = 0 then List.rev acc
      else (match%bitstring x with
            | {| service_id          : 16
               ; dvb_rfu             : 1
               ; running_status      : 3
               ; service_loop_length : 12
               ; descriptors         : service_loop_length * 8 : bitstring
               ; rest                : -1 : bitstring
               |} -> parse_services ({ service_id
                                     ; dvb_rfu
                                     ; running_status
                                     ; service_loop_length
                                     ; descriptors = parse_descriptors [] descriptors
                                     } :: acc) rest) in
    match%bitstring rest with
    | {| dvb_rfu_1                     : 16
       ; iso_reserved                  : 2
       ; version_number                : 5
       ; current_next_indicator        : 1
       ; section_number                : 8
       ; last_section_number           : 8
       ; dvb_rfu_2                     : 4
       ; transmission_info_loop_length : 12
       ; descriptors                   : transmission_info_loop_length * 8 : bitstring, save_offset_to (off)
       ; services                      : len - off - (transmission_info_loop_length * 8) - 32 : bitstring
       ; crc32                         : 32
       ; rest                          : -1 : bitstring
       |} when bitstring_length rest = 0 -> { header; dvb_rfu_1; iso_reserved; version_number;
                                              current_next_indicator; section_number; last_section_number;
                                              dvb_rfu_2; transmission_info_loop_length;
                                              descriptors = parse_descriptors [] descriptors;
                                              services = parse_services [] services; crc32 }

end
