open Containers
open Board_types.Streams.TS

let ( % ) = Fun.( % )

module Bitstring = struct
  include Bitstring
  let of_string = bitstring_of_string
  let to_string = string_of_bitstring
  let to_cstruct = Cstruct.of_string % string_of_bitstring
  let length = bitstring_length
end

let value_to_string name x = Printf.sprintf "%s (%d)" name x
let rfu_to_string          = value_to_string "Reserved"

let to_node ?parsed ~offset length name value =
  { offset; length; name; value = value, parsed }

let parse_date (days:int) : Ptime.date option =
  let date = Option.get_exn @@ Ptime.of_date @@ (1858, 11, 17) in
  let sec  = 3600 * 24 * days in
  let span = Ptime.Span.of_int_s sec in

  Ptime.add_span date span
  |> Option.map Ptime.to_date

let parse_time (bs:Bitstring.t) : Ptime.time option =
  try
    match Bitstring.length bs with
    | 24 -> (match%bitstring bs with
             | {| hr1  : 4
                ; hr2  : 4
                ; min1 : 4
                ; min2 : 4
                ; sec1 : 4
                ; sec2 : 4
                |} ->
                let hr, min, sec = hr1 * 10 + hr2, min1 * 10 + min2, sec1 * 10 + sec2 in
                Some ((hr, min, sec), 0))
    | 16 -> (match%bitstring bs with
             | {| hr1  : 4
                ; hr2  : 4
                ; min1 : 4
                ; min2 : 4
                |} ->
                let hr, min = hr1 * 10 + hr2, min1 * 10 + min2 in
                Some ((hr, min, 0), 0))
    | _ -> None
  with _ -> None

let parse_timestamp (bs:Bitstring.t) : Ptime.t option =
  try
    match%bitstring bs with
    | {| x    : 40 |} when Int64.equal x 0xFFFFFFFFFFL -> None
    | {| date : 16
       ; time : 24 : bitstring
       |} ->
       let date = parse_date date in
       let time = parse_time time in
       Option.map2 (fun date time ->
           Ptime.of_date_time (date, time)) date time
       |> Option.flatten
  with _ -> None

let parse_duration (bs:Bitstring.t) : Ptime.span option =
  let time = parse_time bs in
  let date = Ptime.to_date @@ Ptime.epoch in
  let ts   = Option.flat_map (fun x -> Ptime.of_date_time (date, x)) time in
  Option.map Ptime.to_span ts

let parsed_length = List.fold_left (fun acc x -> x.length + acc) 0

let rec parse_bytes ?bytes ~offset x str =
  let bytes = match bytes with
    | Some x -> x
    | None   -> 1
  in
  if Bitstring.length x = 0 then []
  else match%bitstring x with
       | {| smth : bytes * 8
          ; rest : -1 : save_offset_to (off_1), bitstring
          |} ->
          let node = to_node ~offset 8 str (Bits (Int64 smth)) in
          node :: parse_bytes ~offset:(offset + off_1) rest str

let parse_lang_code code =
  match%bitstring code with
  | {| s : 24 : string |} -> s,
                             match%bitstring code with
                             | {| d : 24 : int |} -> d

module Descriptor = struct

  module type Descriptor_base = sig

    type t [@@deriving yojson]
    val name   : string
    val decode : int -> Bitstring.t -> parsed

  end

  (* 0x00
   * Reserved
   *)

  (* 0x01
   * Forbidden
   *)

  (* 0x02 *)
  module Video_stream = struct

    let name = "video_stream_descriptor"

    let frame_rate_to_string = function
      | 0b0000  -> "Forbidden" | 0b0001 -> "23.976" | 0b0010 -> "24"
      | 0b0011  -> "25"        | 0b0100 -> "29.97"  | 0b0101 -> "30"
      | 0b0110  -> "50"        | 0b0111 -> "59.94"  | 0b1000 -> "60"
      | x -> rfu_to_string x

    let chroma_format_to_string = function
      | 0b01 -> "4:2:0" | 0b10 -> "4:2:2"
      | 0b11 -> "4:4:4" | x   -> rfu_to_string x

    let decode bs off =
      match%bitstring bs with
      | {| mfr_flag        : 1
         ; frame_rate      : 4 : save_offset_to (off_1)
         ; true            : 1 : save_offset_to (off_2)
         ; const_par_flag  : 1 : save_offset_to (off_3)
         ; still_pict_flag : 1 : save_offset_to (off_4)
         |} ->
         let fr = frame_rate_to_string frame_rate in
         [ to_node ~offset:off 1 "mfr_flag" (Bits (Bool mfr_flag))
         ; to_node ~parsed:fr ~offset:(off + off_1) 4 "frame_rate" (Hex (Int frame_rate))
         ; to_node ~offset:(off + off_2) 1 "MPEG_1_only_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_3) 1 "constrained_parameter_flag" (Bits (Bool const_par_flag))
         ; to_node ~offset:(off + off_4) 1 "still_picture_flag" (Bits (Bool still_pict_flag)) ]
      | {| mfr_flag        : 1
         ; frame_rate      : 4 : save_offset_to (off_1)
         ; false           : 1 : save_offset_to (off_2)
         ; const_par_flag  : 1 : save_offset_to (off_3)
         ; still_pict_flag : 1 : save_offset_to (off_4)
         ; profile_lvl_ind : 8 : save_offset_to (off_5)
         ; chroma_format   : 2 : save_offset_to (off_6)
         ; fr_ext_flag     : 1 : save_offset_to (off_7)
         ; reserved        : 5 : save_offset_to (off_8)
         |} ->
         let chroma = chroma_format_to_string chroma_format in
         let fr = frame_rate_to_string frame_rate in
         [ to_node ~offset:off 1 "mfr_flag" (Bits (Bool mfr_flag))
         ; to_node ~parsed:fr ~offset:(off + off_1) 4 "frame_rate" (Hex (Int frame_rate))
         ; to_node ~offset:(off + off_2) 1 "MPEG_1_only_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_3) 1 "constrained_parameter_flag" (Bits (Bool const_par_flag))
         ; to_node ~offset:(off + off_4) 1 "still_picture_flag" (Bits (Bool still_pict_flag))
         ; to_node ~offset:(off + off_5) 8 "profile_and_level_indication" (Hex (Int profile_lvl_ind))
         ; to_node ~parsed:chroma ~offset:(off + off_6) 2 "chroma_format" (Bits (Bool true))
         ; to_node ~offset:(off + off_7) 1 "frame_rate_extension_flag" (Bits (Bool fr_ext_flag))
         ; to_node ~offset:(off + off_8) 5 "reserved" (Bits (Int reserved)) ]
  end

  (* 0x03 *)
  module Audio_stream = struct

    let name = "audio_stream_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| free_format_flag : 1
         ; id               : 1 : save_offset_to (off_1)
         ; layer            : 2 : save_offset_to (off_2)
         ; variable_rate    : 1 : save_offset_to (off_3)
         ; reserved         : 3 : save_offset_to (off_4)
         |} ->
         [ to_node ~offset:off 1 "free_format_flag" (Bits (Bool free_format_flag))
         ; to_node ~offset:(off + off_1) 1 "id" (Bits (Bool id))
         ; to_node ~offset:(off + off_2) 2 "layer" (Dec (Int layer))
         ; to_node ~offset:(off + off_3) 1 "variable_rate" (Bits (Bool variable_rate))
         ; to_node ~offset:(off + off_4) 3 "reserved" (Bits (Int reserved)) ]
  end

  (* 0x04 *)
  module Hierarchy = struct

    let name = "hierarchy_descriptor"

    let hierarchy_to_string = function
      | 1  -> "Spatial_scalability"
      | 2  -> "SNR_scalability"
      | 3  -> "Temporal_scalability"
      | 4  -> "Data_partitioning"
      | 5  -> "Extension_bitstream"
      | 6  -> "Private_stream"
      | 7  -> "Multi_view_profile"
      | 8  -> "Combined_scalability"
      | 9  -> "MVC_or_MVCD"
      | 15 -> "Base_layer_etc"
      | _  -> "Reserved"

    let decode bs off =
      match%bitstring bs with
      | {| reserved_1           : 1
         ; temporal_scalability : 1 : save_offset_to (off_1)
         ; spatial_scalability  : 1 : save_offset_to (off_2)
         ; quality_scalability  : 1 : save_offset_to (off_3)
         ; hier_typ             : 4 : save_offset_to (off_4)
         ; reserved_2           : 2 : save_offset_to (off_5)
         ; hierarchy_index      : 6 : save_offset_to (off_6)
         ; tref_present_flag    : 1 : save_offset_to (off_7)
         ; reserved_3           : 1 : save_offset_to (off_8)
         ; emb_index            : 6 : save_offset_to (off_9)
         ; reserved_4           : 2 : save_offset_to (off_10)
         ; hierarchy_channel    : 6 : save_offset_to (off_11)
         |} ->
         let parsed = hierarchy_to_string hier_typ in
         [ to_node ~offset:off 1 "reserved_1" (Bits (Bool reserved_1))
         ; to_node ~offset:(off_1 + off)  1 "temporal_scalability" (Bits (Bool temporal_scalability))
         ; to_node ~offset:(off_2 + off)  1 "quality_scalability" (Bits (Bool quality_scalability))
         ; to_node ~offset:(off_3 + off)  1 "spatial_scalability" (Bits (Bool spatial_scalability))
         ; to_node ~parsed ~offset:(off_4 + off) 4 "hierarchy_type" (Hex (Int hier_typ))
         ; to_node ~offset:(off_5 + off)  2 "reserved_2" (Bits (Int reserved_2))
         ; to_node ~offset:(off_6 + off)  6 "hierarchy_layer_index" (Dec (Int hierarchy_index))
         ; to_node ~offset:(off_7 + off)  1 "tref_present_flag" (Bits (Bool tref_present_flag))
         ; to_node ~offset:(off_8 + off)  1 "reserved_3" (Bits (Bool reserved_3))
         ; to_node ~offset:(off_9 + off)  6 "hierarchy_embedded_layer_index" (Dec (Int emb_index))
         ; to_node ~offset:(off_10 + off) 2 "reserved_4" (Bits (Int reserved_4))
         ; to_node ~offset:(off_11 + off) 6 "hierarchy_channel" (Dec (Int hierarchy_channel)) ]

  end

  (* 0x05 *)
  module Registration = struct

    let name = "registration_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| format_id : 32
         ; rest      : -1 : save_offset_to (off_1), bitstring
         |} ->
         let info = match Text_decoder.decode @@ Bitstring.to_cstruct rest with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         [ to_node ~offset:off 32 "format_identifier" (Hex (Int32 format_id))
         ; to_node ~offset:(off + off_1) (Bitstring.length rest)
             "additional_identification_info" (String info) ]

  end

  (* 0x06 *)
  module Data_stream_alignment = struct

    let name = "data_stream_alignment_descriptor"

    let alignment_to_string alignment = match alignment with
        | 00 -> "Reserved"
        | 01 -> "Slice, or video access unit"
        | 02 -> "Video access unit"
        | 03 -> "GOP, or SEQ"
        | 04 -> "SEQ"
        | _  -> "Reserved"

    let decode bs off =
      match%bitstring bs with
      | {| alignment_type : 8 |} ->
         let parsed = alignment_to_string alignment_type in
         [ to_node ~parsed ~offset:off 1 "alignment_type" (Hex (Int alignment_type)) ]

  end

  (* 0x07 *)
  module Target_background_grid = struct

    let name = "target_background_grid_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| horizontal_size : 14
         ; vertical_size   : 14 : save_offset_to (off_1)
         ; asp_ratio_inf   : 4  : save_offset_to (off_2)
         |} ->
         [ to_node ~offset:off 14 "horizontal_size" (Dec (Int horizontal_size))
         ; to_node ~offset:(off_1 + off) 14 "vertical_size" (Dec (Int vertical_size))
         ; to_node ~offset:(off_2 + off) 4  "aspect_ratio_information" (Hex (Int asp_ratio_inf)) ]

  end

  (* 0x08 *)
  module Video_window = struct

    let name = "video_window_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| horizontal_offset : 14
         ; vertical_offset   : 14 : save_offset_to (off_1)
         ; window_priority   : 4  : save_offset_to (off_2)
         |} ->
         [ to_node ~offset:off 14 "horizontal_offset" (Dec (Int horizontal_offset))
         ; to_node ~offset:(off_1 + off) 14 "vertical_offset" (Dec (Int vertical_offset))
         ; to_node ~offset:(off_2 + off) 4  "window_priority" (Dec (Int window_priority)) ]

  end

  (* 0x09 *)
  module CA = struct

    let name = "CA_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| ca_system_id : 16
         ; reserved     : 3  : save_offset_to (off_1)
         ; ca_pid       : 13 : save_offset_to (off_2)
         ; private_data : -1 : save_offset_to (off_3), bitstring
         |} ->
         let nodes =
           [ to_node ~offset:off 16 "CA_system_id" (Hex (Int ca_system_id))
           ; to_node ~offset:(off_1 + off) 3  "reserved" (Bits (Int reserved))
           ; to_node ~offset:(off_2 + off) 13 "CA_PID" (Hex (Int ca_pid)) ]
         in
         nodes @ parse_bytes ~offset:(off + off_3) private_data "private_data_byte"


  end

  (* 0x0A *)
  module ISO_639_language= struct

    let audio_to_string = function
      | 0x00 -> "Undefined"
      | 0x01 -> "Clean_effects"
      | 0x02 -> "Hearing_impaired"
      | 0x03 -> "Visual_impaired_commentary"
      | x when x >= 0x04 && x <= 0x7F -> "User_private x"
      | _ -> "Reserved"

    let name = "ISO_639_language_descriptor"

    let rec f = fun off x ->
      if Bitstring.length x = 0 then []
      else (match%bitstring x with
            | {| lang_code : 24 : bitstring
               ; audio     : 8  : save_offset_to (off_1)
               ; rest      : -1 : save_offset_to (off_2), bitstring
               |} ->
               let typ = audio_to_string audio in
               let lang, lang_code = parse_lang_code lang_code in
               let nodes =
                 [ to_node ~parsed:lang ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
                 ; to_node ~parsed:typ  ~offset:(off_1 + off) 8 "audio_type" (Hex (Int audio)) ]
               in
               let node = to_node ~offset:off 32 lang (List nodes) in
               node :: f (off + off_2) rest)

    let decode bs off = f off bs

  end

  (* 0x0B *)
  module System_clock = struct

    let name = "system_clock_descriptor"

    let external_clock_ref_indicator_of_bool ecri =
      if ecri
      then "system clock has derived"
      else "system clock has not derived"

    let decode bs off =
      match%bitstring bs with
      | {| ext_clock_ref_ind : 1
         ; reserved_1        : 1 : save_offset_to (off_1)
         ; clock_acc_int     : 6 : save_offset_to (off_2)
         ; clock_acc_exp     : 3 : save_offset_to (off_3)
         ; reserved_2        : 5 : save_offset_to (off_4)
         |} ->
         [ to_node ~offset:off 1 "external_clock_reference_indicator" (Bits (Bool ext_clock_ref_ind))
         ; to_node ~offset:(off_1 + off) 1 "reserved" (Bits (Bool reserved_1))
         ; to_node ~offset:(off_2 + off) 6 "clock_accuracy_integer" (Dec (Int clock_acc_int))
         ; to_node ~offset:(off_3 + off) 3 "clock_accuracy_exponent" (Dec (Int clock_acc_exp))
         ; to_node ~offset:(off_4 + off) 5 "reserved" (Bits (Int reserved_2)) ]

  end

  (* 0x0C *)
  module Multiplex_buffer_utilization = struct

    let name = "multiplex_buffer_utilization_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| bound_valid_flag : 1
         ; lower_bound      : 15 : save_offset_to (off_1)
         ; reserved         : 1  : save_offset_to (off_2)
         ; upper_bound      : 15 : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 1 "bound_valid_flag" (Bits (Bool bound_valid_flag))
         ; to_node ~offset:(off_1 + off) 15 "LTW_offset_lower_bound" (Dec (Int lower_bound))
         ; to_node ~offset:(off_2 + off) 1  "reserved" (Bits (Bool reserved))
         ; to_node ~offset:(off_3 + off) 15 "LTW_offset_upper_bound" (Dec (Int upper_bound)) ]

  end

  (* 0x0D *)
  module Copyright = struct

    let name = "copyright_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| copyright_id : 32
         ; rest         : -1 : save_offset_to (off_1), bitstring
         |} ->
         let info = match Text_decoder.decode @@ Bitstring.to_cstruct rest with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         [ to_node ~offset:off 32 "copyright_identifier" (Hex (Int32 copyright_id))
         ; to_node ~offset:(off + off_1) (Bitstring.length rest)
             "additional copyright info" (String info) ]

  end

  (* 0x0E *)
  module Maximum_bitrate = struct

    let name = "maximum_bitrate_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| reserved        : 2
         ; maximum_bitrate : 22 : save_offset_to (off_1)
         |} ->
         [ to_node ~offset:off 2 "reserved" (Bits (Int reserved))
         ; to_node ~offset:(off+off_1) 22 "maximum_bitrate" (Dec (Uint maximum_bitrate)) ]

  end

  (* 0x0F *)
  module Private_data_indicator = struct

    let name = "private_data_indicator_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| private_data_indicator : 32
         |} ->
         [ to_node ~offset:off 32 "private_data_indicator" (Hex (Int32 private_data_indicator)) ]

  end

  (* 0x10 *)
  module Smoothing_buffer = struct

    let name = "smoothing_buffer_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| reserved_1   : 2
         ; sb_leak_rate : 22 : save_offset_to (off_1)
         ; reserved_2   : 2  : save_offset_to (off_2)
         ; sb_size      : 22 : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 2 "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off_1 + off) 22 "sb_leak_rate" (Dec (Int sb_leak_rate))
         ; to_node ~offset:(off_2 + off) 2  "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off_3 + off) 22 "sb_size" (Dec (Int sb_size)) ]

  end

  (* 0x11 *)
  module STD_descriptor = struct

    let name = "STD_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| reserved        : 7
         ; leak_valid_flag : 1 : save_offset_to (off_1)
         |} ->
         [ to_node ~offset:off 7 "reserved" (Bits (Int reserved))
         ; to_node ~offset:(off_1 + off) 1 "leak_valid_flag" (Bits (Bool leak_valid_flag)) ]

  end

  (* 0x12 *)
  module IBP_descriptor = struct

    let name = "IBP_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| closed_gop_flag    : 1
         ; identical_gop_flag : 1  : save_offset_to (off_1)
         ; max_gop_length     : 14 : save_offset_to (off_2)
         |} ->
         [ to_node ~offset:off 7 "closed_gop_flag" (Bits (Bool closed_gop_flag))
         ; to_node ~offset:(off_1 + off) 1 "identical_gop_flag" (Bits (Bool identical_gop_flag))
         ; to_node ~offset:(off_2 + off) 1 "max_gop_length" (Dec (Int max_gop_length)) ]

  end

  (* 0x13 - 0x1A
   * Defined in ISO/IEC 13818-6
   *)

  (* 0x1B *)
  module MPEG4_video = struct

    let name = "MPEG-4_video_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| mpeg_4_visual : 8
         |} ->
         [ to_node ~offset:off 8 "MPEG-4_visual_profile_and_level" (Hex (Int mpeg_4_visual)) ]

  end

  (* 0x1C *)
  module MPEG4_audio = struct

    let name = "MPEG-4_audio_descriptor"

    let audio_value_of_int = function
      | 0x0F -> "No_audio_profile"
      | 0x10 -> "Main profile, level 1"
      | 0x11 -> "Main profile, level 2"
      | 0x12 -> "Main profile, level 3"
      | 0x13 -> "Main profile, level 4"
      | 0x18 -> "Scalable profile, level 1"
      | 0x19 -> "Scalable profile, level 2"
      | 0x1A -> "Scalable profile, level 3"
      | 0x1B -> "Scalable profile, level 4"
      | 0x20 -> "Speech profile, level 1"
      | 0x21 -> "Speech profile, level 2"
      | 0x28 -> "Synthesis profile, level 1"
      | 0x29 -> "Synthesis profile, level 2"
      | 0x2A -> "Synthesis profile, level 3"
      | 0x30 -> "High quality audio profile, level 1"
      | 0x31 -> "High quality audio profile, level 2"
      | 0x32 -> "High quality audio profile, level 3"
      | 0x33 -> "High quality audio profile, level 4"
      | 0x34 -> "High quality audio profile, level 5"
      | 0x35 -> "High quality audio profile, level 6"
      | 0x36 -> "High quality audio profile, level 7"
      | 0x37 -> "High quality audio profile, level 8"
      | 0x38 -> "Low delay audio profile, level 1"
      | 0x39 -> "Low delay audio profile, level 2"
      | 0x3A -> "Low delay audio profile, level 3"
      | 0x3B -> "Low delay audio profile, level 4"
      | 0x3C -> "Low delay audio profile, level 5"
      | 0x3D -> "Low delay audio profile, level 6"
      | 0x3E -> "Low delay audio profile, level 7"
      | 0x3F -> "Low delay audio profile, level 8"
      | 0x40 -> "Natural audio profile, level 1"
      | 0x41 -> "Natural audio profile, level 2"
      | 0x42 -> "Natural audio profile, level 3"
      | 0x43 -> "Natural audio profile, level 4"
      | 0x48 -> "Mobile audio internetworking profile, level 1"
      | 0x49 -> "Mobile audio internetworking profile, level 2"
      | 0x4A -> "Mobile audio internetworking profile, level 3"
      | 0x4B -> "Mobile audio internetworking profile, level 4"
      | 0x4C -> "Mobile audio internetworking profile, level 5"
      | 0x4D -> "Mobile audio internetworking profile, level 6"
      | 0x50 -> "AAC profile, level 1"
      | 0x51 -> "AAC profile, level 2"
      | 0x52 -> "AAC profile, level 4"
      | 0x53 -> "AAC profile, level 5"
      | 0x58 -> "High efficiency AAC profile, level 2"
      | 0x59 -> "High efficiency AAC profile, level 3"
      | 0x5A -> "High efficiency AAC profile, level 4"
      | 0x5B -> "High efficiency AAC profile, level 5"
      | 0x60 -> "High efficiency AAC v2 profile, level 2"
      | 0x61 -> "High efficiency AAC v2 profile, level 3"
      | 0x62 -> "High efficiency AAC v2 profile, level 4"
      | 0x63 -> "High efficiency AAC v2 profile, level 5"
      | 0xFF -> "Not_specified"
      | _    -> "Reserved"

    let decode bs off =
      match%bitstring bs with
      | {| mpeg_4_audio : 8
         |} ->
         let parsed = audio_value_of_int mpeg_4_audio in
           [ to_node
               ~parsed
               ~offset:off
               7
               "MPEG-4_audio_profile_and_level"
               (Hex (Int mpeg_4_audio)) ]

  end

  (* 0x1D *)
  module IOD = struct

    let name = "IOD_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| scope_of_iod_label : 8
         ; iod_label          : 8 : save_offset_to (off_1)
         ; initial_obj_desc   : 8 : save_offset_to (off_2)
         |} ->
         [ to_node ~offset:off 8 "Scope_of_IOD_label" (Hex (Int scope_of_iod_label))
         ; to_node ~offset:(off_1 + off) 8 "IOD_label" (Hex (Int iod_label))
         ; to_node ~offset:(off_2 + off) 8 "InitialObjectDesc" (Hex (Int initial_obj_desc)) ]

  end

  (* 0x1E *)
  module SL= struct

    let name = "SL_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| es_id : 16
         |} ->
         [ to_node ~offset:off 16 "ES_ID" (Hex (Int es_id)) ]

  end

  (* 0x1F *)
  module FMC = struct

    let name = "FMC_descriptor"

    let rec f = fun off x ->
      if Bitstring.length x = 0 then []
      else (match%bitstring x with
            | {| es_id       : 16
               ; flex_mux_ch : 8  : save_offset_to (off_1)
               ; rest        : -1 : save_offset_to (off_2), bitstring
               |} ->
               let nodes =
                 [ to_node ~offset:off 16 "ES_ID" (Hex (Int es_id))
                 ; to_node ~offset:(off + off_1) 8 "FlexMuxChannel" (Hex (Int flex_mux_ch))]
               in
               let id = Printf.sprintf "ES_ID %s" (string_of_int es_id) in
               let node = to_node ~offset:off 24 id (List nodes) in
               node :: f (off + off_2) rest)

    let decode bs off = f off bs

  end

  (* 0x20 *)
  module ES_ID = struct

    let name = "external_ES_ID descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| ext_es_id : 16
         |} ->
         [ to_node ~offset:off 16 "External_ES_ID" (Hex (Int ext_es_id)) ]

  end

  (* 0x21 *)
  module Mux_code = struct

    let name = "MuxCode_descriptor"

    let rec f_2 ~offset ~i ~k x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| channel : 8
              ; num     : 8  : save_offset_to (off_1)
              ; rest    : -1 : save_offset_to (off_2), bitstring
              |} ->
              let s_1 = Printf.sprintf "flexMuxChannel[%d][%d]" i k in
              let s_2 = Printf.sprintf "numberOfBytes[%d][%d]" i k in
              let nodes =
                [ to_node ~offset 8 s_1 (Dec (Int channel))
                ; to_node ~offset:(offset + off_1) 8 s_2 (Dec (Int num)) ]
              in
              let channel_name = Printf.sprintf "Channel %s" (string_of_int channel) in
              let node = to_node ~offset 16 channel_name (List nodes) in
              node :: f_2 ~offset:(offset + off_2) ~i ~k:(k + 1) rest

    let rec f_1 ~offset ~i x =
      if Bitstring.length x = 0 then []
      else  match%bitstring x with
            | {| slot_count : 5
               ; rep_count  : 3 : save_offset_to (off_1)
               ; channels   : slot_count * 16 : save_offset_to (off_2), bitstring
               ; rest       : -1 : save_offset_to (off_3), bitstring
               |} ->
              let nodes =
                [ to_node ~offset 5 "slotCount" (Dec (Int slot_count))
                ; to_node ~offset:(offset + off_1) 3 "repetitionCount" (Dec (Int rep_count)) ]
              in
              let channels = nodes @ f_2 ~offset:(offset + off_2) ~i ~k:0 channels in
              let slot_name = Printf.sprintf "Slot %s" (string_of_int slot_count) in
              let node = to_node ~offset (8 + slot_count * 16) slot_name (List channels) in
              node :: f_1 ~offset:(offset + off_3) ~i:(i + 1) rest

    (* ISO/IEC 14496-1 11.2.4.3 *)
    let decode bs off =
      match%bitstring bs with
      | {| length    : 8
         ; mux_code  : 4  : save_offset_to (off_1)
         ; version   : 4  : save_offset_to (off_2)
         ; sub_count : 8  : save_offset_to (off_3)
         ; rest      : -1 : save_offset_to (off_4), bitstring
         |} ->
         let nodes =
           [ to_node ~offset:off 8 "length" (Dec (Int length))
           ; to_node ~offset:(off + off_1) 4 "MuxCode" (Dec (Int mux_code))
           ; to_node ~offset:(off + off_2) 4 "version" (Dec (Int version))
           ; to_node ~offset:(off + off_3) 8 "substructureCount" (Dec (Int sub_count)) ]
         in
         nodes @ f_1 ~offset:(off + off_4) ~i:0 rest

  end

  (* 0x22 *)
  module Fmx_buffer_size = struct
    (* TODO ITU-T Rec. H.222.0 page 89 and then to 11.2 of ISO/IEC 14496-1*)
    let name = "FmxBufferSize_descriptor"

    let decode _ _ = []

  end

  (* 0x23 *)
  module Multiplex_buffer = struct

    let name = "multiplexBuffer_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| mb_buffer_size : 24
         ; tb_leak_rate   : 24 : save_offset_to (off_1)
         |} ->
         [ to_node ~offset:off 24 "MB_buffer_size" (Hex (Int mb_buffer_size))
         ; to_node ~offset:(off + off_1) 24 "TB_leak_rate" (Hex (Int tb_leak_rate))
         ]

  end

  (* 0x24 *)
  module Content_labelling = struct
    (* FIXME *)
    (* This might be working, but can be re-organized I guess *)
    let name = "content_labelling_descriptor"

    let parse_time_base = function
      | 0 -> "No content time base defined in this descriptor"
      | 1 -> "Use of STC"
      | 2 -> "Use of NPT"
      | x when x > 7 && x < 16 -> "Use of privately defined content time base"
      | _ -> "Reserved"

    let parse_metadata_app = function
      | 0x0010 -> "ISO 15706 (ISAN) encoded in its binary form"
      | 0x0011 -> "ISO 15706-2 (V-ISAN) encoded in its binary form"
      | 0xFFFF -> "Defined by the metadata_application_format_identifier field"
      | x when x >= 0x0100 && x < 0xFFFF -> "User defined"
      | _      -> "Reserved"

    let decode_2 bs off tbi =
      match tbi with
      | 1 -> (match%bitstring bs with
             | {| reserved_1 : 7
                ; cont_value : 33 : save_offset_to (off_1)
                ; reserved_2 : 7  : save_offset_to (off_2)
                ; md_value   : 33 : save_offset_to (off_3)
                ; rest       : -1 : save_offset_to (off_4), bitstring
                |} ->
                let nodes =
                  [ to_node ~offset:off 7 "reserved" (Bits (Int reserved_1))
                  ; to_node ~offset:(off + off_1) 33 "content_time_base_value" (Hex (Int64 cont_value))
                  ; to_node ~offset:(off + off_2) 7  "reserved" (Bits (Int reserved_2))
                  ; to_node ~offset:(off + off_3) 33 "metadata_time_base_value" (Hex (Int64 md_value))
                  ]
                in
                nodes @ parse_bytes ~offset:(off + off_4) rest "private_data_byte"
             )
      | 2 -> (match%bitstring bs with
             | {| reserved_1 : 7
                ; cont_value : 33 : save_offset_to (off_1)
                ; reserved_2 : 7  : save_offset_to (off_2)
                ; md_value   : 33 : save_offset_to (off_3)
                ; reserved_3 : 1  : save_offset_to (off_4)
                ; contentid  : 7  : save_offset_to (off_5)
                ; rest       : -1 : save_offset_to (off_6), bitstring
                |} ->
                let nodes =
                  [ to_node ~offset:off 7 "reserved" (Bits (Int reserved_1))
                  ; to_node ~offset:(off + off_1) 33 "content_time_base_value" (Hex (Int64 cont_value))
                  ; to_node ~offset:(off + off_2) 7  "reserved" (Bits (Int reserved_2))
                  ; to_node ~offset:(off + off_3) 33 "metadata_time_base_value" (Hex (Int64 md_value))
                  ; to_node ~offset:(off + off_4) 1  "reserved" (Bits (Bool reserved_3))
                  ; to_node ~offset:(off + off_5) 33 "contentId" (Hex (Int contentid)) ]
                in
                nodes @ parse_bytes ~offset:(off + off_6) rest "private_data_byte"
             )
      | _ -> (match%bitstring bs with
              | {| tbad_length : 8
                 ; reserved    : tbad_length * 8 : save_offset_to (off_1), bitstring
                 ; rest        : -1  : save_offset_to (off_2), bitstring
                 |} ->
                let nodes =
                  [ to_node ~offset:off 8 "time_base_association_data_length" (Bits (Int tbad_length))]
                in
                let nodes = nodes @ (parse_bytes ~offset:(off + off_1) reserved "reserved") in
                nodes @ parse_bytes ~offset:(off + off_2) rest "private_data_byte"
             )

    let decode_1 bs off =
      match%bitstring bs with
      | {| true             : 1
         ; ctb_ind          : 4 : save_offset_to (off_1)
         ; reserved_1       : 3 : save_offset_to (off_2)
         ; crir_len         : 8 : save_offset_to (off_3)
         ; cont_ref_id_byte : crir_len * 8 : save_offset_to (off_4), bitstring
         ; rest             : -1 : save_offset_to (off_5), bitstring
         |} ->
         let parsed = parse_time_base ctb_ind in
         let cri = match Text_decoder.decode @@ Bitstring.to_cstruct cont_ref_id_byte with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         let nodes =
           [ to_node ~offset:off 1 "content_reference_id_record_flag" (Bits (Bool true))
           ; to_node ~parsed ~offset:(off + off_1) 4 "content_time_base_indicator" (Hex (Int ctb_ind))
           ; to_node ~offset:(off + off_2) 3 "reserved" (Bits (Int reserved_1))
           ; to_node ~offset:(off + off_3) 8 "content_reference_id_record_length" (Dec (Int crir_len))
           ; to_node ~offset:(off + off_4) (crir_len * 8) "content_reference_id" (String cri)
           ]
         in
         nodes @ decode_2 rest (off + off_5) ctb_ind
      | {| false            : 1
         ; ctb_ind          : 4 : save_offset_to (off_1)
         ; reserved_1       : 3 : save_offset_to (off_2)
         ; rest             : -1 : save_offset_to (off_3), bitstring
         |} ->
         let parsed = parse_time_base ctb_ind in
         let nodes =
           [ to_node ~offset:off 1 "content_reference_id_record_flag" (Bits (Bool false))
           ; to_node ~parsed ~offset:(off + off_1) 4 "content_time_base_indicator" (Hex (Int ctb_ind))
           ; to_node ~offset:(off + off_2) 3 "reserved" (Bits (Int reserved_1)) ]
         in
         nodes @ decode_2 rest (off + off_3) ctb_ind

    let decode bs off =
      match%bitstring bs with

      | {| 0xFFFF : 16
         ; mafid  : 32 : save_offset_to (off_1)
         ; rest   : -1 : save_offset_to (off_2), bitstring
         |} ->
         let parsed = parse_metadata_app 0xFFFF in
         [ to_node ~offset:off 16 "metadata_application_format" (Hex (Int 0xFFFF))
         ; to_node ~parsed ~offset:(off + off_1) 32
             "metadata_application_format_identifier" (Hex (Int32 mafid))
         ] @ decode_1 rest (off + off_2)
      | {| maf    : 16
         ; rest   : -1 : save_offset_to (off_1), bitstring
         |} ->
          let parsed = parse_metadata_app maf in
         [ to_node ~parsed ~offset:off 16 "metadata_application_format" (Hex (Int maf))
         ] @ decode_1 rest (off + off_1)
  end

  (* 0x25 *)
  module Metadata_pointer = struct
    (* TODO ITU-T Rec. H.222.0 page 93 *)

    let name = "metadata_pointer_descriptor"

    let decode _ _ = []

  end


  (* 0x26 *)
  module Metadata = struct
    (* TODO ITU-T Rec. H.222.0 page 95 *)
    let name = "metadata_descriptor"

    let decode _ _ = []

  end

  (* 0x27 *)
  module Metadata_STD = struct

    let name = "metadata_STD_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| reserved_1   : 2
         ; metadata_in  : 22 : save_offset_to (off_1)
         ; reserved_2   : 2  : save_offset_to (off_2)
         ; metadata_buf : 22 : save_offset_to (off_3)
         ; reserved_3   : 2  : save_offset_to (off_4)
         ; metadata_out : 22 : save_offset_to (off_5)
         |} ->
         [ to_node ~offset:off 2 "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_1) 22 "metadata_input_leak_rate" (Dec (Int metadata_in))
         ; to_node ~offset:(off + off_2) 2  "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off + off_3) 22 "metadata_buffer_size" (Dec (Int metadata_buf))
         ; to_node ~offset:(off + off_4) 2  "reserved" (Bits (Int reserved_3))
         ; to_node ~offset:(off + off_5) 22 "metadata_output_leak_rate" (Dec (Int metadata_out)) ]

  end


  (* 0x28 *)
  module AVC_video = struct

    let name = "AVC video descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| profile_idc          : 8
         ; constraint_set0_flag : 1 : save_offset_to (off_1)
         ; constraint_set1_flag : 1 : save_offset_to (off_2)
         ; constraint_set2_flag : 1 : save_offset_to (off_3)
         ; constraint_set3_flag : 1 : save_offset_to (off_4)
         ; constraint_set4_flag : 1 : save_offset_to (off_5)
         ; constraint_set5_flag : 1 : save_offset_to (off_6)
         ; avc_compatible_flags : 2 : save_offset_to (off_7)
         ; level_idc            : 8 : save_offset_to (off_8)
         ; avc_still_present    : 1 : save_offset_to (off_9)
         ; avc_24_hr_pict_flag  : 1 : save_offset_to (off_10)
         ; fr_flag              : 1 : save_offset_to (off_11)
         ; reserved             : 5 : save_offset_to (off_12)
         |} ->
         [ to_node ~offset:off 8 "profile_idc" (Hex (Int profile_idc))
         ; to_node ~offset:(off + off_1) 1 "constraint_set0_flag" (Bits (Bool constraint_set0_flag))
         ; to_node ~offset:(off + off_2) 1 "constraint_set1_flag" (Bits (Bool constraint_set1_flag))
         ; to_node ~offset:(off + off_3) 1 "constraint_set2_flag" (Bits (Bool constraint_set2_flag))
         ; to_node ~offset:(off + off_4) 1 "constraint_set3_flag" (Bits (Bool constraint_set3_flag))
         ; to_node ~offset:(off + off_5) 1 "constraint_set4_flag" (Bits (Bool constraint_set4_flag))
         ; to_node ~offset:(off + off_6) 1 "constraint_set5_flag" (Bits (Bool constraint_set5_flag))
         ; to_node ~offset:(off + off_7) 2 "AVC_compatible_flags" (Bits (Int avc_compatible_flags))
         ; to_node ~offset:(off + off_8) 8 "level_idc" (Hex (Int level_idc))
         ; to_node ~offset:(off + off_9) 1 "AVC_still_present" (Bits (Bool avc_still_present))
         ; to_node ~offset:(off + off_10) 1 "AVC_24_hour_picture_flag" (Bits (Bool avc_24_hr_pict_flag))
         ; to_node ~offset:(off + off_11) 1 "Frame_Packing_SEI_not_present_flag" (Bits (Bool fr_flag))
         ; to_node ~offset:(off + off_12) 5 "reserved" (Bits (Int reserved)) ]

  end

  (* 0x29 *)
  module IPMP = struct

    let name = "IPMP_descriptor"

    (* TODO refers to ISO/IEC 13818-11, which is not accessible *)
    let decode _ _ = []

  end

  (* 0x2A *)
  module AVC_HRD = struct

    let name = "AVC timing and HRD descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| hrd_mng_valid_flag   : 1
         ; reserved_1           : 6  : save_offset_to (off_1)
         ; true                 : 1  : save_offset_to (off_2)
         ; false                : 1  : save_offset_to (off_3)
         ; reserved_2           : 7  : save_offset_to (off_4)
         ; n                    : 32 : save_offset_to (off_5)
         ; k                    : 32 : save_offset_to (off_6)
         ; num_units_in_tick    : 32 : save_offset_to (off_7)
         ; fixed_fr_rate_flag   : 1  : save_offset_to (off_8)
         ; temporal_poc_flag    : 1  : save_offset_to (off_9)
         ; pict_to_display_flag : 1  : save_offset_to (off_10)
         ; reserved_3           : 5  : save_offset_to (off_11)
         |} ->
         [ to_node ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_mng_valid_flag))
         ; to_node ~offset:(off + off_1)  6  "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_2)  1  "picture_and_timing_info_present" (Bits (Bool true))
         ; to_node ~offset:(off + off_3)  1  "90kHz_flag" (Bits (Bool false))
         ; to_node ~offset:(off + off_4)  7  "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off + off_5)  32 "N" (Dec (Int32 n))
         ; to_node ~offset:(off + off_6)  32 "K" (Dec (Int32 k))
         ; to_node ~offset:(off + off_7)  32 "num_units_in_tick" (Bits (Int32 num_units_in_tick))
         ; to_node ~offset:(off + off_8)  1  "fixed_frame_rate_flag" (Bits (Bool fixed_fr_rate_flag))
         ; to_node ~offset:(off + off_9)  1  "temporal_poc_flag" (Bits (Bool temporal_poc_flag))
         ; to_node ~offset:(off + off_10) 1  "picture_to_display_conversion_flag"
             (Bits (Bool pict_to_display_flag))
         ; to_node ~offset:(off + off_11) 5  "reserved" (Bits (Int reserved_3)) ]
      | {| hrd_mng_valid_flag   : 1
         ; reserved_1           : 6  : save_offset_to (off_1)
         ; true                 : 1  : save_offset_to (off_2)
         ; true                 : 1  : save_offset_to (off_3)
         ; reserved_2           : 7  : save_offset_to (off_4)
         ; num_units_in_tick    : 32 : save_offset_to (off_5)
         ; fixed_fr_rate_flag   : 1  : save_offset_to (off_6)
         ; temporal_poc_flag    : 1  : save_offset_to (off_7)
         ; pict_to_display_flag : 1  : save_offset_to (off_8)
         ; reserved_3           : 5  : save_offset_to (off_9)
         |} ->
         [ to_node ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_mng_valid_flag))
         ; to_node ~offset:(off + off_1)  6  "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_2)  1  "picture_and_timing_info_present" (Bits (Bool true))
         ; to_node ~offset:(off + off_3)  1  "90kHz_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_4)  7  "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off + off_5)  32 "num_units_in_tick" (Bits (Int32 num_units_in_tick))
         ; to_node ~offset:(off + off_6)  1  "fixed_frame_rate_flag" (Bits (Bool fixed_fr_rate_flag))
         ; to_node ~offset:(off + off_7)  1  "temporal_poc_flag" (Bits (Bool temporal_poc_flag))
         ; to_node ~offset:(off + off_8)  1  "picture_to_display_conversion_flag"
             (Bits (Bool pict_to_display_flag))
         ; to_node ~offset:(off + off_9)  5  "reserved" (Bits (Int reserved_3)) ]
      | {| hrd_mng_valid_flag   : 1
         ; reserved_1           : 6 : save_offset_to (off_1)
         ; false                : 1 : save_offset_to (off_2)
         ; fixed_fr_rate_flag   : 1 : save_offset_to (off_3)
         ; temporal_poc_flag    : 1 : save_offset_to (off_4)
         ; pict_to_display_flag : 1 : save_offset_to (off_5)
         ; reserved_2           : 5 : save_offset_to (off_6)
         |} ->
         [ to_node ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_mng_valid_flag))
         ; to_node ~offset:(off + off_1)  6  "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_2)  1  "picture_and_timing_info_present" (Bits (Bool false))
         ; to_node ~offset:(off + off_3)  1  "fixed_frame_rate_flag" (Bits (Bool fixed_fr_rate_flag))
         ; to_node ~offset:(off + off_4)  1  "temporal_poc_flag" (Bits (Bool temporal_poc_flag))
         ; to_node ~offset:(off + off_5)  1  "picture_to_display_conversion_flag"
             (Bits (Bool pict_to_display_flag))
         ; to_node ~offset:(off + off_6)  5  "reserved" (Bits (Int reserved_2)) ]

  end

  (* 0x2B *)
  module MPEG2_AAC_audio = struct

    let name = "MPEG-2_AAC_audio_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| profile        : 8
         ; channel_config : 8 : save_offset_to (off_1)
         ; add_info       : 8 : save_offset_to (off_2)
         |} ->
         [ to_node ~offset:off 8 "MPEG-2_AAC_profile" (Hex (Int profile))
         ; to_node ~offset:(off + off_1) 8 "MPEG-2_AAC_channel_configuration" (Hex (Int channel_config))
         ; to_node ~offset:(off + off_2) 8 "MPEG-2_AAC_additional_information" (Hex (Int add_info)) ]

  end

  (* 0x2C *)
  module Flex_mux_timing = struct

    let name = "FlexMuxTiming descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| fcr_es_id       : 16
         ; fcr_resolution  : 32 : save_offset_to (off_1)
         ; fcr_length      : 8  : save_offset_to (off_2)
         ; fmx_rate_length : 8  : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 16 "FCR_ES_ID" (Hex (Int fcr_es_id))
         ; to_node ~offset:(off + off_1) 32 "FCRResolution" (Hex (Int32 fcr_resolution))
         ; to_node ~offset:(off + off_2) 8  "FCRLength" (Dec (Int fcr_length))
         ; to_node ~offset:(off + off_3) 8  "FmxRateLength" (Dec (Int fmx_rate_length)) ]

  end

  (* 0x2D *)
  module MPEG4_text = struct

    let name = "MPEG-4_text_descriptor"
    (* FIXME *)
    (* defined in ITU-T Rec.H.220.0 *)
    (* and refers to ISO/IEC 14496-17 TextConfig 5.3 page 3 *)
    (* and to TS 26.245 page 12 then*)

    let parse_base_format = function
      | 0x10 -> "Timed Text as specified in 3GPP TS 26.245"
      | x when x >= 0x00 && x <= 0x0F || x >= 0x11 && x <= 0xFF -> "Reserved"
      | _ -> assert false

    let parse_profile_level = function
      | 0x10 -> "Base profile, base level"
      | x when x >= 0x00 && x <= 0x0F || x >= 0x11 && x <= 0xFF -> "Reserved"
      | _ -> assert false

    let parse_descr_flags = function
      | 0x00 -> "Forbidden"
      | 0x01 -> "No in-band, out-of band only"
      | 0x10 -> "In-band onlym no out-of band"
      | 0x11 -> "Both in-band and out-of band"
      | _    -> assert false

    let parse_text_format = function
      | 0x00 | 0xFF -> "Reserved"
      | 0x01 -> "Timed Text as specified in 3GPP TS 26.245"
      | x when x >= 0x02 && x <= 0xEF -> "Reserved"
      | x when x >= 0xF0 && x <= 0xFE -> "User-private"
      | _ -> assert false

(*    let decode_specific_text_config bs off =
      let len = Bitstring.length bs - 184 in
      match%bitstring bs with
      | {| base_format    : 8
         ; profile_level  : 8   : save_offset_to (off_1)
         ; duration_clock : 24  : save_offset_to (off_2)
         ; true           : 1   : save_offset_to (off_3)
         ; descr_flags    : 2   : save_offset_to (off_4)
         ; true           : 1   : save_offset_to (off_5)
         ; true           : 1   : save_offset_to (off_6)
         ; reserved       : 3   : save_offset_to (off_7)
         ; layer          : 8   : save_offset_to (off_8)
         ; text_track_w   : 16  : save_offset_to (off_9)
         ; text_track_h   : 16  : save_offset_to (off_10)
         ; num_of_formats : 8   : save_offset_to (off_11)
         ; comp_format    : 8   : save_offset_to (off_12)
         ; num_sam_descr  : 8   : save_offset_to (off_13)
         ; sample_index   : 8   : save_offset_to (off_14)
         ; sample_descr   : len : save_offset_to (off_15) idk how to parse dis line
         ; scene_width    : 16  : save_offset_to (off_16)
         ; scene_height   : 16  : save_offset_to (off_17)
         ; hor_scene_off  : 16  : save_offset_to (off_18)
         ; ver_scene_off  : 16  : save_offset_to (off_19)
         |} ->
         []*)

    let decode bs off =
      match%bitstring bs with
      | {| text_format : 8
         ; length      : 16 : save_offset_to (off_1)
         ; _    : length * 8 : save_offset_to (off_2), bitstring
         |} ->
      (*   let specific = decode_specific_text_config specific (off + off_2) in*)
         [ to_node ~offset:off 8 "text_format" (String (parse_text_format text_format))
         ; to_node ~offset:(off + off_1) 16 "text_config_length" (Dec (Uint length))
         ; to_node ~offset:(off + off_2) (length * 8) "format_specific_text_config" (List []) ]

  end

  (* 0x2E *)
  module MPEG4_audio_ext = struct

    let name = "MPEG-4_audio_extension_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| true          : 1
         ; reserved      : 3  : save_offset_to (off_1)
         ; num_of_loops  : 4  : save_offset_to (off_2)
         ; audio_profile : num_of_loops * 8 : save_offset_to (off_3), bitstring
         ; asc_size      : 8  : save_offset_to (off_4)
         ; _             : -1 : bitstring
         |} ->
         (* TODO the rest here must be decoded considering ISO 1496-3 page 38 (AudioSpecificConfig), but its kinda messy *)
         let audio_profile =
           parse_bytes ~offset:(off + off_3) audio_profile "audioProfileLevelIndication"
         in
         [ to_node ~offset:off 1 "ASC_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_1) 3 "reserved" (Bits (Int reserved))
         ; to_node ~offset:(off + off_2) 4 "num_of_loops" (Dec (Int num_of_loops))
         ; to_node ~offset:(off + off_3) (num_of_loops * 8) "" (List audio_profile)
         ; to_node ~offset:(off + off_4) 8 "ASC_size" (Dec (Int asc_size))]

      | {| false         : 1
         ; reserved      : 3 : save_offset_to (off_1)
         ; num_of_loops  : 4 : save_offset_to (off_2)
         ; audio_profile : num_of_loops * 8 : save_offset_to (off_3), bitstring
         |} ->
         let audio_profile =
           parse_bytes ~offset:(off + off_3) audio_profile "audioProfileLevelIndication"
         in
         [ to_node ~offset:off 1 "ASC_flag" (Bits (Bool false))
         ; to_node ~offset:(off + off_1) 3 "reserved" (Bits (Int reserved))
         ; to_node ~offset:(off + off_2) 4 "num_of_loops" (Dec (Int num_of_loops))
         ; to_node ~offset:(off + off_3) (num_of_loops * 8) "audio_profile" (List audio_profile)]

  end

  (* 0x2F *)
  module Aux_video_stream = struct

    let name = "Auxiliary_video_stream_descriptor"

    (* TODO refers to ISO/IEC 23002-3, not accessible *)

    let decode _ _ = []

  end

  (* 0x30 *)
  module SVC_ext = struct

    let name = "SVC extension descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| width             : 16
         ; height            : 16 : save_offset_to (off_1)
         ; frame_rate        : 16 : save_offset_to (off_2)
         ; average_bitrate   : 16 : save_offset_to (off_3)
         ; maximum_bitrate   : 16 : save_offset_to (off_4)
         ; dependency_id     : 3  : save_offset_to (off_5)
         ; reserved_1        : 5  : save_offset_to (off_6)
         ; quality_id_start  : 4  : save_offset_to (off_7)
         ; quality_id_end    : 4  : save_offset_to (off_8)
         ; temporal_id_start : 3  : save_offset_to (off_9)
         ; temporal_id_end   : 3  : save_offset_to (off_10)
         ; nsnu_present      : 1  : save_offset_to (off_11)
         ; reserved_2        : 1  : save_offset_to (off_12)
         |} ->
         [ to_node ~offset:off 16 "width" (Dec (Int width))
         ; to_node ~offset:(off + off_1)  16 "height" (Dec (Int height))
         ; to_node ~offset:(off + off_2)  16 "frame_rate" (Dec (Int frame_rate))
         ; to_node ~offset:(off + off_3)  16 "average_bitrate" (Dec (Int average_bitrate))
         ; to_node ~offset:(off + off_4)  16 "maximum_bitrate" (Dec (Int maximum_bitrate))
         ; to_node ~offset:(off + off_5)  3  "dependency_id" (Hex (Int dependency_id))
         ; to_node ~offset:(off + off_6)  5  "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_7)  4  "quality_id_start" (Hex (Int quality_id_start))
         ; to_node ~offset:(off + off_8)  4  "quality_id_end" (Hex (Int quality_id_end))
         ; to_node ~offset:(off + off_9)  3  "temporal_id_start" (Hex (Int temporal_id_start))
         ; to_node ~offset:(off + off_10) 3  "temportal_id_end" (Hex (Int temporal_id_end))
         ; to_node ~offset:(off + off_11) 1  "no_sei_nal_unit_present" (Bits (Bool nsnu_present))
         ; to_node ~offset:(off + off_12) 1  "reserved" (Bits (Bool reserved_2)) ]

  end

  (* 0x31 *)
  module MVC_ext = struct

    let name = "MVC extension descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| average_bit_rate  : 16
         ; maximum_bitrate   : 16 : save_offset_to (off_1)
         ; assoc_not_pr      : 1  : save_offset_to (off_2)
         ; is_left_eyeview   : 1  : save_offset_to (off_3)
         ; reserved          : 2  : save_offset_to (off_4)
         ; order_index_min   : 10 : save_offset_to (off_5)
         ; order_index_max   : 10 : save_offset_to (off_6)
         ; temporal_id_start : 3  : save_offset_to (off_7)
         ; temporal_id_end   : 3  : save_offset_to (off_8)
         ; nsnu_present      : 1  : save_offset_to (off_9)
         ; npnu_present      : 1  : save_offset_to (off_10)
         |} ->
         [ to_node ~offset:off 16 "average_bit_rate" (Dec (Int average_bit_rate))
         ; to_node ~offset:(off + off_1)  16 "maximum_bitrate" (Dec (Int maximum_bitrate))
         ; to_node ~offset:(off + off_2)  1  "view_association_not_present" (Bits (Bool assoc_not_pr))
         ; to_node ~offset:(off + off_3)  1  "base_view_is_left_eyeview" (Bits (Bool is_left_eyeview))
         ; to_node ~offset:(off + off_4)  2  "reserved" (Bits (Int reserved))
         ; to_node ~offset:(off + off_5)  10 "view_order_index_min" (Dec (Int order_index_min))
         ; to_node ~offset:(off + off_6)  10 "view_order_index_max" (Dec (Int order_index_max))
         ; to_node ~offset:(off + off_7)  3  "temporal_id_start" (Hex (Int temporal_id_start))
         ; to_node ~offset:(off + off_8)  3  "temporal_id_end" (Hex (Int temporal_id_end))
         ; to_node ~offset:(off + off_9)  1  "no_sei_nal_unit_present" (Bits (Bool nsnu_present))
         ; to_node ~offset:(off + off_10) 1  "no_prefix_nal_unit_present" (Bits (Bool npnu_present)) ]

  end

  (* 0x32 *)
  module J2K_video = struct

    let name = "J2K video descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| profile_and_level   : 16
         ; horizontal_size     : 32 : save_offset_to (off_1)
         ; vertical_size       : 32 : save_offset_to (off_2)
         ; max_bit_rate        : 32 : save_offset_to (off_3)
         ; max_buffer_size     : 32 : save_offset_to (off_4)
         ; den_frame_rate      : 16 : save_offset_to (off_5)
         ; num_frame_rate      : 16 : save_offset_to (off_6)
         ; color_specification : 8  : save_offset_to (off_7)
         ; still_mode          : 1  : save_offset_to (off_8)
         ; interlaced_video    : 1  : save_offset_to (off_9)
         ; reserved            : 6  : save_offset_to (off_10)
         ; rest                : -1 : save_offset_to (off_11), bitstring
         |} ->
         let nodes =
         [ to_node ~offset:off 16 "profile_and_level" (Hex (Int profile_and_level))
         ; to_node ~offset:(off + off_1)  32 "horizontal_size" (Dec (Int32 horizontal_size))
         ; to_node ~offset:(off + off_2)  32 "vertical_size" (Dec (Int32 vertical_size))
         ; to_node ~offset:(off + off_3)  32 "max_bit_rate" (Dec (Int32 max_bit_rate))
         ; to_node ~offset:(off + off_4)  32 "max_buffer_size" (Dec (Int32 max_buffer_size))
         ; to_node ~offset:(off + off_5)  16 "den_frame_rate" (Dec (Int den_frame_rate))
         ; to_node ~offset:(off + off_6)  16 "num_frame_rate" (Dec (Int num_frame_rate))
         ; to_node ~offset:(off + off_7)  8  "color_specification" (Hex (Int color_specification))
         ; to_node ~offset:(off + off_8)  1  "still_mode" (Bits (Bool still_mode))
         ; to_node ~offset:(off + off_9)  1  "interlaced_video" (Bits (Bool interlaced_video))
         ; to_node ~offset:(off + off_10) 6  "reserved" (Bits (Int reserved)) ]
         in
         nodes @ parse_bytes ~offset:(off + off_11) rest "private_data_byte"

  end

  (* 0x33 *)
  module MVC_operation_point = struct

    let name = "MVC operation point descriptor"

    let rec parse_recommendations off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| level_idc    : 8
              ; points_count : 8  : save_offset_to (off_1)
              ; rest         : -1 : save_offset_to (off_2), bitstring
              |} ->
              let nodes =
                [ to_node ~offset:off 8 "level_idc" (Hex (Int level_idc))
                ; to_node ~offset:(off + off_1) 8 "operation_points_count" (Hex (Int points_count)) ]
              in
              let level_name = Printf.sprintf "Level %s" (string_of_int level_idc) in
              let node = to_node ~offset:off 16 level_name (List nodes) in
              node :: parse_recommendations (off + off_2) rest

    (* FIXME *)

    let decode bs off =
      match%bitstring bs with
      | {| profile_idc          : 8
         ; constraint_set0_flag : 1  : save_offset_to (off_1)
         ; constraint_set1_flag : 1  : save_offset_to (off_2)
         ; constraint_set2_flag : 1  : save_offset_to (off_3)
         ; constraint_set3_flag : 1  : save_offset_to (off_4)
         ; constraint_set4_flag : 1  : save_offset_to (off_5)
         ; constraint_set5_flag : 1  : save_offset_to (off_6)
         ; avc_compatible_flags : 2  : save_offset_to (off_7)
         ; level_count          : 8  : save_offset_to (off_8)
         ; rest                 : -1 : save_offset_to (off_9), bitstring
         |} ->
         let nodes =
         [ to_node ~offset:off 8 "profile_and_level" (Hex (Int profile_idc))
         ; to_node ~offset:(off + off_1) 1 "constraint_set0_flag" (Bits (Bool constraint_set0_flag))
         ; to_node ~offset:(off + off_2) 1 "constraint_set1_flag" (Bits (Bool constraint_set1_flag))
         ; to_node ~offset:(off + off_3) 1 "constraint_set2_flag" (Bits (Bool constraint_set2_flag))
         ; to_node ~offset:(off + off_4) 1 "constraint_set3_flag" (Bits (Bool constraint_set3_flag))
         ; to_node ~offset:(off + off_5) 1 "constraint_set4_flag" (Bits (Bool constraint_set4_flag))
         ; to_node ~offset:(off + off_6) 1 "constraint_set5_flag" (Bits (Bool constraint_set5_flag))
         ; to_node ~offset:(off + off_7) 2 "AVC_compatible_flags" (Bits (Int avc_compatible_flags))
         ; to_node ~offset:(off + off_8) 8 "level_count" (Hex (Int level_count)) ]
         in
         nodes @ parse_recommendations (off + off_9) rest

  end

  (* 0x34 *)
  module MPEG2_stereo_format = struct

    let name = "MPEG2_stereoscopic_video_format_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| true     : 1
         ; arr_type : 7 : save_offset_to (off_1)
         |} ->
         [ to_node ~offset:off 1 "stereo_video_arrangement_type" (Bits (Bool true))
         ; to_node ~offset:(off + off_1) 7 "arrangement_type" (Hex (Int arr_type)) ]
      | {| false    : 1
         ; reserved : 7 : save_offset_to (off_1)
         |} ->
         [ to_node ~offset:off 1 "stereo_video_arrangement_type" (Bits (Bool false))
         ; to_node ~offset:(off + off_1) 7 "reserved" (Hex (Int reserved)) ]

  end

  (* 0x35 *)
  module Stereo_program_info = struct

    let name = "Stereoscopic_program_info_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| reserved     : 5
         ; service_type : 3 : save_offset_to (off_1)
         |} ->
         [ to_node ~offset:off 5 "reserved" (Bits (Int reserved))
         ; to_node ~offset:(off + off_1) 3 "stereoscopic_service_type" (Bits (Int service_type)) ]

  end

  (* 0x36 *)
  module Stereo_video_info = struct

    let name = "Stereoscopic_video_info_descriptor"

    let parse_factor factor =
      match factor with
      | 0 -> "Forbidden"
      | 1 -> "unspecified"
      | 2 -> "Coded resolution is same as coded resolution of base view"
      | 3 -> "Coded resolution is 3/4 coded resolution of base view"
      | 4 -> "Coded resolution is 2/3 coded resolution of base view"
      | 5 -> "Coded resolution is 1/2 coded resolution of base view"
      | 6 | 7 | 8 -> "reserved"
      | x -> Printf.sprintf "user private %d" x

    let decode bs off =
      match%bitstring bs with
      | {| reserved_1    : 7
         ; true          : 1 : save_offset_to (off_1)
         ; reserved_2    : 7 : save_offset_to (off_2)
         ; leftview_flag : 1 : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 7 "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_1) 1 "base_video_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_2) 7 "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off + off_3) 1 "leftview_flag" (Bits (Bool leftview_flag)) ]
      | {| reserved_1   : 7
         ; false        : 1 : save_offset_to (off_1)
         ; reserved_2   : 7 : save_offset_to (off_2)
         ; usable_as_2d : 1 : save_offset_to (off_3)
         ; hu_f         : 4 : save_offset_to (off_4)
         ; vu_f         : 4 : save_offset_to (off_5)
         |} ->
         let vu = parse_factor vu_f in
         let hu = parse_factor hu_f in
         [ to_node ~offset:off 7 "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_1) 1 "base_video_flag" (Bits (Bool false))
         ; to_node ~offset:(off + off_2) 7 "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off + off_3) 1 "usable_as_2D" (Bits (Bool usable_as_2d))
         ; to_node ~parsed:hu ~offset:(off + off_4) 4 "horizontal_upsampling_factor" (Bits (Int hu_f))
         ; to_node ~parsed:vu ~offset:(off + off_5) 4 "vertical_upsampling_factor" (Bits (Int vu_f))
         ]

  end

  (* 0x37 *)
  module Transport_profile = struct

    let name = "Transport_profile_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| transport_profile : 8
         ; rest              : -1 : save_offset_to (off_1), bitstring
         |} ->
         let node = to_node ~offset:off 8 "transport_profile" (Hex (Int transport_profile)) in
         node :: parse_bytes ~offset:(off + off_1) rest "private_data"

  end

  (* 0x38 *)
  module HEVC_video = struct

    let name = "HEVC video descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| profile_space        : 2
         ; tier_flag            : 1  : save_offset_to (off_1)
         ; profile_idc          : 5  : save_offset_to (off_2)
         ; pr_com_ind           : 32 : save_offset_to (off_3)
         ; prog_src_flag        : 1  : save_offset_to (off_4)
         ; interlaced_src_flag  : 1  : save_offset_to (off_5)
         ; np_const_flag        : 1  : save_offset_to (off_6)
         ; fo_const_flag        : 1  : save_offset_to (off_7)
         ; res_zero_44bits      : 44 : save_offset_to (off_8)
         ; level_idc            : 8  : save_offset_to (off_9)
         ; true                 : 1  : save_offset_to (off_10)
         ; hevc_still_flag      : 1  : save_offset_to (off_11)
         ; hevc_24_flag         : 1  : save_offset_to (off_12)
         ; sph_flag             : 1  : save_offset_to (off_13)
         ; reserved_1           : 4  : save_offset_to (off_14)
         ; temp_id_min          : 3  : save_offset_to (off_15)
         ; reserved_2           : 5  : save_offset_to (off_16)
         ; temp_id_max          : 3  : save_offset_to (off_17)
         ; reserved_3           : 5  : save_offset_to (off_18)
         |} ->
         [ to_node ~offset:off 2 "profile_space" (Hex (Int profile_space))
         ; to_node ~offset:(off + off_1)  1  "tier_flag" (Bits (Bool tier_flag))
         ; to_node ~offset:(off + off_2)  5  "profile_idc" (Hex (Int profile_idc))
         ; to_node ~offset:(off + off_3)  32 "profile_compatibility_indication" (Hex (Int32 pr_com_ind))
         ; to_node ~offset:(off + off_4)  1  "progressive_source_flag" (Bits (Bool prog_src_flag))
         ; to_node ~offset:(off + off_5)  1  "interlaced_source_flag" (Bits (Bool interlaced_src_flag))
         ; to_node ~offset:(off + off_6)  1  "non_packed_constraint_flag" (Bits (Bool np_const_flag))
         ; to_node ~offset:(off + off_7)  1  "frame_only_constraint_flag" (Bits (Bool fo_const_flag))
         ; to_node ~offset:(off + off_8)  44 "reserved_zero_44bits" (Bits (Int64 res_zero_44bits))
         ; to_node ~offset:(off + off_9)  8  "level_idc" (Hex (Int level_idc))
         ; to_node ~offset:(off + off_10) 1  "temporal_layer_subset_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_11) 1  "HEVC_still_present_flag" (Bits (Bool hevc_still_flag))
         ; to_node ~offset:(off + off_12) 1  "HEVC_24hr_picture_present_flag" (Bits (Bool hevc_24_flag))
         ; to_node ~offset:(off + off_13) 1 "sub_pic_hrd_params_not_present_flag" (Bits (Bool sph_flag))
         ; to_node ~offset:(off + off_14) 4  "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_15) 3  "temporal_id_min" (Dec (Int temp_id_min))
         ; to_node ~offset:(off + off_16) 5  "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off + off_17) 3  "temporal_id_max" (Dec (Int temp_id_max))
         ; to_node ~offset:(off + off_18) 5  "reserved" (Bits (Int reserved_3)) ]

      | {| profile_space       : 2
         ; tier_flag           : 1  : save_offset_to (off_1)
         ; profile_idc         : 5  : save_offset_to (off_2)
         ; pr_com_ind          : 32 : save_offset_to (off_3)
         ; prog_src_flag       : 1  : save_offset_to (off_4)
         ; interlaced_src_flag : 1  : save_offset_to (off_5)
         ; np_const_flag       : 1  : save_offset_to (off_6)
         ; fo_const_flag       : 1  : save_offset_to (off_7)
         ; res_zero_44bits     : 44 : save_offset_to (off_8)
         ; level_idc           : 8  : save_offset_to (off_9)
         ; false               : 1  : save_offset_to (off_10)
         ; hevc_still_flag     : 1  : save_offset_to (off_11)
         ; hevc_24_flag        : 1  : save_offset_to (off_12)
         ; sph_flag            : 1  : save_offset_to (off_13)
         ; reserved_1          : 4  : save_offset_to (off_14)
         |} ->
         [ to_node ~offset:off 2 "profile_space" (Hex (Int profile_space))
         ; to_node ~offset:(off + off_1)  1  "tier_flag" (Bits (Bool tier_flag))
         ; to_node ~offset:(off + off_2)  5  "profile_idc" (Hex (Int profile_idc))
         ; to_node ~offset:(off + off_3)  32 "profile_compatibility_indication" (Hex (Int32 pr_com_ind))
         ; to_node ~offset:(off + off_4)  1  "progressive_source_flag" (Bits (Bool prog_src_flag))
         ; to_node ~offset:(off + off_5)  1  "interlaced_source_flag" (Bits (Bool interlaced_src_flag))
         ; to_node ~offset:(off + off_6)  1  "non_packed_constraint_flag" (Bits (Bool np_const_flag))
         ; to_node ~offset:(off + off_7)  1  "frame_only_constraint_flag" (Bits (Bool fo_const_flag))
         ; to_node ~offset:(off + off_8)  44 "reserved_zero_44bits" (Bits (Int64 res_zero_44bits))
         ; to_node ~offset:(off + off_9)  8  "level_idc" (Hex (Int level_idc))
         ; to_node ~offset:(off + off_10) 1  "temporal_layer_subset_flag" (Bits (Bool false))
         ; to_node ~offset:(off + off_11) 1  "HEVC_still_present_flag" (Bits (Bool hevc_still_flag))
         ; to_node ~offset:(off + off_12) 1  "HEVC_24hr_picture_present_flag" (Bits (Bool hevc_24_flag))
         ; to_node ~offset:(off + off_13) 1 "sub_pic_hrd_params_not_present_flag" (Bits (Bool sph_flag))
         ; to_node ~offset:(off + off_14) 4  "reserved" (Bits (Int reserved_1)) ]

  end

  (* 0x39-0x3E
   * Rec. ITU-T H.220.0 | ISO/IEC 13818-1 Reserved
   *)

  (* Supply descriptor *)
  module HEVC_HRD = struct

    let name = "HEVC timing and HRD descriptor"

    let decode off bs =
      match%bitstring bs with
      | {| hrd_man_valid_flag : 1
         ; reserved_1         : 6 : save_offset_to (off_1)
         ; false              : 1 : save_offset_to (off_2)
         |} ->
         [ to_node ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_man_valid_flag))
         ; to_node ~offset:(off + off_1) 6 "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_2) 1 "picture_and_timing_info_present_flag" (Bits (Bool false)) ]
      | {| hrd_man_valid_flag : 1
         ; reserved_1         : 6  : save_offset_to (off_1)
         ; true               : 1  : save_offset_to (off_2)
         ; true               : 1  : save_offset_to (off_3)
         ; reserved_2         : 7  : save_offset_to (off_4)
         ; num_units_in_tick  : 32 : save_offset_to (off_5)
         |} ->
         [ to_node ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_man_valid_flag))
         ; to_node ~offset:(off + off_1) 6  "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_2) 1  "picture_and_timing_info_present_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_3) 1  "90kHz_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_4) 7  "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off + off_5) 32 "num_units_in_tick" (Dec (Int32 num_units_in_tick)) ]
      | {| hrd_man_valid_flag : 1
         ; reserved_1         : 6  : save_offset_to (off_1)
         ; true               : 1  : save_offset_to (off_2)
         ; false              : 1  : save_offset_to (off_3)
         ; reserved_2         : 7  : save_offset_to (off_4)
         ; n                  : 32 : save_offset_to (off_5)
         ; k                  : 32 : save_offset_to (off_6)
         ; num_units_in_tick  : 32 : save_offset_to (off_7)
         |} ->
         [ to_node ~offset:off 1 "hrd_management_valid_flag" (Bits (Bool hrd_man_valid_flag))
         ; to_node ~offset:(off + off_1) 6  "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:(off + off_2) 1  "picture_and_timing_info_present_flag" (Bits (Bool true))
         ; to_node ~offset:(off + off_3) 1  "90kHz_flag" (Bits (Bool false))
         ; to_node ~offset:(off + off_4) 7  "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:(off + off_5) 32 "N" (Dec (Int32 n))
         ; to_node ~offset:(off + off_6) 32 "K" (Dec (Int32 k))
         ; to_node ~offset:(off + off_7) 32 "num_units_in_tick" (Dec (Int32 num_units_in_tick)) ]

  end

  (* 0x3F *)
  module Extension_1 = struct

    let name = "Extension_descriptor"

    let parse_tag = function
      | 0 -> "Reserved"
      | 1 -> "Forbidden"
      | 2 -> "ODUpdata_descriptor"
      | 3 -> "HEVC_timing_and_HRD_descriptor"
      | x when x >= 4 && x <= 255 -> "Rec. ITU-T H.222.0 | ISO/IEC 13818-1 Reserved"
      | _ -> assert false

    let decode bs off =
      match%bitstring bs with
      | {| 0x02                     :  8
         ; rest  : -1 : save_offset_to (off_1), bitstring
         |} ->
         (* NOTE refers to 8.5.5.2 of ISO/IEC 14496-1, doesn't exist *)
         let str = Bitstring.string_of_bitstring rest in
         let nodes =
           [ to_node ~parsed:(parse_tag 2) ~offset:off 8 "extension_descriptor_tag" (Hex (Int 0x02))
           ; to_node ~parsed:str ~offset:(off + off_1) (Bitstring.length rest)
               "rest" (Bits (Bool false))]
         in
         nodes @ []
      (* FIXME *)
      | {| 0x03     : 8
         ; rest     : -1 : save_offset_to (off_1), bitstring
         |} ->
         let node =
         [ to_node ~parsed:(parse_tag 3) ~offset:off 8 "extension_descriptor_tag" (Hex (Int 0x03))]
         in
         node @ (HEVC_HRD.decode (off + off_1) rest)
      | {| ext_desc_tag : 8
         ; rest         : -1 : save_offset_to (off_1), bitstring
         |} ->
         let node = to_node ~parsed:(parse_tag ext_desc_tag) ~offset:off
                      8 "extension_descriptor_tag" (Hex (Int ext_desc_tag)) in
         node :: parse_bytes ~offset:(off + off_1) rest "reserved"

  end

  (* 0x40 *)
  module Network_name = struct

    let name = "network_name_descriptor"

    let decode bs off =
      let s = match Text_decoder.decode @@ Bitstring.to_cstruct bs with
        | Ok s    -> s
        | Error _ -> "Unable to decode"
      in
      [ to_node ~offset:off (Bitstring.length bs) "network name" (String s) ]

  end

  (* 0x41 *)
  module Service_list = struct

    let name = "service_list_descriptor"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| service_id   : 16
              ; service_type : 8  : save_offset_to (off_1)
              ; rest         : -1 : save_offset_to (off_2), bitstring
              |} ->
              let parsed = Common.Mpeg_ts.service_type_to_string service_type in
              let nodes =
                [ to_node ~offset:off 16 "service_id" (Hex (Int service_id))
                ; to_node ~parsed ~offset:(off + off_1) 8 "service_type" (Hex (Int service_type)) ]
              in
              let service_name = Printf.sprintf "Service %s" (string_of_int service_id) in
              let node = to_node ~offset:off 24 service_name (List nodes) in
              node :: f (off + off_2) rest

    let decode bs off = f off bs

  end

  (* 0x42*)
  module Stuffing = struct

    let name = "stuffing_descriptor"

    let decode bs off = parse_bytes ~offset:off bs "stuffing_byte"

  end

  (* 0x43 *)
  module Satellite_delivery = struct

    let name = "satellite_delivery_system_descriptor"

    let parse_polarization pol =
      match pol with
      | 0 -> "linear - horizontal"
      | 1 -> "linear - vertical"
      | 2 -> "Circular - left"
      | 3 -> "Circular - right"
      | _ -> assert false

    let parse_rolloff rf =
      match rf with
      | 0 -> "a = 0,35"
      | 1 -> "a = 0,25"
      | 2 -> "a = 0,20"
      | 3 -> "reserved"
      | _ -> assert false

    let parse_mod_system system =
      match system with
      | false -> "DVB-S"
      | true  -> "DVB-S2"

    let parse_mod_type typ =
      match typ with
      | 0 -> "Auto"
      | 1 -> "QPSK"
      | 2 -> "8PSK"
      | 3 -> "16-QAM (n/a for DVB-S2)"
      | _ -> assert false

    let decode bs off =
      match%bitstring bs with
      | {| frequency         : 32
         ; orbital_position  : 16 : save_offset_to (off_1)
         ; west_east_flag    : 1  : save_offset_to (off_2)
         ; polarization      : 2  : save_offset_to (off_3)
         ; roll_off          : 2  : save_offset_to (off_4)
         ; modulation_system : 1  : save_offset_to (off_5)
         ; modulation_type   : 2  : save_offset_to (off_6)
         ; symbol_rate       : 28 : save_offset_to (off_7)
         ; fec_inner         : 4  : save_offset_to (off_8)
         |} ->
         let pol = parse_polarization polarization in
         let rf  = parse_rolloff roll_off in
         let mod_system = parse_mod_system modulation_system in
         let mod_type   = parse_mod_type modulation_type in
         [ to_node ~offset:off 32 "frequency" (Dec (Int32 frequency))
         ; to_node ~offset:(off + off_1) 16 "orbital_position" (Hex (Int orbital_position))
         ; to_node ~offset:(off + off_2) 1  "west_east_flag" (Bits (Bool west_east_flag))
         ; to_node ~parsed:pol ~offset:(off + off_3) 2  "polarization" (Bits (Int polarization))
         ; to_node ~parsed:rf ~offset:(off + off_4) 2  "roll_off" (Bits (Int roll_off))
         ; to_node ~parsed:mod_system ~offset:(off + off_5) 1
             "modulation_system" (Bits (Bool modulation_system))
         ; to_node ~parsed:mod_type ~offset:(off + off_6) 2
             "modulation_type" (Bits (Int modulation_type))
         ; to_node ~offset:(off + off_7) 28 "symbol_rate" (Dec (Int symbol_rate))
         ; to_node ~offset:(off + off_8) 4  "FEC_inner" (Dec (Int fec_inner)) ]

  end

  (* 0x44 *)
  module Cable_delivery = struct

    let name = "cable_delivery_system_descriptor"

    let parse_outer_fec outer =
      match outer with
      | 0 -> "not defined"
      | 1 -> "not outer FEC coding"
      | 2 -> "RS(204/188)"
      | _ -> "reserved for future use"

    let parse_modulation_scheme modulation =
      match modulation with
      | 0x00 -> "not defined"
      | 0x01 -> "16-QAM"
      | 0x02 -> "32-QAM"
      | 0x03 -> "64-QAM"
      | 0x04 -> "128-QAM"
      | 0x05 -> "256-QAM"
      | _    -> "reserved for future use"

    let parse_inner_fec inner =
      match inner with
      | 0  -> "defined"
      | 1  -> "1/2 conv. code rate"
      | 2  -> "2/3 conv. code rate"
      | 3  -> "3/4 conv. code rate"
      | 4  -> "5/6 conv. code rate"
      | 5  -> "7/8 conv. code rate"
      | 6  -> "8/9 conv. code rate"
      | 7  -> "3/5 conv. code rate"
      | 8  -> "4/5 conv. code rate"
      | 9  -> "9/10 conv. code rate"
      | 15 -> "no conv. Coding"
      | _  -> "reserved for future use"

    let decode bs off =
      match%bitstring bs with
      | {| frequency   : 32
         ; rfu         : 12 : save_offset_to (off_1)
         ; fec_outer   : 4  : save_offset_to (off_2)
         ; modulation  : 8  : save_offset_to (off_3)
         ; symbol_rate : 28 : save_offset_to (off_4)
         ; fec_inner   : 4  : save_offset_to (off_5)
         |} ->
         let parsed_mod = parse_modulation_scheme modulation in
         let parsed_out = parse_outer_fec fec_outer in
         let parsed_in  = parse_inner_fec fec_inner in
         [ to_node ~offset:off 32 "frequency" (Dec (Int32 frequency))
         ; to_node ~offset:(off + off_1) 12 "reserved_future_use" (Bits (Int rfu))
         ; to_node ~parsed:parsed_out ~offset:(off + off_2) 4  "FEC_outer" (Dec (Int rfu))
         ; to_node ~parsed:parsed_mod ~offset:(off + off_3) 8  "modulation" (Hex (Int rfu))
         ; to_node ~offset:(off + off_4) 28 "symbol_rate" (Dec (Int symbol_rate))
         ; to_node ~parsed:parsed_in ~offset:(off + off_5) 4  "FEC_inner" (Dec (Int rfu)) ]

  end

  (* 0x45 *)
  module VBI_data = struct

    let name = "VBI_data_descriptor"

    (* TODO EN 300 468 page 86 *)

    let decode _ _ = []

  end

  (* 0x46 *)

  module VBI_teletext = struct

    let name = "VBI_teletext_descriptor"

    let parse_type = function
      | 0x00 -> "reserved for future use"
      | 0x01 -> "initial Teletext page"
      | 0x02 -> "Teletext subtitle page"
      | 0x03 -> "additional information page"
      | 0x04 -> "programme schedule page"
      | 0x05 -> "Teletext subtitle page for hearing impaired people"
      | x when x >= 0x06 && x <= 0x1F -> "reserved_for_future_use"
      | x    -> Printf.sprintf "%d" x

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| lang_code : 24 : bitstring
              ; txt_type  : 5  : save_offset_to (off_1)
              ; mag_num   : 3  : save_offset_to (off_2)
              ; page_num  : 8  : save_offset_to (off_3)
              ; rest      : -1 : save_offset_to (off_4), bitstring
              |} ->
              let parsed_code, lang_code = parse_lang_code lang_code in
              let parsed_typ = parse_type txt_type in
              let nodes =
                [ to_node ~parsed:parsed_code ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
                ; to_node ~parsed:parsed_typ ~offset:(off + off_1) 5 "teletext_type" (Hex(Int txt_type))
                ; to_node ~offset:(off + off_2) 3 "teletext_magazine_number" (Dec (Int mag_num))
                ; to_node ~offset:(off + off_3) 8 "teletext_page_number" (Dec (Int page_num)) ]
              in
              nodes @ f (off + off_4) rest

    let decode bs off = f off bs

  end

  (* 0x47 *)
  module Bouquet_name = struct

    let name = "bouquet_name_descriptor"

    let decode bs off =
      let s = match Text_decoder.decode @@ Bitstring.to_cstruct bs with
        | Ok s    -> s
        | Error _ -> "Unable to decode"
      in
      [ to_node ~offset:off (Bitstring.length bs) "bouquet name" (String s) ]

  end

  (* 0x48 *)
  module Service = struct

    let name = "service_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| service_type     : 8
         ; length_1         : 8 : save_offset_to (off_1)
         ; service_provider : length_1 * 8 : save_offset_to (off_2), bitstring
         ; length_2         : 8 : save_offset_to (off_3)
         ; service          : length_2 * 8 : save_offset_to (off_4), bitstring
         |} ->
         let parsed = Common.Mpeg_ts.service_type_to_string service_type in
         let service = match Text_decoder.decode @@ Bitstring.to_cstruct service with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         let sp = match Text_decoder.decode @@ Bitstring.to_cstruct service_provider with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         [ to_node ~parsed ~offset:off 8 "service_type" (Hex (Int service_type))
         ; to_node ~offset:(off + off_1) 8 "service_provider_name_length" (Dec (Int length_1))
         ; to_node ~offset:(off + off_2) (length_1 * 8) "service_provider" (String sp)
         ; to_node ~offset:(off + off_3) 8 "service_name_length" (Dec (Int length_2))
         ; to_node ~offset:(off + off_4) (length_2 * 8) "service" (String service) ]

  end

  (* 0x49 *)
  module Country_availability = struct

    let name = "country_availability_descriptor"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| country_code : 24 : bitstring
              ; rest         : -1 : save_offset_to (off_1), bitstring
              |} ->
              let parsed, code = parse_lang_code country_code in
              let node = to_node ~parsed ~offset:off 24 "country code" (Bits (Int code)) in
              node :: f (off + off_1) rest

    let decode bs off =
      match%bitstring bs with
      | {| country_avail_flag : 1
         ; rfu                : 7  : save_offset_to (off_1)
         ; rest               : -1 : save_offset_to (off_2), bitstring
         |} ->
         let nodes =
         [ to_node ~offset:off 1 "country_availability_flag" (Bits (Bool country_avail_flag))
         ; to_node ~offset:(off + off_1) 7 "reserved_future_use" (Bits (Int rfu)) ]
         in
         nodes @ f (off + off_2) rest

  end

  (* 0x4A *)
  module Linkage = struct

    open Bitstring

    let parse_linkage_type = function
      | 0x00 | 0xFF -> "reserved for future use"
      | 0x01 -> "information service"
      | 0x02 -> "EPG service"
      | 0x03 -> "CA replacement service"
      | 0x04 -> "TS containing complete Network/Bouquet SI"
      | 0x05 -> "service replacement service"
      | 0x06 -> "data broadcast service"
      | 0x07 -> "RCS Map"
      | 0x08 -> "mobile hand-over"
      | 0x09 -> "System Software Update Service (ERSI TS 102 006 [11])"
      | 0x0A -> "TS containing SSU BAT or NIT (ETSI EN 102 006 [11])"
      | 0x0B -> "IP/MAC Notification Service (ETSI EN 301 192 [4])"
      | 0x0C -> "TS containing INT BAT or NIT (ETSI EN 301 192 [4])"
      | 0x0D -> "event linkage"
      | x when x >= 0x0E && x <= 0x1F -> "extended event linkage"
      | x when x >= 0x20 && x <= 0x7F -> "reserved for future use"
      | x when x >= 0x80 && x <= 0xFE -> "user defined"
      | _    -> assert false

    let parse_handover_type = function
      | 0x00 -> "reserved for future use"
      | 0x01 -> "DVB hand-over to and identical service in a neighbouring country"
      | 0x02 -> "DVB hand-over to an associated service"
      | 0x03 -> "DVB hand-over to an associated service"
      | x when x >= 0x04 && x <= 0x0F -> "reserved for future use"
      | _    -> assert false

    let parse_origin_type = function
      | 0x00 -> "NIT"
      | 0x01 -> "SDT"
      | _    -> assert false

    let parse_link_type ~linkage ~link =
      match linkage with
      | 0x0E -> ( match link with
                  | 0 -> "SD"
                  | 1 -> "HD"
                  | 2 -> "frame compatible plano-stereoscopic H.264/AVC"
                  | 3 -> "service compatible plano-stereoscopic MVC"
                  | _ -> assert false)
      | 0x0F -> ( match link with
                  | 0 -> "UHD"
                  | 1 -> "service frame compatible plano-stereoscopic"
                  | 2 | 3 -> "reserved or future use"
                  | _ -> assert false)
      | x when x >= 0x10 && x <= 0x1F -> ( match link with
                                           | x when x >= 0 && x <= 3 -> "reserved for future use"
                                           | _ -> assert false)
      | _ -> assert false

    let parse_target_id = function
      | 0 -> "use transport_stream_id"
      | 1 -> "use target_transport_stream_id"
      | 2 -> "match any transport_stream_id (wildcard)"
      | 3 -> "use user_defined_id"
      | _ -> assert false

    let decode_handover_linkage bs off =
      match%bitstring bs with
      | {| 0x01        : 4
         ; rfu         : 3  : save_offset_to (off_1)
         ; origin_type : 1  : save_offset_to (off_2)
         ; network_id  : 16 : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 4 "hand-over_type" (Hex (Int 0x01))
         ; to_node ~offset:(off + off_1) 3  "reserved_future_use" (Bits (Int rfu))
         ; to_node ~offset:(off + off_2) 1  "origin_type" (Bits (Bool origin_type))
         ; to_node ~offset:(off + off_3) 16 "network_id" (Dec (Int network_id)) ]
      | {| 0x02        : 4
         ; rfu         : 3  : save_offset_to (off_1)
         ; origin_type : 1  : save_offset_to (off_2)
         ; network_id  : 16 : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 4 "hand-over_type" (Hex (Int 0x02))
         ; to_node ~offset:(off + off_1) 3  "reserved_future_use" (Bits (Int rfu))
         ; to_node ~offset:(off + off_2) 1  "origin_type" (Bits (Bool origin_type))
         ; to_node ~offset:(off + off_3) 16 "network_id" (Dec (Int network_id)) ]
      | {| 0x03        : 4
         ; rfu         : 3  : save_offset_to (off_1)
         ; origin_type : 1  : save_offset_to (off_2)
         ; network_id  : 16 : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 4 "hand-over_type" (Hex (Int 0x03))
         ; to_node ~offset:(off + off_1) 3  "reserved_future_use" (Bits (Int rfu))
         ; to_node ~offset:(off + off_2) 1  "origin_type" (Bits (Bool origin_type))
         ; to_node ~offset:(off + off_3) 16 "network_id" (Dec (Int network_id)) ]
      | {| 0x00            : 4
         ; rfu             : 3  : save_offset_to (off_1)
         ; origin_type     : 1  : save_offset_to (off_2)
         ; init_service_id : 16 : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 4 "hand-over_type" (Hex (Int 0x03))
         ; to_node ~offset:(off + off_1) 3  "reserved_future_use" (Bits (Int rfu))
         ; to_node ~offset:(off + off_2) 1  "origin_type" (Bits (Bool origin_type))
         ; to_node ~offset:(off + off_3) 16 "initial_service_id" (Dec (Int init_service_id)) ]

    let decode_event_linkage bs off =
      match%bitstring bs with
      | {| target_event_id : 16
         ; target_listed   : 1 : save_offset_to (off_1)
         ; event_simulcast : 1 : save_offset_to (off_2)
         ; reserved        : 6 : save_offset_to (off_3)
         |} ->
         [ to_node ~offset:off 16 "target_event_id" (Dec (Int target_event_id))
         ; to_node ~offset:(off + off_1) 1 "target_listed" (Bits (Bool target_listed))
         ; to_node ~offset:(off + off_2) 1 "event_simulcast" (Bits (Bool event_simulcast))
         ; to_node ~offset:(off + off_3) 6 "reserved" (Bits (Int reserved)) ]

    let decode_extended_event_linkage ~linkage bs off =
      let rec f off x =
        if Bitstring.length x = 0 then []
        else match%bitstring x with
             | {| target_event_id : 16
                ; target_listed   : 1  : save_offset_to (off_1)
                ; event_simulcast : 1  : save_offset_to (off_2)
                ; link_type       : 2  : save_offset_to (off_3)
                ; target_id_type  : 2  : save_offset_to (off_4)
                ; on_id_flag      : 1  : save_offset_to (off_5)
                ; service_id_flag : 1  : save_offset_to (off_6)
                ; rest            : -1 : save_offset_to (off_7), bitstring
                |} ->
                let nodes =
                  [ to_node ~offset:off 16 "target_event_id" (Dec (Int target_event_id))
                  ; to_node ~offset:(off + off_1) 1 "target_listed" (Bits (Bool target_listed))
                  ; to_node ~offset:(off + off_2) 1 "event_simulcast" (Bits (Bool event_simulcast))
                  ; to_node ~parsed:(parse_link_type ~linkage ~link:link_type)
                      ~offset:(off + off_3) 2 "link_type" (Hex (Int link_type))
                  ; to_node ~parsed:(parse_target_id target_id_type) ~offset:(off + off_4)
                      2 "target_id_type" (Hex (Int target_id_type))
                  ; to_node ~offset:(off + off_5) 1 "original_network_id_flag" (Bits (Bool on_id_flag))
                  ; to_node ~offset:(off + off_6) 1 "service_id_flag" (Bits (Bool service_id_flag))]
                in
                nodes @ f (off + off_7) rest
      in
      match%bitstring bs with
      | {| loop_length    : 8
         ; extended_event : loop_length * 8 : save_offset_to (off_1), bitstring
         ; rest           : -1 : save_offset_to (off_2), bitstring
         |} ->
         let extended_linkage = f (off + off_1) extended_event in
         let private_data     = parse_bytes ~offset:(off + off_2) rest "private_data_byte" in
         [ to_node ~offset:off 8 "loop_length" (Dec (Int loop_length))
         ; to_node ~offset:(off + off_1) (loop_length * 8) "extended_event_linkage"
             (List extended_linkage)
         ; to_node ~offset:(off + off_1) (length rest) "private_data_bytes" (List private_data) ]

    let name = "linkage_descriptor"
    let decode bs off =
      match%bitstring bs with
      | {| ts_id                : 16
         ; on_id                : 16 : save_offset_to (off_1)
         ; service_id           : 16 : save_offset_to (off_2)
         ; 0x08                 : 8  : save_offset_to (off_3)
         ; mobile_handover_info : 24 : save_offset_to (off_4), bitstring
         ; rest                 : -1 : save_offset_to (off_5), bitstring
         |} ->
         let handover = decode_handover_linkage mobile_handover_info (off + off_4) in
         let bytes = parse_bytes ~offset:(off + off_5) rest "private_data_bytes" in
         [ to_node ~offset:off 16 "transport_stream_id" (Hex (Int ts_id))
         ; to_node ~offset:(off + off_1) 16 "original_network_id" (Hex (Int on_id))
         ; to_node ~offset:(off + off_2) 16 "service_id" (Hex (Int service_id))
         ; to_node ~offset:(off + off_3) 8  "linkage_type" (Dec (Int 0x08))
         ; to_node ~offset:(off + off_4) 24 "mobile_handover_info" (List handover)
         ; to_node ~offset:(off + off_5) (length rest) "private_data_bytes" (List bytes) ]
      | {| ts_id              : 16
         ; on_id              : 16 : save_offset_to (off_1)
         ; service_id         : 16 : save_offset_to (off_2)
         ; 0x0D               : 8  : save_offset_to (off_3)
         ; event_linkage_info : 24 : save_offset_to (off_4), bitstring
         ; rest               : -1 : save_offset_to (off_5), bitstring
         |} ->
         let bytes = parse_bytes ~offset:(off + off_5) rest "private_data_bytes" in
         let event_linkage = decode_event_linkage event_linkage_info (off + off_4) in
         [ to_node ~offset:off 16 "transport_stream_id" (Hex (Int ts_id))
         ; to_node ~offset:(off + off_1) 16 "original_network_id" (Hex (Int on_id))
         ; to_node ~offset:(off + off_2) 16 "service_id" (Hex (Int service_id))
         ; to_node ~offset:(off + off_3) 8  "linkage_type" (Hex (Int 0x0D))
         ; to_node ~offset:(off + off_4) 24 "event_linkage_info" (List event_linkage)
         ; to_node ~offset:(off + off_5) (length rest) "private_data_bytes" (List bytes) ]
      | {| ts_id        : 16
         ; on_id        : 16 : save_offset_to (off_1)
         ; service_id   : 16 : save_offset_to (off_2)
         ; linkage_type : 8  : save_offset_to (off_3)
         ; rest         : -1 : save_offset_to (off_4), bitstring
         |} ->
         let bytes = parse_bytes ~offset:(off + off_4) rest "private_data_bytes" in
         [ to_node ~offset:off 16 "transport_stream_id" (Hex (Int ts_id))
         ; to_node ~offset:(off + off_1) 16 "original_network_id" (Hex (Int on_id))
         ; to_node ~offset:(off + off_2) 16 "service_id" (Hex (Int service_id))
         ; to_node ~parsed:(parse_linkage_type linkage_type)~offset:(off + off_3) 8
             "linkage_type" (Hex (Int linkage_type))
         ; to_node ~offset:(off + off_4) (length rest) "private_data_bytes" (List bytes) ]

  end

  (* 0x4B *)
  module NVOD_reference = struct

    let name = "NVOD_reference_descriptor"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| ts_id : 16
              ; on_id : 16 : save_offset_to (off_1)
              ; sv_id : 16 : save_offset_to (off_2)
              ; rest  : -1 : save_offset_to (off_3), bitstring
              |} ->
              let nodes =
                [ to_node ~offset:off 16 "transport_stream_id" (Hex (Int ts_id))
                ; to_node ~offset:(off + off_1) 16 "original_network_id" (Hex (Int on_id))
                ; to_node ~offset:(off + off_2) 16 "service_id" (Hex (Int sv_id)) ]
              in
              nodes @ f (off + off_3) rest

    let decode bs off = f off bs

  end

  (* 0x4C *)
  module Time_shifted_service = struct

    let name = "time_shifted_service_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| service_id : 16 |} ->
         [ to_node ~offset:off 16 "reference_service_id" (Hex (Int service_id)) ]

  end

  (* 0x4D *)
  module Short_event = struct

    let name = "short_event_descriptor"

    (* TODO  is this supposed to work like it does
     *  let parse_lang_code code =
     *   match%bitstring code with
     *   | {| ch_1 : 8 : string
     *      ; ch_2 : 8 : string
     *      ; ch_3 : 8 : string
     *      |} ->
     *      Printf.sprintf "%s%s%s" ch_1 ch_2 ch_3 *)

    let decode bs off =
      match%bitstring bs with
      | {| lang_code  : 24 : bitstring
         ; length_1   : 8  : save_offset_to (off_1)
         ; event_name : length_1 * 8 : save_offset_to (off_2), bitstring
         ; length_2   : 8  : save_offset_to (off_3)
         ; text       : length_2 * 8 : save_offset_to (off_4), bitstring
         |} ->
         let name = match Text_decoder.decode @@ Bitstring.to_cstruct event_name with
           | Ok s -> s
           | Error _ -> "Failed to decode" in
         let text = match Text_decoder.decode @@ Bitstring.to_cstruct text with
           | Ok s -> s
           | Error _ -> "Failed to decode" in
         let parsed_code, lang_code = parse_lang_code lang_code in
         [ to_node ~parsed:parsed_code ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
         ; to_node ~offset:(off + off_1) 8 "event_name_length" (Dec (Int length_1))
         ; to_node ~offset:(off + off_2) (length_1 * 8) "event_name" (String name)
         ; to_node ~offset:(off + off_3) 8 "text_length" (Dec (Int length_2))
         ; to_node ~offset:(off + off_4) (length_2 * 8) "text" (String text)
         ]

  end

  (* 0x4E *)
  module Extended_event = struct

    let name = "extended_event_descriptor"

    let rec decode_items bs off =
      if Bitstring.length bs = 0 then []
      else match%bitstring bs with
           | {| item_descr_length : 8
              ; item_descr        : item_descr_length * 8 : save_offset_to (off_1), bitstring
              ; item_length       : 8  : save_offset_to (off_2)
              ; item              : item_length * 8 : save_offset_to (off_3), bitstring
              ; rest              : -1 : save_offset_to (off_4), bitstring
              |} ->
              let item_descr = match Text_decoder.decode @@ Bitstring.to_cstruct item_descr with
                | Ok s -> s
                | Error _ -> "Failed to decode" in
              let item = match Text_decoder.decode @@ Bitstring.to_cstruct item with
                | Ok s -> s
                | Error _ -> "Failed to decode" in
              let nodes =
                [ to_node ~offset:off 8 "item_description_length" (Dec (Int item_descr_length))
                ; to_node ~offset:(off + off_1) (item_descr_length * 8) "item_description" (String item_descr)
                ; to_node ~offset:(off + off_2) 8 "item_length" (Dec (Int item_length))
                ; to_node ~offset:(off + off_3) (item_length * 8) "item" (String item) ]
              in
              let real_len = 16 + item_descr_length * 8 + item_length * 8 in
              let node = to_node ~offset:off real_len item (List nodes) in
              node :: decode_items rest (off + off_4)

    let decode bs off =
      match%bitstring bs with
      | {| desc_num      : 4
         ; last_desc_num : 4  : save_offset_to (off_1)
         ; lang_code     : 24 : save_offset_to (off_2), bitstring
         ; items_length  : 8  : save_offset_to (off_3)
         ; items         : items_length * 8 : save_offset_to (off_4), bitstring
         ; text_length   : 8  : save_offset_to (off_5)
         ; text          : text_length * 8 : save_offset_to (off_6), bitstring
         |} ->
         let items = decode_items items (off + off_4) in
         let text = match Text_decoder.decode @@ Bitstring.to_cstruct text with
           | Ok s -> s
           | Error _ -> "Failed to decode" in
         let parsed_code, lang_code = parse_lang_code lang_code in
         [ to_node ~offset:off 4 "descriptor_number" (Dec (Int desc_num))
         ; to_node ~offset:(off + off_1) 4 "last_desc_num" (Dec (Int last_desc_num))
         ; to_node ~parsed:parsed_code ~offset:(off + off_2) 24 "ISO_639_language_code"
             (Bits (Int lang_code))
         ; to_node ~offset:(off + off_3) 8 "items_length" (Dec (Int items_length))
         ; to_node ~offset:(off + off_4) (items_length * 8) "items" (List items)
         ; to_node ~offset:(off + off_5) 8 "text_length" (Dec (Int text_length))
         ; to_node ~offset:(off + off_6) (text_length * 8) "text" (String text) ]

  end

  (* 0x4F *)
  module Time_shifted_event = struct

    let name = "time_shifted_service_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| service_id : 16
         ; event_id   : 16 : save_offset_to (off_1)
         |} ->
         [ to_node ~offset:off 16 "reference_service_id" (Hex (Int service_id))
         ; to_node ~offset:(off + off_1) 16 "reference_event_id" (Hex (Int event_id)) ]

  end

  (* 0x50 *)
  module Component = struct

    let name = "component_descriptor"

    (* page 42 EN 300 468 *)
    let parse ~stream_content ~stream_content_ext ~component_type =
      match stream_content with
      | 0x0 -> (match stream_content_ext with
                | x when x >= 0x0 && x <= 0xF -> (match component_type with
                                                  | x when x >= 0x00 && x <= 0xFF -> "reserved for future use"
                                                  | _ -> assert false)
                | _ -> assert false)
      | 0x1 -> (match component_type with
                | 0x00 -> "reserved for future use"
                | 0x01 -> "MPEG-2 video, 4:3 aspect ratio, 25 Hz"
                | 0x02 -> "MPEG-2 video, 16:9 aspect ratio with pan vectors, 25 Hz"
                | 0x03 -> "MPEG-2 video, 16:9 aspect ratio without pan vectors, 25 Hz"
                | 0x04 -> "MPEG-2 video, > 16:9 aspect ratio, 25 Hz"
                | 0x05 -> "MPEG-2 video, 4:3 aspect ratio, 30 Hz"
                | 0x06 -> "MPEG-2 video, 16:9 aspect ratio with pan"
                | 0x07 -> "MPEG-2 video, 16:9 aspect ratio without pan"
                | 0x08 -> "MPEG-2 video, > 16:9 aspect ratio, 30 Hz"
                | 0x09 -> "MPEG-2 high definition video, 4:3 aspect ratio"
                | 0x0A -> "MPEG-2 high definition video, 16:9 aspect ratio with pan vectors, 25 Hz "
                | 0x0B -> "MPEG-2 high definition video, 16:9 aspect ratio without pan vectors, 25 Hz"
                | 0x0C -> "MPEG-2 high definition video, > 16:9 aspect ratio, 25 Hz"
                | 0x0D -> "MPEG-2 high definition video, 4:3 aspect ratio, 30 Hz"
                | 0x0E -> "MPEG-2 high definition video, 16:9 aspect ratio with pan vectors, 30 Hz"
                | 0x0F -> "MPEG-2 high definition video, 16:9 aspect ratio without pan vectors, 30 Hz"
                | 0x10 -> "MPEG-2 high definition video, > 16:9 aspect ratio, 30 Hz"
                | 0xFF -> "reserved for future use"
                | x when x >= 0x11 && x <= 0xAF -> "reserved for future use"
                | x when x >= 0xB0 && x <= 0xFE -> "user defined"
                | _    -> assert false)
      | 0x2 -> (match component_type with
                | 0x00 -> "reserved for future use"
                | 0x01 -> "MPEG-1 Layer 2 audio, single mono channel"
                | 0x02 -> "MPEG-1 Layer 2 audio, dual mono channel"
                | 0x03 -> "MPEG-1 Layer 2 audio, stereo (2 channel)"
                | 0x04 -> "MPEG-1 Layer 2 audio, multi-lingual, multi-channel"
                | 0x05 -> "MPEG-1 Layer 2 audio, surround sound"
                | 0x40 -> "MPEG-1 Layer 2 audio description for the visually impaired"
                | 0x41 -> "MPEG-1 Layer 2 audio for the hard of hearing"
                | 0x42 -> "receiver-mix supplementary audio as per annex E of ETSI TS 101 154"
                | 0x47 -> "MPEG-1 Layer 2 audio, receiver-mix audio description"
                | 0x48 -> "MPEG-1 Layer 2 audio, broadcast-mix audio description"
                | 0xFF -> "reserved for future use"
                | 0x43 | 0x44 | 0x45 | 0x46 -> "reserved for future use"
                | x when x >= 0x06 && x <= 0x3F -> "reserved for future use"
                | x when x >= 0x49 && x <= 0xAF -> "reserved for future use"
                | x when x >= 0xB0 && x <= 0xFE -> "user-defined"
                | _    -> assert false)
      | 0x3 -> (match component_type with
                | 0x01 -> "EBU Teletext subtitles"
                | 0x02 -> "associated EBU Teletext"
                | 0x03 -> "VBI data"
                | 0x10 -> "DVB subtitles (normal) with no monitor aspect ratio criticality"
                | 0x11 -> "DVB subtitles (normal) for display on 4:3 aspect ratio monitor"
                | 0x12 -> "DVB subtitles (normal) for display on 16:9 aspect ratio monitor"
                | 0x13 -> "DVB subtitles (normal) for display on 2.21:1 aspect ratio monitor"
                | 0x14 -> "DVB subtitles (normal) for display on a high definition monitor"
                | 0x15 -> "DVB subtitles (normal) with plano-stereoscopic \
                           disparity for display on a high definition monitor"
                | 0x20 -> "DVB subtitles (for the hard of hearing) with \
                           no monitor aspect ratio criticality"
                | 0x21 -> "DVB subtitles (for the hard of hearing) for  \
                           display on 4:3 aspect ratio monitor"
                | 0x22 -> "DVB subtitles (for the hard of hearing) for \
                           display on 16:9 aspect ratio monitor"
                | 0x23 -> "DVB subtitles (for the hard of hearing) for \
                           display on 2.21:1 aspect ratio monitor"
                | 0x24 -> "DVB subtitles (for the hard of hearing) for \
                           display on a high definition monitor"
                | 0x25 -> "DVB subtitles (for the hard of hearing) with \
                           plano-stereoscopic disparity for display on a high definition monitor"
                | 0x30 -> "open (in-vision) sign language interpretation for the deaf"
                | 0x31 -> "closed sign language interpretation for the deaf"
                | 0x40 -> "video up-sampled from standard definition source material"
                | 0x80 -> "dependent SAOC-DE data stream"
                | 0xFF -> "reserved for future use"
                | x when x >= 0x04 && x <= 0x0F ||
                           x >= 0x16 && x <= 0x1F ||
                             x >= 0x26 && x <= 0x2F ||
                               x >= 0x32 && x <= 0x3F ||
                                 x >= 0x41 && x <= 0x7F ||
                                   x >= 0x81 && x <= 0xAF ||
                                     x = 0xFF ||
                                       x = 0x00 -> "reserved for future use"
                | x when x >= 0xB0 && x <= 0xFE -> "user defined"
                | _   -> assert false)
      | 0x4 -> (match component_type with
                | x when x >= 0x00 && x <= 0x7F -> "reserved for AC-3 audio modules"
                | x when x >= 0x80 && x <= 0xFF -> "reserved for enhanced AC-3 audio modules"
                | _ -> assert false)
      | 0x5 -> (match component_type with
                | 0x01 -> "H.264/AVC standard definition video, 4:3 aspect ratio, 25 Hz"
                | 0x03 -> "H.264/AVC standard definition video, 16:9 aspect ratio, 25 Hz"
                | 0x04 -> "H.264/AVC standard definition video, > 16:9 aspect ratio, 25 Hz"
                | 0x05 -> "H.264/AVC standard definition video, 4:3 aspect ratio, 30 Hz"
                | 0x07 -> "H.264/AVC standard definition video, 16:9 aspect ratio, 30 Hz"
                | 0x08 -> "H.264/AVC standard definition video, > 16:9 aspect ratio, 30 Hz"
                | 0x0B -> "H.264/AVC high definition video, 16:9 aspect ratio, 25 Hz"
                | 0x0C -> "H.264/AVC high definition video, > 16:9 aspect ratio, 25 Hz"
                | 0x0F -> "H.264/AVC high definition video, 16:9 aspect ratio, 30 Hz"
                | 0x10 -> "H.264/AVC high definition video, > 16:9 aspect ratio, 30 Hz"
                | 0x80 -> "H.264/AVC plano-stereoscopic frame compatible high \
                           definition video, 16:9 aspect ratio, 25 Hz, Side-by-Side"
                | 0x81 -> "H.264/AVC plano-stereoscopic frame compatible high \
                           definition video, 16:9 aspect ratio, 25 Hz, Top-and-Bottom"
                | 0x82 -> "H.264/AVC plano-stereoscopic frame compatible high \
                           definition video, 16:9 aspect ratio, 30 Hz, Side-by-Side"
                | 0x83 -> "H.264/AVC stereoscopic frame compatible high \
                           definition video, 16:9 aspect ratio, 30 Hz, Top-and-Bottom"
                | 0x84 -> "H.264/MVC dependent view, planostereoscopic service compatible video"
                | x when x = 0x00 ||
                           x = 0x02 ||
                             x = 0x06 ||
                               x = 0xFF ||
                                 x >= 0x09 && x <= 0x0A ||
                                   x >= 0x0D && x <= 0x0E ||
                                     x >= 0x11 && x <= 0x7F ||
                                       x >= 0x85 && x <= 0xAF -> "reserved for future use"
                | x when x >= 0xB0 && x <= 0xFE -> "user defined"
                | _    -> assert false)
      | 0x6 -> (match component_type with
                | 0x01 -> "HE AAC audio, single mono channel"
                | 0x03 -> "HE AAC audio, stereo"
                | 0x05 -> "HE AAC audio, surround sound"
                | 0x40 -> "HE AAC audio description for the visually impaired"
                | 0x41 -> "HE AAC audio for the hard of hearing"
                | 0x42 -> "HE AAC receiver-mix supplementary audio as per \
                           annex E of ETSI TS 101 154 [9]"
                | 0x43 -> "HE AAC v2 audio, stereo"
                | 0x44 -> "HE AAC v2 audio description for the visually impaired"
                | 0x45 -> "HE AAC v2 audio for the hard of hearing"
                | 0x46 -> "HE AAC v2 receiver-mix supplementary audio as per \
                           annex E of ETSI TS 101 154 [9]"
                | 0x47 -> "HE AAC receiver-mix audio description for the visually impaired"
                | 0x48 -> "HE AAC broadcast-mix audio description for the visually impaired"
                | 0x49 -> "HE AAC v2 receiver-mix audio description for the visually impaired"
                | 0x4A -> "HE AAC v2 broadcast-mix audio description for the visually impaired"
                | 0xA0 -> "HE AAC, or HE AAC v2 with SAOC-DE ancillary data"
                | x when x = 0xFF ||
                           x = 0x00 ||
                             x = 0x02 ||
                               x = 0x04 ||
                                 x >= 0x06 && x <= 0x3F ||
                                   x >= 0x4B && x <= 0x9F ||
                                     x >= 0xA1 && x <= 0xAF -> "reserved for future use"
                | x when x >= 0xB0 && x <= 0xFE -> "user defined"
                | _   -> assert false)
      | 0x7 -> (match component_type with
                | x when x >= 0x00 && x <= 0x7F -> "reserved for DTS® and DTS-HD® audio modes"
                | x when x >= 0x80 && x <= 0xFF -> "reserved for future use"
                | _ -> assert false)
      | 0x8 -> (match component_type with
                | 0x00 -> "reserved for future use"
                | 0x01 -> "DVB SRM data"
                | x when x >= 0x02 && x <= 0xFF -> "reserved for DVB CPCM modes"
                | _    -> assert false)
      | 0x9 -> (match stream_content_ext with
                | 0x0 -> (match component_type with
                          | 0x00 -> "HEVC Main Profile high definition video, 50 Hz"
                          | 0x01 -> "HEVC Main 10 Profile high definition video, 50 Hz"
                          | 0x02 -> "HEVC Main Profile high definition video, 60 Hz"
                          | 0x03 -> "HEVC Main 10 Profile high definition video, 60 Hz"
                          | 0x04 -> "HEVC ultra high definition video"
                          | x when x >= 0x05 && x <= 0xFF -> "reserved for future use"
                          | _    -> assert false)
                | 0x1 -> (match component_type with
                          | 0x00 -> "AC-4 main audio, mono"
                          | 0x01 -> "AC-4 main audio, mono, dialogue enhancement enabled"
                          | 0x02 -> "AC-4 main audio, stereo"
                          | 0x03 -> "AC-4 main audio, stereo, dialogue enhancement enabled"
                          | 0x04 -> "AC-4 main audio, multichannel"
                          | 0x05 -> "AC-4 main audio, multichannel, dialogue enhancement enabled"
                          | 0x06 -> "AC-4 broadcast-mix audio description, mono, \
                                     for the visually impaired"
                          | 0x07 -> "AC-4 broadcast-mix audio description, mono, \
                                     for the visually impaired, dialogue enhancement enabled"
                          | 0x08 -> "AC-4 broadcast-mix audio description, stereo, \
                                     for the visually impaired"
                          | 0x09 -> "AC-4 broadcast-mix audio description, stereo, \
                                     for the visually impaired, dialogue enhancement enabled"
                          | 0x0A -> "AC-4 broadcast-mix audio description, \
                                     multichannel, for the visually impaired"
                          | 0x0B -> "AC-4 broadcast-mix audio description, multichannel, \
                                     for the visually impaired, dialogue enhancement enabled"
                          | 0x0C -> "AC-4 receiver-mix audio description, mono, \
                                     for the visually impaired"
                          | 0x0D -> "AC-4 receiver-mix audio description, stereo, \
                                     for the visually impaired"
                          | x when x >= 0x0E && x <= 0xFF -> "reserved for future use"
                          | _    -> assert false)
                | x when x >= 0x2 && x <= 0xF -> (match component_type with
                                                  | x when x >= 0x00 && x <= 0xFF ->
                                                     "reserved for future use"
                                                  | _ -> assert false)
                | _ -> assert false)
      | 0xA -> (match stream_content_ext with
                | x when x >= 0x0 && x <= 0xF -> (match component_type with
                                                  | x when x >= 0x00 && x <= 0xFF ->
                                                     "reserved for future use"
                                                  | _ -> assert false)
                | _ -> assert false)
      | 0xB -> (match stream_content_ext with
                | x when x >= 0x0 && x <= 0xE -> (match component_type with
                                                  | x when x >= 0x00 && x <= 0xFF ->
                                                     "reserved for future use"
                                                  | _ -> assert false)
                | 0xF -> (match component_type with
                          | 0x00 -> "less than 16:9 aspect ratio"
                          | 0x01 -> "16:9 aspect ratio"
                          | 0x02 -> "greater than 16:9 aspect ratio"
                          | 0x03 -> "plano-stereoscopic top and bottom (TaB) frame-packing"
                          | x when x >= 0x04 && x <= 0xFF -> "reserved for future use"
                          | _    -> assert false)
                | _ -> assert false)
      | x when x >= 0xC && x <= 0xF -> (match component_type with
                                        | x when x >= 0x00 && x <= 0xFF -> "user defined"
                                        | _ -> assert false)
      | _ -> assert false

    let decode bs off =
      match%bitstring bs with
      | {| stream_content_ext : 4
         ; stream_content     : 4  : save_offset_to (off_1)
         ; component_type     : 8  : save_offset_to (off_2)
         ; component_tag      : 8  : save_offset_to (off_3)
         ; lang_code          : 24 : save_offset_to (off_4), bitstring
         ; rest               : -1 : save_offset_to (off_5), bitstring
         |} ->
         let parsed_lang, lang_code = parse_lang_code lang_code in
         let parsed = parse ~stream_content_ext ~stream_content ~component_type in
         let text = match Text_decoder.decode @@ Bitstring.to_cstruct rest with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         [ to_node ~offset:off 4 "stream_content_ext" (Hex (Int stream_content_ext))
         ; to_node ~offset:(off + off_1) 4  "stream_content" (Hex (Int stream_content))
         ; to_node ~parsed ~offset:(off + off_2) 8  "component_type" (Hex (Int component_type))
         ; to_node ~offset:(off + off_3) 8  "component_tag" (Hex (Int component_tag))
         ; to_node ~parsed:parsed_lang ~offset:(off + off_4) 24
             "ISO_639_language_code" (Hex (Int lang_code))
         ; to_node ~offset:(off + off_5) (Bitstring.length bs - 48) "text" (String text) ]
  end

  (* 0x51 *)
  module Mosaic = struct

    (* FIXME this might be working, but should be tested carefully*)

    let name = "mosaic_descriptor"

    let parse_present_info inf =
      match inf with
      | 0 -> "undefined"
      | 1 -> "video"
      | 2 -> "still picture"
      | 3 -> "graphics/text"
      | _ -> "reserved for future use"

    let parse_cli cli =
      match cli with
      | 0 -> "undefined"
      | 1 -> "bouquet related"
      | 2 -> "service related"
      | 3 -> "other mosaic related"
      | 4 -> "event related"
      | _ -> "reserved for future use"

    let parse_cells num =
      match num with
      | 0 -> "one cell"
      | 1 -> "two cells"
      | 2 -> "three cells"
      | 3 -> "four cells"
      | 4 -> "five cells"
      | 5 -> "six cells"
      | 6 -> "seven cells"
      | 7 -> "eight cells"
      | _ -> assert false

    let further ~cli ~offset _ x = (* FIXME acc *)
      match cli with
      | 0x01 ->
         begin match%bitstring x with
         | {| bouquet_id : 16
            ; rest       : -1 : save_offset_to (off_1), bitstring
            |} ->
            [ to_node ~offset 16 "bouquet_id" (Hex (Int bouquet_id))], rest, (offset + off_1)
         end
      | 0x02 | 0x03 ->
         begin match%bitstring x with
         | {| on_id : 16
            ; ts_id : 16 : save_offset_to (off_1)
            ; sv_id : 16 : save_offset_to (off_2)
            ; rest  : -1 : save_offset_to (off_3), bitstring
            |} ->
            [ to_node ~offset 16 "original_network_id" (Hex (Int on_id))
            ; to_node ~offset:(offset + off_1) 16 "transport_stream_id" (Hex (Int ts_id))
            ; to_node ~offset:(offset + off_2) 16 "service_id" (Hex (Int sv_id))],
            rest, (offset + off_3)
         end
      | 0x04 ->
         begin match%bitstring x with
         | {| on_id : 16
            ; ts_id : 16 : save_offset_to (off_1)
            ; sv_id : 16 : save_offset_to (off_2)
            ; ev_id : 16 : save_offset_to (off_3)
            ; rest  : -1 : save_offset_to (off_4), bitstring
            |} ->
            [ to_node ~offset 16 "original_network_id" (Hex (Int on_id))
            ; to_node ~offset:(offset + off_1) 16 "transport_stream_id" (Hex (Int ts_id))
            ; to_node ~offset:(offset + off_2) 16 "service_id" (Hex (Int sv_id))
            ; to_node ~offset:(offset + off_3) 16 "event_id" (Hex (Int ev_id))], rest, (offset + off_4)
           end
      | _ -> assert false

    let rec f_2 off x =
      if Bitstring.length x = 0 then [] else
        match%bitstring x with
        | {| rfu                : 2
           ; elementary_cell_id : 6  : save_offset_to (off_1)
           ; rest               : -1 : save_offset_to (off_2), bitstring
           |} ->
           let nodes =
             [ to_node ~offset:off 2 "reserved_future_use" (Bits (Int rfu))
             ; to_node ~offset:(off + off_1) 6 "elementary_cell_id" (Dec (Int elementary_cell_id)) ]
           in
           nodes @ f_2 (off + off_2) rest

    let rec f off x =
      if Bitstring.length x = 0 then [] else
        match%bitstring x with
        | {| logical_cell_id : 6
           ; rfu             : 7  : save_offset_to (off_1)
           ; pr_info         : 3  : save_offset_to (off_2)
           ; elem_len        : 8  : save_offset_to (off_3)
           ; elem_cell_field : elem_len * 8 : save_offset_to (off_4), bitstring
           ; cli             : 8  : save_offset_to (off_5)
           ; rest            : -1 : save_offset_to (off_6), bitstring
           |} ->
           let parsed = parse_present_info pr_info in
           let nodes =
             [ to_node ~offset:off 6 "logical_cell_id" (Hex (Int logical_cell_id))
             ; to_node ~offset:(off + off_1) 7 "reserved_future_use" (Bits (Int rfu))
             ; to_node ~parsed ~offset:(off + off_2) 3
                 "logical_cell_presentation_info" (Bits (Int pr_info))
             ; to_node ~offset:(off + off_3) 8 "elementary_cell_field_length" (Dec (Int elem_len)) ]
           in
           let nodes = nodes @ f_2 (off + off_4) elem_cell_field in
           let cli_parsed = parse_cli cli in
           let cli_node = [to_node ~parsed:cli_parsed ~offset:(off + off_5) 8
                             "cell_linkage_info" (Hex (Int cli))]
           in
           let nodes = nodes @ cli_node in
           let nodes, rest, off_7 = further ~cli ~offset:(off + off_6) nodes rest in
           nodes @ f (off + off_7) rest

    let decode bs off =
      match%bitstring bs with
      | {| mosaic_entry_point : 1
         ; hor                : 3  : save_offset_to (off_1)
         ; rfu                : 1  : save_offset_to (off_2)
         ; ver                : 3  : save_offset_to (off_3)
         ; rest               : -1 : save_offset_to (off_4), bitstring
         |} ->
         let ver_cells = parse_cells ver in
         let hor_cells = parse_cells hor in
         let nodes =
           [ to_node ~offset:off 1 "mosaic_entry_point" (Bits (Bool mosaic_entry_point))
           ; to_node ~parsed:hor_cells ~offset:(off + off_1) 3
               "number_of_horizontal_elementary_cells" (Dec (Int hor))
           ; to_node ~offset:(off + off_2) 1 "reserved_future_use" (Bits (Bool rfu))
           ; to_node ~parsed:ver_cells ~offset:(off + off_3) 3
               "number_of_vertical_elementary_cells" (Dec (Int ver)) ]
         in
         nodes @ f (off + off_4) rest

  end

  (* 0x52 *)
  module Stream_identifier = struct

    let name = "stream_identifier_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| comp_tag : 8 |} -> [to_node ~offset:off 8 "component_tag" (Hex (Int comp_tag))]

  end

  (* 0x53 *)
  module CA_identifier = struct

    let name = "CA_identifier_descriptor"

    let parse_system_id = function
      | 0x0000 -> "Reserved"
      | x when x > 0x0000 && x < 0x0100 -> "Reserved for registration to standardized \
                                            systems through the DVB Project Office"
      | x when x > 0x00FF && x <= 0xFFFF -> "Reserved for general registration through \
                                             the DVB Project Office"
      | _ -> assert false

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| ca_sys_id : 16
              ; rest      : -1 : save_offset_to (off_1), bitstring
              |} ->
              let parsed = parse_system_id ca_sys_id in
              let node = to_node ~parsed ~offset:off 16 "CA_system_id" (Hex (Int ca_sys_id)) in
              node :: f (off + off_1) rest

    let decode bs off = f off bs

  end

  (* 0x54 *)
  module Content = struct

    let name = "content_descriptor"

    let parse_lvl1 = function
      | 0x0 -> "Undefined"
      | 0x1 -> "Movie/Drama"
      | 0x2 -> "News/Current affairs"
      | 0x3 -> "Show/Game show"
      | 0x4 -> "Sports"
      | 0x5 -> "Children's/Youth programmes"
      | 0x6 -> "Music/Ballet/Dance"
      | 0x7 -> "Arts/Culture (without music)"
      | 0x8 -> "Social/Political issues/Economics"
      | 0x9 -> "Education/Science/Factual topics"
      | 0xA -> "Leisure hobbies"
      | 0xB -> "Special characteristics"
      | 0xF -> "User defined"
      | 0xC | 0xD | 0xE -> "Reserved for future use"
      | _   -> assert false

    let parse_lvl2 ~lvl_1 ~lvl_2 =
      match lvl_1 with
      | 0x0 -> (match lvl_2 with
                | x when x >= 0x0 && x <= 0xF -> "undefined content"
                | _ -> assert false)
      | 0x1 -> (match lvl_2 with
                | 0x0 -> "movie/drama (general)"
                | 0x1 -> "detective/thriller"
                | 0x2 -> "adventure/western/war"
                | 0x3 -> "science fiction/fantasy/horror"
                | 0x4 -> "comedy"
                | 0x5 -> "soap/melodrama/folkloric"
                | 0x6 -> "romance"
                | 0x7 -> "serious/classical/religious movie/drama"
                | 0x8 -> "adult movie/drama"
                | 0xF -> "user defined"
                | x when x >= 0x9 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0x2 -> (match lvl_2 with
                | 0x0 -> "news/current affairs (general)"
                | 0x1 -> "news/weather report"
                | 0x2 -> "news magazine"
                | 0x3 -> "documentary"
                | 0x4 -> "discussion/interview/debate"
                | 0xF -> "user defined"
                | x when x >= 0x5 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0x3 -> (match lvl_2 with
                | 0x0 -> "show/game show (general)"
                | 0x1 -> "game show/quiz/contest"
                | 0x2 -> "variety show"
                | 0x3 -> "talk show"
                | 0xF -> "user defined"
                | x when x >= 0x4 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0x4 -> (match lvl_2 with
                | 0x0 -> "sports (general)"
                | 0x1 -> "special events (Olympic Games, World Cup, etc.)"
                | 0x2 -> "sport magazines"
                | 0x3 -> "football/soccer"
                | 0x4 -> "tennis/squash"
                | 0x5 -> "team sports (excluding football)"
                | 0x6 -> "athletics"
                | 0x7 -> "motor sport"
                | 0x8 -> "water sport"
                | 0x9 -> "winter sports"
                | 0xA -> "equestrian"
                | 0xB -> "martial sports"
                | 0xF -> "user defined"
                | x when x >= 0xC && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0x5 -> (match lvl_2 with
                | 0x0 -> "children's/youth programmes (general)"
                | 0x1 -> "pre-school children's programmes"
                | 0x2 -> "entertainment programmes for 6 to 14"
                | 0x3 -> "entertainment programmes for 10 to 16"
                | 0x4 -> "informational/educational/school programmes"
                | 0x5 -> "cartoons/puppets"
                | 0xF -> "user defined"
                | x when x >= 0x6 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0x6 -> (match lvl_2 with
                | 0x0 -> "music/ballet/dance (general)"
                | 0x1 -> "rock/pop"
                | 0x2 -> "serious music/classical music"
                | 0x3 -> "folk/traditional music"
                | 0x4 -> "jazz"
                | 0x5 -> "musical/opera"
                | 0x6 -> "ballet"
                | 0xF -> "user defined"
                | x when x >= 0x7 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0x7 -> (match lvl_2 with
                | 0x0 -> "arts/culture (without music, general)"
                | 0x1 -> "performing arts"
                | 0x2 -> "fine arts"
                | 0x3 -> "religion"
                | 0x4 -> "popular culture/traditional arts"
                | 0x5 -> "literature"
                | 0x6 -> "film/cinema"
                | 0x7 -> "experimental film/video"
                | 0x8 -> "broadcasting/press"
                | 0x9 -> "new media"
                | 0xA -> "arts/culture magazines"
                | 0xB -> "fashion"
                | 0xF -> "user defined"
                | 0xC | 0xD | 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0x8 -> (match lvl_2 with
                | 0x0 -> "social/political issues/economics (general)"
                | 0x1 -> "magazines/reports/documentary"
                | 0x2 -> "economics/social advisory"
                | 0x3 -> "remarkable people"
                | 0xF -> "user defined"
                | x when x >= 0x4 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0x9 -> (match lvl_2 with
                | 0x0 -> "education/science/factual topics (general)"
                | 0x1 -> "nature/animals/environment"
                | 0x2 -> "technology/natural sciences"
                | 0x3 -> "medicine/physiology/psychology"
                | 0x4 -> "foreign countries/expeditions"
                | 0x5 -> "social/spiritual sciences"
                | 0x6 -> "further education"
                | 0x7 -> "languages"
                | 0xF -> "user defined"
                | x when x >= 0x8 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0xA -> (match lvl_2 with
                | 0x0 -> "leisure hobbies (general)"
                | 0x1 -> "tourism/travel"
                | 0x2 -> "handicraft"
                | 0x3 -> "motoring"
                | 0x4 -> "fitness and health"
                | 0x5 -> "cooking"
                | 0x6 -> "advertisement/shopping"
                | 0x7 -> "gardening"
                | 0xF -> "user defined"
                | x when x >= 0x8 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0xB -> (match lvl_2 with
                | 0x0 -> "original language"
                | 0x1 -> "black and white"
                | 0x2 -> "unpublished"
                | 0x3 -> "live broadcast"
                | 0x4 -> "plano-stereoscopic"
                | 0x5 -> "local or regional"
                | 0xF -> "user defined"
                | x when x >= 0x6 && x <= 0xE -> "reserved for future use"
                | _   -> assert false)
      | 0xF -> (match lvl_2 with
                | x when x >= 0x0 && x <= 0xF -> "user defined"
                | _ -> assert false)
      | 0xC | 0xD | 0xE -> (match lvl_2 with
                            | x when x >= 0x0 && x <= 0xF -> "reserved for future use"
                            | _ -> assert false)
      | _ -> assert false



    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| cont_lvl1 : 4
              ; cont_lvl2 : 4  : save_offset_to (off_1)
              ; user_byte : 8  : save_offset_to (off_2)
              ; rest      : -1 : save_offset_to (off_3), bitstring
              |} ->
              let lvl_1 = parse_lvl1 cont_lvl1 in
              let lvl_2 = parse_lvl2 ~lvl_1:cont_lvl1 ~lvl_2:cont_lvl2 in
              let nodes =
                [ to_node ~parsed:lvl_1 ~offset:off 4 "content_nibble_level_1" (Hex (Int cont_lvl1))
                ; to_node ~parsed:lvl_2 ~offset:(off + off_1) 4 "content_nibble_level_2" (Hex (Int cont_lvl2))
                ; to_node ~offset:(off + off_2) 8 "user_byte" (Bits (Int user_byte))]
              in
              nodes @ f (off + off_3) rest

    let decode bs off = f off bs

  end

  (* 0x55 *)
  module Parental_rating = struct

    let name = "patental_rating_descriptor"

    let parse_rating rating =
      match rating with
      | 0x00 -> "undefined"
      | x when x > 0x00 && x < 0x10 -> Printf.sprintf "%d+" (x + 3)
      | _ -> "defined by the broadcaster"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| country_code : 24 : bitstring
              ; rating       : 8  : save_offset_to (off_1)
              ; rest         : -1 : save_offset_to (off_2), bitstring
              |} ->
              let p_rating = parse_rating rating in
              let p_code, country_code = parse_lang_code country_code in
              let nodes =
                [ to_node ~parsed:p_code ~offset:off 24 "country_code" (Bits (Int country_code))
                ; to_node ~parsed:p_rating ~offset:(off + off_1) 8 "rating" (Dec (Int rating)) ]
              in
              let node = to_node ~offset:off 32 p_code (List nodes) in
              node :: f (off + off_2) rest

    let decode bs off = f off bs

  end

  (* 0x56 *)
  module Teletext = struct

    let name = "teletext_descriptor"

    let parse_type = function
      | 0x00 -> "reserved for future use"
      | 0x01 -> "initial Teletext page"
      | 0x02 -> "Teletext subtitle page"
      | 0x03 -> "additional information page"
      | 0x04 -> "programme schedule page"
      | 0x05 -> "Teletext subtitle page for hearing impaired people"
      | x when x > 0x05 && x < 0x20 -> "reserved_for_future_use"
      | x    -> Printf.sprintf "%d" x

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| lang_code : 24 : bitstring
              ; txt_type  : 5  : save_offset_to (off_1)
              ; mag_num   : 3  : save_offset_to (off_2)
              ; page_num  : 8  : save_offset_to (off_3)
              ; rest      : -1 : save_offset_to (off_4), bitstring
              |} ->
              let p_code, lang_code = parse_lang_code lang_code in
              let parsed = parse_type txt_type in
              let nodes =
                [ to_node ~parsed:p_code ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
                ; to_node ~parsed ~offset:(off + off_1) 5 "teletext_type" (Hex(Int txt_type))
                ; to_node ~offset:(off + off_2) 3 "teletext_magazine_number" (Dec (Int mag_num))
                ; to_node ~offset:(off + off_3) 8 "teletext_page_number" (Dec (Int page_num)) ]
              in
              let node = to_node ~offset:off 40 p_code (List nodes) in
              node :: f (off + off_4) rest

    let decode bs off = f off bs


  end

  (* 0x57 *)
  module Telephone = struct

    let name = "telephone_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| rfu_1          : 2
         ; foreign_avail  : 1 : save_offset_to (off_1)
         ; connection     : 5 : save_offset_to (off_2)
         ; rfu_2          : 1 : save_offset_to (off_3)
         ; length_1       : 2 : save_offset_to (off_4)
         ; length_2       : 3 : save_offset_to (off_5)
         ; length_3       : 2 : save_offset_to (off_6)
         ; rfu_3          : 1 : save_offset_to (off_7)
         ; length_4       : 3 : save_offset_to (off_8)
         ; length_5       : 4 : save_offset_to (off_9)
         ; country_prefix : length_1 * 8 : save_offset_to (off_10), bitstring
         ; int_area_code  : length_2 * 8 : save_offset_to (off_11), bitstring
         ; operator_code  : length_3 * 8 : save_offset_to (off_12), bitstring
         ; nat_area_code  : length_4 * 8 : save_offset_to (off_13), bitstring
         ; core_number    : length_5 * 8 : save_offset_to (off_14), bitstring
         |} ->
         let country_prefix = match Text_decoder.decode @@ Bitstring.to_cstruct country_prefix with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         let int_code = match Text_decoder.decode @@ Bitstring.to_cstruct int_area_code with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         let operator_code = match Text_decoder.decode @@ Bitstring.to_cstruct operator_code with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         let nat_area_code = match Text_decoder.decode @@ Bitstring.to_cstruct nat_area_code with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         let core_number = match Text_decoder.decode @@ Bitstring.to_cstruct core_number with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         [ to_node ~offset:off 2 "reserved_future_use" (Bits (Int rfu_1))
         ; to_node ~offset:(off + off_1) 1 "foreign_availability" (Bits (Bool foreign_avail))
         ; to_node ~offset:(off + off_2) 5 "connection_type" (Hex (Int connection))
         ; to_node ~offset:(off + off_3) 1 "reserved_future_use" (Bits (Bool rfu_2))
         ; to_node ~offset:(off + off_4) 2 "country_prefix_length" (Dec (Int length_1))
         ; to_node ~offset:(off + off_5) 3 "international_area_code_length" (Dec (Int length_2))
         ; to_node ~offset:(off + off_6) 2 "operator_code_length" (Dec (Int length_3))
         ; to_node ~offset:(off + off_7) 1 "reserved_future_use" (Bits (Bool rfu_3))
         ; to_node ~offset:(off + off_8) 3 "national_area_code_length" (Dec (Int length_4))
         ; to_node ~offset:(off + off_9) 4 "core_number_length" (Dec (Int length_5))
         ; to_node ~offset:(off + off_10) (length_1 * 8) "country_prefix" (String country_prefix)
         ; to_node ~offset:(off + off_11) (length_2 * 8) "international_area_code" (String int_code)
         ; to_node ~offset:(off + off_12) (length_3 * 8) "operator_code" (String operator_code)
         ; to_node ~offset:(off + off_13) (length_4 * 8) "national_area_code" (String nat_area_code)
         ; to_node ~offset:(off + off_14) (length_5 * 8) "core_number" (String core_number) ]
  end

  (* 0x58 *)
  module Local_time_offset = struct

    let name = "local_time_offset_descriptor"

    let parse_country country =
      match country with
      | 0 -> "no time zone extension used"
      | x when x > 0 && x < 61 -> Printf.sprintf "time zone %d" x
      | _ -> "reserved"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| country_code     : 24 : bitstring
              ; country_reg_id   : 6  : save_offset_to (off_1)
              ; reserved         : 1  : save_offset_to (off_2)
              ; offset_pol       : 1  : save_offset_to (off_3)
              ; offset           : 16 : save_offset_to (off_4), bitstring
              ; time_of_change   : 40 : save_offset_to (off_5), bitstring
              ; next_time_offset : 16 : save_offset_to (off_6), bitstring
              ; rest             : -1 : save_offset_to (off_7), bitstring
              |} ->
              let p_code, country_code = parse_lang_code country_code in
              let time_ch = match parse_timestamp time_of_change with
                | Some x -> Time x
                | None   -> match%bitstring time_of_change with
                            | {| i : 40 |} -> Dec (Uint64  i)
              in
              let local_offset = match parse_timestamp offset with
                | Some x -> Time x
                | None   -> match%bitstring time_of_change with
                            | {| i : 40 |} -> Dec (Uint64  i)
              in
              let nt_offset = match parse_timestamp next_time_offset with
                | Some x -> Time x
                | None   -> match%bitstring time_of_change with
                            | {| i : 40 |} -> Dec (Uint64  i)
              in
              let nodes =
                [ to_node ~parsed:p_code ~offset:off 24 "country_code" (Bits (Int country_code))
                ; to_node ~offset:(off + off_1) 6  "country_reg_id" (Hex (Int country_reg_id))
                ; to_node ~offset:(off + off_2) 1  "reserved" (Bits (Bool reserved))
                ; to_node ~offset:(off + off_3) 1  "local_time_offset_polarity" (Bits (Bool offset_pol))
                ; to_node ~offset:(off + off_4) 16 "local_time_offset" local_offset
                ; to_node ~offset:(off + off_5) 40 "time_of_change" time_ch
                ; to_node ~offset:(off + off_6) 16 "next_time_offset" nt_offset ]
              in
              let node = to_node ~offset:off 104 p_code (List nodes) in
              node :: f (off + off_7) rest

    let decode bs off = f off bs

  end

  (* 0x59 *)
  module Subtitling = struct

    let name = "subtitling_descriptor"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| lang_code : 24 : bitstring
              ; sub_type  : 8  : save_offset_to (off_1)
              ; cp_id     : 16 : save_offset_to (off_2)
              ; ap_id     : 16 : save_offset_to (off_3)
              ; rest      : -1 : save_offset_to (off_4), bitstring
              |} ->
              let parsed, lang_code = parse_lang_code lang_code in
              let nodes =
                [ to_node ~parsed ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
                ; to_node ~offset:(off + off_1) 8  "subtitling_type" (Hex (Int sub_type))
                ; to_node ~offset:(off + off_2) 16 "composition_page_id" (Hex (Int cp_id))
                ; to_node ~offset:(off + off_3) 16 "ancillary_page_id" (Hex (Int ap_id)) ]
              in
              let node = to_node ~offset:off 64 parsed (List nodes) in
              node :: f (off + off_4) rest

    let decode bs off = f off bs

  end

  (* 0x5A *)
  module Terrestrial_delivery = struct

    let name = "terrestrial_delivery_system_descriptor"

    let parse_bandwith bandwith =
      match bandwith with
      | 0 -> "8 MHz"
      | 1 -> "7 MHz"
      | 2 -> "6 MHz"
      | 3 -> "5 MHz"
      | _ -> "Reserved for future use"

    let parse_priority priority =
      match priority with
      | true  -> "HP (high priority)"
      | false -> "LP (low priority)"

    let parse_constellation const =
      match const with
      | 0 -> "QPSK"
      | 1 -> "16-QAM"
      | 2 -> "64-QAM"
      | 3 -> "reserved for future use"
      | _ -> assert false

    let parse_hierarchy hier =
      match hier with
      | 0 -> "non-hierarchical, native interleaver"
      | 1 -> "a = 1, native interleaver"
      | 2 -> "a = 2, native interleaver"
      | 3 -> "a = 4, native interleaver"
      | 4 -> "non-hierarchical, in-depth interleaver"
      | 5 -> "a = 1, in-depth interleaver"
      | 6 -> "a = 2, in-depth interleaver"
      | 7 -> "a = 4, in-depth interleaver"
      | _ -> assert false

    let parse_code_rate cr =
      match cr with
      | 0 -> "1/2"
      | 1 -> "2/3"
      | 2 -> "3/4"
      | 3 -> "5/6"
      | 4 -> "7/8"
      | _ -> "reserved_for_future_use"

    let parse_guard_interval gi =
      match gi with
      | 0 -> "1/32"
      | 1 -> "1/16"
      | 2 -> "1/8"
      | 3 -> "1/4"
      | _ -> assert false

    let parse_transmission tr =
      match tr with
      | 0 -> "2k mode"
      | 1 -> "8k mode"
      | 2 -> "4k mode"
      | 3 -> "reserved for future use"
      | _ -> assert false

    let decode bs off =
      match%bitstring bs with
      | {| centre_frequency  : 32
         ; bandwith          : 3  : save_offset_to (off_1)
         ; priority          : 1  : save_offset_to (off_2)
         ; time_slicing_ind  : 1  : save_offset_to (off_3)
         ; mpe_fec_ind       : 1  : save_offset_to (off_4)
         ; rfu_1             : 2  : save_offset_to (off_5)
         ; constellation     : 2  : save_offset_to (off_6)
         ; hierarchy_info    : 3  : save_offset_to (off_7)
         ; code_rate_hp_str  : 3  : save_offset_to (off_8)
         ; code_rate_lp_str  : 3  : save_offset_to (off_9)
         ; guard_interval    : 2  : save_offset_to (off_10)
         ; transmission_mode : 2  : save_offset_to (off_11)
         ; other_freq_flag   : 1  : save_offset_to (off_12)
         ; rfu_2             : 32 : save_offset_to (off_13)
         |} ->
         let trans = parse_transmission transmission_mode in
         let guard = parse_guard_interval guard_interval in
         let hp_cr = parse_code_rate code_rate_hp_str in
         let lp_cr = parse_code_rate code_rate_lp_str in
         let hier  = parse_hierarchy hierarchy_info in
         let const = parse_constellation constellation in
         let prior = parse_priority priority in
         let bandw = parse_bandwith bandwith in
         [ to_node ~offset:off 32 "centre_frequency" (Dec (Int32 centre_frequency))
         ; to_node ~parsed:bandw ~offset:(off + off_1)  3  "bandwith" (Dec (Int bandwith))
         ; to_node ~parsed:prior ~offset:(off + off_2)  1  "priority" (Bits (Bool priority))
         ; to_node ~offset:(off + off_3) 1 "Time_slicing_indicator" (Bits (Bool time_slicing_ind))
         ; to_node ~offset:(off + off_4) 1 "MPE-FEC-indicator" (Bits (Bool mpe_fec_ind))
         ; to_node ~offset:(off + off_5) 2 "reserved_future_use" (Bits (Int rfu_1))
         ; to_node ~parsed:const ~offset:(off + off_6)  2  "constellation" (Bits (Int rfu_1))
         ; to_node ~parsed:hier  ~offset:(off + off_7)  3
             "hierarchy_information" (Hex (Int hierarchy_info))
         ; to_node ~parsed:hp_cr ~offset:(off + off_8)  3
             "code_rate-HP_stream" (Dec (Int code_rate_hp_str))
         ; to_node ~parsed:lp_cr ~offset:(off + off_9)  3
             "code_rate-LP_stream" (Dec (Int code_rate_lp_str))
         ; to_node ~parsed:guard ~offset:(off + off_10) 2
             "guard_interval" (Dec (Int guard_interval))
         ; to_node ~parsed:trans ~offset:(off + off_11) 2
             "transmission_mode" (Dec (Int transmission_mode))
         ; to_node ~offset:(off + off_12) 1  "other_frequency_flag" (Bits (Bool other_freq_flag))
         ; to_node ~offset:(off + off_13) 32 "reserved_future_use" (Bits (Int32 rfu_2)) ]

  end

  (* 0x5B *)
  module Multilingual_network = struct

    let name = "multilingual_network_name_descriptor"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| lang_code : 24 : bitstring
              ; length    : 8  : save_offset_to (off_1)
              ; network   : length * 8 : save_offset_to (off_2), bitstring
              ; rest      : -1 : save_offset_to (off_3), bitstring
              |} ->
              let parsed, lang_code = parse_lang_code lang_code in
              let network_name = match Text_decoder.decode @@ Bitstring.to_cstruct network with
                | Ok s    -> s
                | Error _ -> "Unable to decode"
              in
              let nodes =
                [ to_node ~parsed ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
                ; to_node ~offset:(off + off_1) 8 "network_name_length" (Dec (Int length))
                ; to_node ~offset:(off + off_2) (length * 8) "network_name" (String network_name)]
              in
              let node = to_node ~offset:off (32 + length * 8) parsed (List nodes) in
              node :: f (off + off_3) rest

    let decode bs off = f off bs

  end

  (* 0x5C *)
  module Multilingual_bouquet = struct

    let name = "multilingual_bouquet_name_descriptor"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| lang_code : 24 : bitstring
              ; length    : 8  : save_offset_to (off_1)
              ; bouquet   : length * 8 : save_offset_to (off_2), bitstring
              ; rest      : -1 : save_offset_to (off_3), bitstring
              |} ->
              let parsed, lang_code = parse_lang_code lang_code in
              let name = match Text_decoder.decode @@ Bitstring.to_cstruct bouquet with
                | Ok s    -> s
                | Error _ -> "Unable to decode"
              in
              let nodes =
                [ to_node ~parsed ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
                ; to_node ~offset:(off + off_1) 8 "bouquet_name_length" (Dec (Int length))
                ; to_node ~offset:(off + off_2) (length * 8) "bouquet_name" (String name) ]
              in
              let node = to_node ~offset:off (32 + length * 8) parsed (List nodes) in
              node :: f (off + off_3) rest

    let decode bs off = f off bs

  end

  (* 0x5D *)
  module Multilinguql_service = struct

    let name = "multilingual_service_name_descriptor"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| lang_code : 24 : bitstring
              ; length_1  : 8  : save_offset_to (off_1)
              ; service_p : length_1 * 8 : save_offset_to (off_2), bitstring
              ; length_2  : 8  : save_offset_to (off_3)
              ; service   : length_2 * 8 : save_offset_to (off_4), bitstring
              ; rest      : -1 : save_offset_to (off_5), bitstring
              |} ->
              let parsed, lang_code = parse_lang_code lang_code in
              let ser_p = match Text_decoder.decode @@ Bitstring.to_cstruct service_p with
                | Ok s    -> s
                | Error _ -> "Unable to decode"
              in
              let ser = match Text_decoder.decode @@ Bitstring.to_cstruct service with
                | Ok s    -> s
                | Error _ -> "Unable to decode"
              in
              let nodes =
                [ to_node ~parsed ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
                ; to_node ~offset:(off + off_1) 8 "service_provider_name_length" (Dec (Int length_1))
                ; to_node ~offset:(off + off_2) (length_1 * 8) "service_provider_name" (String ser_p)
                ; to_node ~offset:(off + off_3) 8 "service_name_length" (Dec (Int length_2))
                ; to_node ~offset:(off + off_4) (length_2 * 8) "service__name" (String ser) ]
              in
              let real_length = 40 + (length_1 + length_2) * 8 in
              let node = to_node ~offset:off real_length parsed (List nodes) in
              node :: f (off + off_5) rest

    let decode bs off = f off bs

  end

  (* 0x5E *)
  module Multilingual_component = struct

    let name = "multilingual_component_descriptor"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| lang_code : 24 : bitstring
              ; length    : 8  : save_offset_to (off_1)
              ; descr     : length * 8 : save_offset_to (off_2), bitstring
              ; rest      : -1 : save_offset_to (off_3), bitstring
              |} ->
              let parsed, lang_code = parse_lang_code lang_code in
              let text = match Text_decoder.decode @@ Bitstring.to_cstruct descr with
                | Ok s    -> s
                | Error _ -> "Unable to decode"
              in
              let nodes =
                [ to_node ~parsed ~offset:off 24 "ISO_639_language_code" (Bits (Int lang_code))
                ; to_node ~offset:(off + off_1) 8 "text_description_length" (Dec (Int length))
                ; to_node ~offset:(off + off_2) (length * 8) "text_description" (String text) ]
              in
              let real_length = 32 + length * 8 in
              let node = to_node ~offset:off real_length parsed (List nodes) in
              node :: f (off + off_3) rest

    let decode bs off =
      match%bitstring bs with
      | {| component_tag : 8
         ; rest          : -1 : save_offset_to (off_1), bitstring
         |} ->
         let node = to_node ~offset:off 8 "component_tag" (Hex (Int component_tag)) in
         node :: f (off + off_1) rest

  end

  (* 0x5F *)
  module Private_data_specifier = struct

    let name = "private_data_specifier_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| pds : 32
         |} ->
         [ to_node ~offset:off 32 "private_data_specifier" (Hex (Int32 pds)) ]

  end

  (* 0x60 *)
  module Service_move = struct

    let name = "service_move_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| new_on_id : 16
         ; new_ts_id : 16 : save_offset_to (off_1)
         ; new_sv_id : 16 : save_offset_to (off_2)
         |} ->
         [ to_node ~offset:off 16 "new_original_network_id" (Hex (Int new_on_id))
         ; to_node ~offset:(off + off_1) 16 "new_transport_stream_id" (Hex (Int new_ts_id))
         ; to_node ~offset:(off + off_2) 16 "new_service_id" (Hex (Int new_sv_id)) ]

  end

  (* 0x61 *)
  module Short_smoothing_buffer = struct

    let name = "short_smoothing_buffer_descriptor"

    let parse_size size =
      match size with
      | 1 -> "1 536"
      | _ -> "DVB_reserved"

    let parse_leak_rate = function
      | 0  -> "DVB_reserved"
      | 1  -> "0,0009" | 2  -> "0,0018" | 3  -> "0,0036" | 4  -> "0,0072" | 5  -> "0,0108"
      | 6  -> "0,0144" | 7  -> "0,0216" | 8  -> "0,0288" | 9  -> "0,075"  | 10 -> "0,5"
      | 11 -> "0,5625" | 12 -> "0,8437" | 13 -> "1,0"    | 14 -> "1,1250" | 15 -> "1,5"
      | 16 -> "1,6875" | 17 -> "2,0"    | 18 -> "2,25"   | 19 -> "2,5"    | 20 -> "3,0"
      | 21 -> "3,3750" | 22 -> "3,5"    | 23 -> "4,0"    | 24 -> "4,5"    | 25 -> "5,0"
      | 26 -> "5,5"    | 27 -> "6,0"    | 28 -> "6,5"    | 29 -> "6,75"   | 30 -> "7,0"
      | 31 -> "7,5"    | 32 -> "8,0"    | 38 -> "13,5"   | 48 -> "27"     | 56 -> "44"
      | 57 -> "48"     | 58 -> "54"     | 59 -> "72"     | 60 -> "108"
      | x when x > 60 && x < 64 -> "DVB_reserved"
      | x when x > 32 && x < 38 -> Printf.sprintf "%d" ((x - 16) / 2)
      | x when x > 38 && x < 44 -> Printf.sprintf "%d" (x - 24)
      | x when x > 43 && x < 48 -> Printf.sprintf "%d" (x - 25)
      | x when x > 48 && x < 56 -> Printf.sprintf "%d" ((x - 34) * 2)
      | _ -> assert false

    let decode bs off =
      match%bitstring bs with
      | {| sb_size : 2
         ; sb_lr   : 6  : save_offset_to (off_1)
         ; rest    : -1 : save_offset_to (off_2), bitstring
         |} ->
         let size  = parse_size sb_size in
         let lr    = parse_leak_rate sb_lr in
         let nodes =
           [ to_node ~parsed:size ~offset:off 2 "sb_size" (Dec (Int sb_size))
           ; to_node ~parsed:lr ~offset:(off + off_1) 6 "sb_leak_rate" (Dec (Int sb_lr)) ]
         in
         nodes @ parse_bytes ~offset:(off + off_2) rest "DVB_reserved"

  end

  (* 0x62 *)
  module Frequency_list = struct

    let name = "frequency_list_descriptor"

    let parse_coding_type typ =
      match typ with
      | 0 -> "not defined"
      | 1 -> "satellite"
      | 2 -> "cable"
      | 3 -> "terrestrial"
      | _ -> assert false

    let decode bs off =
      match%bitstring bs with
      | {| rfu         : 6
         ; coding_type : 2  : save_offset_to (off_1)
         ; rest        : -1 : save_offset_to (off_2), bitstring
         |} ->
         let parsed = parse_coding_type coding_type in
         let nodes =
           [ to_node ~offset:off 6 "reserved_future_use" (Bits (Int rfu))
           ; to_node ~parsed ~offset:(off + off_1) 2 "coding_type" (Bits (Int coding_type)) ]
         in
         nodes @ parse_bytes ~bytes:4 ~offset:(off + off_2) rest "centre_frequency"

  end

  (* 0x63 *)
  module Partial_transport_stream = struct

    let name = "Partial Transport Stream (TS) descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| dvb_rfu_1 : 2
         ; peak_rate : 22 : save_offset_to (off_1)
         ; dvb_rfu_2 : 2  : save_offset_to (off_2)
         ; min_rate  : 22 : save_offset_to (off_3)
         ; dvb_rfu_3 : 2  : save_offset_to (off_4)
         ; max_rate  : 14 : save_offset_to (off_5)
         |} ->
         [ to_node ~offset:off 2 "DVB_reserved_future_use" (Bits (Int dvb_rfu_1))
         ; to_node ~offset:(off + off_1) 22 "peak_rate" (Dec (Uint peak_rate))
         ; to_node ~offset:(off + off_2) 2  "DVB_reserved_future_use" (Bits (Int dvb_rfu_2))
         ; to_node ~offset:(off + off_3) 22 "minimum_overall_smoothing_rate" (Dec (Uint min_rate))
         ; to_node ~offset:(off + off_4) 2  "DVB_reserved_future_use" (Bits (Int dvb_rfu_3))
         ; to_node ~offset:(off + off_5) 14 "maximum_overall_smoothing_rate" (Dec (Uint max_rate)) ]

  end

  (* 0x64 *)
  module Data_broadcast = struct

    let name = "data_broadcast_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| data_broadcast_id : 16
         ; component_tag     : 8  : save_offset_to (off_1)
         ; sel_length        : 8  : save_offset_to (off_2)
         ; selector_bytes    : sel_length * 8 : save_offset_to (off_3), bitstring
         ; lang_code         : 24 : save_offset_to (off_4), bitstring
         ; text_length       : 8  : save_offset_to (off_5)
         ; text_chars        : text_length * 8 : save_offset_to (off_6), bitstring
         |} ->
         let parsed, lang_code = parse_lang_code lang_code in
         let text =match Text_decoder.decode @@ Bitstring.to_cstruct text_chars with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         let nodes_1 =
           [ to_node ~offset:off 16 "data_broadcast_id" (Hex (Int data_broadcast_id))
           ; to_node ~offset:(off + off_1) 8 "component_tag" (Hex (Int component_tag))
           ; to_node ~offset:(off + off_2) 8 "selector_length" (Dec (Int sel_length)) ]
         in
         (nodes_1 @ parse_bytes ~offset:(off + off_3) selector_bytes "selector_byte")
         @ [ to_node ~parsed ~offset:(off + off_4) 24 "ISO_639_language_code" (Hex (Int lang_code))
           ; to_node ~offset:(off + off_5) 8 "text_length" (Dec (Int text_length))
           ; to_node ~offset:(off + off_6) (text_length * 8) "text" (String text) ]

  end

  (* 0x65 *)
  module Scrambling = struct

    let name = "scrambling_descriptor"

    let parse_mode mode =
      match mode with
      | 0x01 -> "DVB-CSA1"
      | 0x02 -> "DVB-CSA2"
      | 0x03 -> "DVB-CSA3 in standard mode"
      | 0x04 -> "DVB-CSA3 in minimally enhanced mode"
      | 0x05 -> "DVB-CSA3 in fully enhanced mode"
      | 0x10 -> "DVB-CISSA version 1"
      | 0x00 | 0xFF -> "Reserved for future use"
      | x when (x > 0x05 && x < 0x10 || x > 0x1F && x < 0x70)-> "Reserved for future use"
      | x when x > 0x10 && x < 0x20 -> "Reserved for future DVB-CSIISA versions"
      | x when x > 0x6F && x < 0x80 -> "ATIS defined (ATIS-0800006, see annex J)"
      | x when x > 0x80 && x < 0xFF -> "User defined"
      | _ -> assert false

    let decode bs off =
      match%bitstring bs with
      | {| scrambling_mode : 8
         |} ->
         let parsed = parse_mode scrambling_mode in
         [ to_node ~parsed ~offset:off 32 "scrambling_mode" (Hex (Int scrambling_mode)) ]

  end

  (* 0x66 *)
  module Data_broadcast_id = struct

    let name = "data_broadcast_id_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| data_broadcast_id : 16
         ; rest              : -1 : save_offset_to (off_1), bitstring
         |} ->
         let node = to_node ~offset:off 16 "data_broadcast_id" (Hex (Int data_broadcast_id)) in
         node :: parse_bytes ~offset:(off + off_1) rest "id_selector_byte"

  end

  (* 0x67 *)
  module Transport_stream = struct

    let name = "transport_stream_descriptor"

    let decode bs off = parse_bytes ~offset:off bs "byte"

  end

  (* 0x68 *)
  module DSNG = struct

    let name = "DSNG_descriptor"

    let decode _ _ = []

  end

  (* 0x69 *)
  module PDC = struct

    let name = "PDC_descriptor"

    let parse_label pil =
      match%bitstring pil with
      | {| day   : 5
         ; month : 4
         ; hour  : 5
         ; min   : 6
         |} ->
         let month = match month with
           | 1  -> "January"
           | 2  -> "February"
           | 3  -> "March"
           | 4  -> "April"
           | 5  -> "May"
           | 6  -> "June"
           | 7  -> "July"
           | 8  -> "August"
           | 9  -> "September"
           | 10 -> "October"
           | 11 -> "November"
           | 12 -> "December"
           | x  -> Printf.sprintf "%d" x
         in
         Printf.sprintf "%d %s %d:%d" day month hour min

    let decode bs off =
      match%bitstring bs with
      | {| rfu : 4
         ; pil : 20 : save_offset_to (off_1), bitstring
         |} ->
         let parsed = parse_label pil in
         let pil = match%bitstring pil with | {|pil : 20|} -> pil in
         [ to_node ~offset:off 4 "reserved_future_use" (Bits (Int rfu))
         ; to_node ~parsed ~offset:(off + off_1) 20
             "programme_identification_label" (Bits (Int pil)) ]

  end

  (* 0x6A *)
  module AC3 = struct

    let name = "AC-3_descriptor"
    (* TODO *)
    let decode _ _ = []

  end

  (* 0x6B *)
  module Ancillary_data = struct

    let name = "ancillary_data_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| anc_data_id : 8
         |} ->
         [ to_node ~offset:off 8 "ancillary_data_identifier" (Hex (Int anc_data_id))]

  end

  (* 0x6C *)
  module Cell_list = struct

    let name = "cell_list_descriptor"

    let decode_lat lat = Printf.sprintf "%f deg" @@ (float_of_int @@ lat * 90) /. 32268.
    let decode_lon lon = Printf.sprintf "%f deg" @@ (float_of_int @@ lon * 180) /. 32268.

    let rec decode_second_loop off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| cell_id_ext : 8
              ; subcell_lat : 16 : save_offset_to (off_1)
              ; subcell_lon : 16 : save_offset_to (off_2)
              ; sub_ext_lat : 12 : save_offset_to (off_3)
              ; sub_ext_lon : 12 : save_offset_to (off_4)
              ; rest        : -1 : save_offset_to (off_5), bitstring
              |} ->
              let lat, ext_lat = decode_lat subcell_lat, decode_lat sub_ext_lat in
              let lon, ext_lon = decode_lon subcell_lon, decode_lon sub_ext_lon in
              let nodes =
                [ to_node ~offset:off 8 "cell_id_extension" (Dec (Int cell_id_ext))
                ; to_node ~parsed:lat ~offset:(off + off_1) 16
                    "subcell_latitude" (Dec (Int subcell_lat))
                ; to_node ~parsed:lon ~offset:(off + off_2) 16
                    "subcell_longtitude" (Dec (Int subcell_lon))
                ; to_node ~parsed:ext_lat ~offset:(off + off_3) 12
                    "subcell_extent_of_latitude" (Dec (Int sub_ext_lat))
                ; to_node ~parsed:ext_lon ~offset:(off + off_4) 12
                    "subcell_extent_of_longtitude" (Dec (Int sub_ext_lon)) ]
              in
              let name = Printf.sprintf "Subcell %s" (string_of_int cell_id_ext) in
              let node = to_node ~offset:off 64 name (List nodes) in
              node :: decode_second_loop (off + off_5) rest

    let rec decode_first_loop off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| cell_id      : 16
              ; cell_lat     : 16 : save_offset_to (off_1)
              ; cell_lon     : 16 : save_offset_to (off_2)
              ; cell_ext_lat : 12 : save_offset_to (off_3)
              ; cell_ext_lon : 12 : save_offset_to (off_4)
              ; loop_length  : 8  : save_offset_to (off_5)
              ; snd_loop     : loop_length * 8 : save_offset_to (off_6), bitstring
              ; rest         : -1 : save_offset_to (off_7), bitstring
              |} ->
              let lat, ext_lat = decode_lat cell_lat, decode_lat cell_ext_lat in
              let lon, ext_lon = decode_lon cell_lon, decode_lon cell_ext_lon in
              let name  = Printf.sprintf "Cell %s" (string_of_int cell_id) in
              let snd_loop = decode_second_loop (off + off_6) snd_loop in
              let nodes =
                [ to_node ~offset:off 16 "cell_id" (Dec (Int cell_id))
                ; to_node ~parsed:lat ~offset:(off + off_1) 16
                    "cell_latitude" (Dec (Int cell_lat))
                ; to_node ~parsed:lon ~offset:(off + off_2) 16
                    "cell_longtitude" (Dec (Int cell_lon))
                ; to_node ~parsed:ext_lat ~offset:(off + off_3) 16
                    "cell_extent_of_latitude" (Dec (Int cell_ext_lat))
                ; to_node ~parsed:ext_lon ~offset:(off + off_4) 16
                    "cell_extent_of_longtitude" (Dec (Int cell_ext_lon))
                ; to_node ~offset:(off + off_5) 8 "subcell_info_loop_length" (Dec (Int loop_length))
                ; to_node ~offset:(off + off_6) (loop_length * 8) "Subcells" (List snd_loop) ]
              in
              let real_length = 88 + loop_length * 8 in
              let node = to_node ~offset:off real_length name (List nodes) in
              node :: decode_first_loop (off + off_7) rest

    let decode bs off = decode_first_loop off bs

  end

  (* 0x6D *)
  module Cell_frequency_link = struct

    let name = "cell_frequency_link_descriptor"

    let rec decode_second_loop off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| cell_id_ext : 8
              ; trans_freq  : 32 : save_offset_to (off_1)
              ; rest        : -1 : save_offset_to (off_2), bitstring
              |} ->
              let nodes =
                [ to_node ~offset:off 8 "cell_id_extension" (Dec (Int cell_id_ext))
                ; to_node ~offset:(off + off_1) 32 "transposer_frequency" (Dec (Uint32 trans_freq)) ]
              in
              let name = Printf.sprintf "Subcell %s" (string_of_int cell_id_ext) in
              let node = to_node ~offset:off 40 name (List nodes) in
              node :: decode_second_loop (off + off_2) rest

    let rec decode_first_loop off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| cell_id     : 16
              ; frequency   : 32 : save_offset_to (off_1)
              ; loop_length : 8  : save_offset_to (off_2)
              ; snd_loop    : loop_length * 8 : save_offset_to (off_3), bitstring
              ; rest        : -1 : save_offset_to (off_4), bitstring
              |} ->
              let name  = Printf.sprintf "Cell %s" (string_of_int cell_id) in
              let snd_loop = decode_second_loop (off + off_3) snd_loop in
              let nodes =
                [ to_node ~offset:off 16 "cell_id" (Dec (Int cell_id))
                ; to_node ~offset:(off + off_1) 32 "frequency" (Dec (Uint32 frequency))
                ; to_node ~offset:(off + off_2) 8 "subcell_info_loop_length" (Dec (Int loop_length))
                ; to_node ~offset:(off + off_3) (loop_length * 8) "Subcells" (List snd_loop) ]
              in
              let real_length = 56 + loop_length * 8 in
              let node = to_node ~offset:off real_length name (List nodes) in
              node :: decode_first_loop (off + off_4) rest

    let decode bs off = decode_first_loop off bs

  end

  (* 0x6E *)
  module Announcement_support = struct

    let name = "announcement_support_descriptor"

    let parse_ann_type ann =
      match ann with
      | 0 -> "Emergency alarm"
      | 1 -> "Road Traffic flash"
      | 2 -> "Public Transport flash"
      | 3 -> "Warning message"
      | 4 -> "News flash"
      | 5 -> "Weather flash"
      | 6 -> "Event announcement"
      | 7 -> "Personal call"
      | _ -> "Reserved for future use"

    let parse_ref_type rf =
      match rf with
      | 0 -> "Announcement is broadcast in the usual audio stream of the service"
      | 1 -> "Announcement is broadcast in a separate audio stream that \
              is part of the service"
      | 2 -> "Announcement is broadcast by means of a different service \
              within the same transport system"
      | 3 -> "Announcement is broadcast by means of a different service \
              within a different transport stream"
      | _ -> "Reserved for future use"

    let rec f off x =
      if Bitstring.length x = 0 then []
      else match%bitstring x with
           | {| ann_type             : 4
              ; rfu                  : 1  : save_offset_to (off_1)
              ; 0x01 | 0x02 | 0x03   : 3  : save_offset_to (off_2)
              ; original_network_id  : 16 : save_offset_to (off_3)
              ; transport_stream_id  : 16 : save_offset_to (off_4)
              ; service_id           : 16 : save_offset_to (off_5)
              ; component_tag        : 8  : save_offset_to (off_6)
              ; rest                 : -1 : save_offset_to (off_7), bitstring
              |} ->
              let ann_typ = parse_ann_type ann_type in
              let ref_typ = parse_ref_type 1 in (* FIXME parsing reference type*)
              let nodes =
                [ to_node ~parsed:ann_typ ~offset:off 4 "announcement_type" (Bits (Int ann_type))
                ; to_node ~offset:(off + off_1) 1  "reserved_future_use" (Bits (Bool rfu))
                ; to_node ~parsed:ref_typ ~offset:(off + off_2) 3
                    "reference_type" (Hex (Int 0x01)) (* FIXME parsing reference type*)
                ; to_node ~offset:(off + off_3) 16 "original_network_id" (Hex (Int original_network_id))
                ; to_node ~offset:(off + off_4) 16 "transport_stream_id" (Hex (Int transport_stream_id))
                ; to_node ~offset:(off + off_5) 16 "service_id" (Hex (Int service_id))
                ; to_node ~offset:(off + off_6) 8  "component_tag" (Hex (Int component_tag)) ]
              in
              nodes @ f (off + off_7) rest
           | {| ann_type        : 4
              ; rfu             : 1  : save_offset_to (off_1)
              ; reference_type  : 3  : save_offset_to (off_2)
              ; rest            : -1 : save_offset_to (off_3), bitstring
              |} ->
              let ann_typ = parse_ann_type ann_type in
              let ref_typ = parse_ref_type reference_type in
              let nodes =
                [ to_node ~parsed:ann_typ ~offset:off 4 "announcement_type" (Bits (Int ann_type))
                ; to_node ~offset:(off + off_1) 1  "reserved_future_use" (Bits (Bool rfu))
                ; to_node ~parsed:ref_typ ~offset:(off + off_2) 3
                    "reference_type" (Hex (Int reference_type)) ]
              in
              let type_name = (Printf.sprintf "Type %s" (string_of_int ann_type)) ^ ann_typ in
              let node = to_node ~offset:off 64 type_name (List nodes) in
              node :: f (off + off_3) rest

    let decode bs off =
      match%bitstring bs with
      | {| ann_support_ind : 16
         ; rest            : -1 : save_offset_to (off_1), bitstring
         |} ->
         let node = to_node ~offset:off 16 "announcement_support_indicator" (Hex (Int ann_support_ind))
         in
         node :: f (off + off_1) rest

  end

  (* 0x6F *)
  module Application_signalling = struct

    (* refers to ETSI TS 102 809 *)

    let name = "application_signalling_descriptor"

    let rec f bs off =
      if Bitstring.length bs = 0 then []
      else match%bitstring bs with
           | {| rfu_1       : 1
              ; app_type    : 15 : save_offset_to (off_1)
              ; rfu_2       : 3  : save_offset_to (off_2)
              ; ait_ver_num : 5  : save_offset_to (off_3)
              ; rest        : -1 : save_offset_to (off_4), bitstring
              |} ->
              let node_list =
                [ to_node ~offset:off 1 "reserved_future_use" (Bits (Bool rfu_1))
                ; to_node ~offset:(off + off_1) 15 "application_type" (Hex (Int app_type))
                ; to_node ~offset:(off + off_2) 3  "reserved_future_use" (Bits (Int rfu_2))
                ; to_node ~offset:(off + off_3) 5  "AIT_version_number" (Dec (Int ait_ver_num)) ]
              in
              let node = [ to_node ~offset:off 24 "application signalling" (List node_list) ] in
              node @ f rest (off + off_4)

    let decode bs off = f bs off

  end

  (* 0x70 *)
  module Adaptation_field_data = struct

    let name = "adaptation_field_data_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| adapt_field_data_id : 8
         |} ->
         [ to_node ~offset:off 8 "adaptation_field_data_identifier" (Hex (Int adapt_field_data_id))]

  end

  (* 0x71 *)
  module Service_identifier = struct

    (* refers to ETSI TS 102 812 *)

    let name = "service_identifier_descriptor"

    let decode bs off = parse_bytes ~offset:off bs "textual_service_identifier_bytes"

  end

  (* 0x72 *)
  module Service_availability = struct

    let name = "service_availability_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| availability_flag : 1
         ; reserved          : 7  : save_offset_to (off_1)
         ; rest              : -1 : save_offset_to (off_2), bitstring
         |} ->
         let nodes =
           [ to_node ~offset:off 1 "availability_flag" (Bits (Bool availability_flag))
           ; to_node ~offset:(off + off_1) 7 "reserved" (Bits (Int reserved)) ]
         in
         nodes @ parse_bytes ~bytes:2 ~offset:(off + off_2) rest "cell_id"

  end

  (* 0x73 *)
  module Default_authority = struct

    (* ETSI TS 102 323 *)

    let name = "default_authority_descriptor"

    let decode bs off = parse_bytes ~offset:off bs "default_authority_byte"

  end

  (* 0x74 *)
  module Related_content = struct

    (* ETSI TS 102 323  *)

    let name = "related_content_descriptor"
    (* DO NOT CHANGE THIS IS LEGIT SEE ETSI TS 102 323 *)
    let decode _ _ = []

  end

  (* 0x75 *)
  module TVA_id = struct

    (* ETSI TS 102 323 *)

    let name = "TVA_id_descriptor"

    let rec f bs off =
      if Bitstring.length bs = 0 then []
      else match%bitstring bs with
           | {| tva_id   : 16
              ; reserved : 5  : save_offset_to (off_1)
              ; status   : 3  : save_offset_to (off_2)
              ; rest     : -1 : save_offset_to (off_3), bitstring
              |} ->
              let node_list =
                [ to_node ~offset:off 16 "TVA_id" (Dec (Int tva_id))
                ; to_node ~offset:(off + off_1) 5 "reserved" (Bits (Int reserved))
                ; to_node ~offset:(off + off_2) 3 "running_status" (Dec (Int status)) ]
              in
              let tva_name = Printf.sprintf "TVA_id %s" (string_of_int tva_id) in
              let node = to_node ~offset:off 24 tva_name (List node_list) in
              node :: f rest (off + off_3)

    let decode bs off = f bs off

  end

  (* 0x76 *)
  module Content_identifier = struct

    (* ETSI TS 102 323  page 101 *)

    let name = "content_identifier_descriptor"

    let rec f bs off =
      if Bitstring.length bs = 0 then []
      else match%bitstring bs with
           | {| crid_type   : 6
              ; 00          : 2  : save_offset_to (off_1)
              ; crid_length : 8  : save_offset_to (off_2)
              ; crid        : crid_length * 8 : save_offset_to (off_3), bitstring
              ; rest        : -1 : save_offset_to (off_4), bitstring
              |} ->
              let crid = match Text_decoder.decode @@ Bitstring.to_cstruct crid with
                | Ok s    -> s
                | Error _ -> "Unable to decode"
              in
              let nodes =
                [ to_node ~offset:off 6 "crid_type" (Dec (Int crid_type))
                ; to_node ~offset:(off + off_1) 2 "crid_location" (Bits (Int 00))
                ; to_node ~offset:(off + off_2) 8 "crid_length" (Dec (Int crid_length))
                ; to_node ~offset:(off + off_3) (crid_length * 8) "crid" (String crid) ]
              in
              let real_length = 16 + crid_length * 8 in
              let node = to_node ~offset:off real_length crid (List nodes) in
              node :: f rest (off + off_4)
           | {| crid_type   : 6
              ; 01          : 2  : save_offset_to (off_1)
              ; crid_ref    : 16 : save_offset_to (off_2)
              ; rest        : -1 : save_offset_to (off_3), bitstring
              |} ->
              let nodes =
                [ to_node ~offset:off 6 "crid_type" (Dec (Int crid_type))
                ; to_node ~offset:(off + off_1) 2 "crid_location" (Bits (Int 00))
                ; to_node ~offset:(off + off_2) 16 "crid_ref" (Dec (Int crid_ref)) ]
              in
              let node = to_node ~offset:off 24 "identifier" (List nodes) in
              node :: f rest (off + off_3)
           | {| crid_type     : 6
              ; crid_location : 2  : save_offset_to (off_1)
              ; rest          : -1 : save_offset_to (off_2), bitstring
              |} ->
              let nodes =
                [ to_node ~offset:off 6 "crid_type" (Dec (Int crid_type))
                ; to_node ~offset:(off + off_1) 2 "crid_location" (Bits (Int crid_location)) ]
              in
              let node = to_node ~offset:off 8 "identifier" (List nodes) in
              node :: f rest (off + off_2)

    let decode bs off = f bs off

  end

  (* 0x77 *)
  module Time_slice_fec_id = struct

    (* ETSI EN 301 192 *)

    let name = "time_slice_fec_identifier_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| time_slicing      : 1
         ; mpe_fec           : 2  : save_offset_to (off_1)
         ; rfu               : 2  : save_offset_to (off_2)
         ; frame_size        : 3  : save_offset_to (off_3)
         ; max_burst_dur     : 8  : save_offset_to (off_4)
         ; max_aver_rate     : 4  : save_offset_to (off_5)
         ; time_slice_fec_id : 4  : save_offset_to (off_6)
         ; rest              : -1 : save_offset_to (off_7), bitstring
         |} ->
         let id_selector = match Text_decoder.decode @@ Bitstring.to_cstruct rest with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         [ to_node ~offset:off 1 "time_slicing" (Bits (Bool time_slicing))
         ; to_node ~offset:(off + off_1) 2 "mpe_fec" (Dec (Int mpe_fec))
         ; to_node ~offset:(off + off_2) 2 "reserved_for_future_use" (Bits (Int rfu))
         ; to_node ~offset:(off + off_3) 3 "frame_size" (Dec (Int frame_size))
         ; to_node ~offset:(off + off_4) 8 "max_burst_duration" (Dec (Int max_burst_dur))
         ; to_node ~offset:(off + off_5) 4 "max_average_rate" (Dec (Int max_aver_rate))
         ; to_node ~offset:(off + off_6) 4 "time_slice_fec_id" (Dec (Int time_slice_fec_id))
         ; to_node ~offset:(off + off_7) (Bitstring.length rest) "id_selector" (String id_selector) ]

  end

  (* 0x78 *)
  module ECM_repetition_rate = struct

    (* ETSI EN 301 192 *)

    let name = "ECM_repetition_rate_descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| ca_system_id        : 16
         ; ecm_repetition_rate : 16 : save_offset_to (off_1)
         ; rest                : -1 : save_offset_to (off_2), bitstring
         |} ->
         let private_data = match Text_decoder.decode @@ Bitstring.to_cstruct rest with
           | Ok s    -> s
           | Error _ -> "Unable to decode"
         in
         [ to_node ~offset:off 16 "CA_system_ID" (Dec (Int ca_system_id))
         ; to_node ~offset:(off + off_1) 16 "ECM repetition rate" (Dec (Int ecm_repetition_rate))
         ; to_node ~offset:(off + off_2) (Bitstring.length rest) "private_data" (String private_data) ]

  end

  (* 0x79 *)
  module S2_satellite_delivery = struct

    let name = "S2_satellite_delivery_system_descriptor"

    let f ~sss ~misf off x =
        match sss, misf with
        | true, true -> (match%bitstring x with
                          {| reserved : 6
                           ; ssi      : 18 : save_offset_to (off_1)
                           ; isi      : 8  : save_offset_to (off_2)
                           |} ->
                          [ to_node ~offset:off 6 "Reserved" (Bits (Int reserved))
                          ; to_node ~offset:(off + off_1) 18 "scrambling_sequence_index" (Dec (Int ssi))
                          ; to_node ~offset:(off + off_2) 8 "input_stream_identifier" (Dec (Int isi)) ])
        | true, false -> (match%bitstring x with
                          {| reserved : 6
                           ; ssi      : 18 : save_offset_to (off_1)
                           |} ->
                          [ to_node ~offset:off 6 "Reserved" (Bits (Int reserved))
                          ; to_node ~offset:(off + off_1) 18 "scrambling_sequence_index" (Dec (Int ssi))])
        | false, true -> (match%bitstring x with
                          {| isi      : 8
                           |} ->
                          [ to_node ~offset:off 8 "input_stream_identifier" (Dec (Int isi)) ])
        | false, false -> []


    let decode bs off =
      match%bitstring bs with
      | {| sss       : 1
         ; misf      : 1  : save_offset_to (off_1)
         ; backw_ind : 1  : save_offset_to (off_2)
         ; rfu       : 5  : save_offset_to (off_3)
         ; rest      : -1 : save_offset_to (off_4), bitstring
         |} ->
         let nodes =
         [ to_node ~offset:off 1 "scrambling_sequence_selector" (Bits (Bool sss))
         ; to_node ~offset:(off + off_1) 1 "multiple_input_stream_flag" (Bits (Bool misf))
         ; to_node ~offset:(off + off_2) 1 "backwards_compatibility_indicator" (Bits (Bool backw_ind))
         ; to_node ~offset:(off + off_3) 5 "reserved_future_use" (Bits (Int rfu)) ]
         in
         nodes @ (f ~sss ~misf (off + off_4) rest)
  end

  (* 0x7A *)
  module Enhanced_AC3 = struct

    let name = "enhanced_AC-3_descriptor"

    (* TODO needs a better method of parsing, ~= 128 variants *)

    let decode _ _ = []

  end

  (* 0x7B *)
  module DTS = struct

    let name = "DTS descriptor"
    (* TODO *)

    let decode _ _ = []

  end

  (* 0x7C *)
  module AAC = struct

    let name = "AAC descriptor"
    (* TODO *)

    let decode _ _ = []

  end

  (* 0x7D *)
  module XAIT_location = struct

    let name = "XAIT location descriptor"
    (* TODO *)

    let decode _ _ = []

  end

  (* 0x7E *)
  module FTA_content_management = struct

    let name = "FTA_content_management_descriptor"

    let parse_access access =
      match access with
      | 0 -> "Redistribution over the Internet is enabled."
      | 1 -> "Redistribution over the Internet is enabled but \
              only within a managed domain."
      | 2 -> "Redistribution over the Internet is enabled but \
              only within a managed domain and after a certain \
              short period of time (e.g. 24 hours)."
      | 3 -> "Redistribution over the Internet is not allowed with \
              the following exception: Redistribution over the Internet \
              within a managed domain is enabled after a specified long \
              (possibly indefinite) period of time."
      | _ -> assert false

    let decode bs off =
      match%bitstring bs with
      | {| user_defined    : 1
         ; rfu             : 3 : save_offset_to (off_1)
         ; do_not_scramble : 1 : save_offset_to (off_2)
         ; cont_remote_acc : 2 : save_offset_to (off_3)
         ; do_not_apply    : 1 : save_offset_to (off_4)
         |} ->
         let parsed = parse_access cont_remote_acc in
         [ to_node ~offset:off 1 "user_defined" (Bits (Bool user_defined))
         ; to_node ~offset:(off + off_1) 3 "reserved_future_use" (Bits (Int rfu))
         ; to_node ~offset:(off + off_2) 1 "do_not_scramble" (Bits (Bool do_not_scramble))
         ; to_node ~parsed ~offset:(off + off_3) 2 "control_remote_access_over_internet"
             (Bits (Int cont_remote_acc))
         ; to_node ~offset:(off + off_4) 1 "do_not_apply_revocation" (Bits (Bool do_not_apply)) ]

  end

  (* 0x7F *)
  module Extension_2 = struct

    let name = "Extension descriptor"

    let decode bs off =
      match%bitstring bs with
      | {| desc_tag_ext : 8
         ; rest         : -1 : save_offset_to (off_1), bitstring
         |} ->
         let node = to_node ~offset:off 8 "descriptor_tag_extension" (Hex (Int desc_tag_ext)) in
         node :: parse_bytes ~offset:(off + off_1) rest "selector_byte"

  end

  (* 0xFF *)
  module Forbidden = struct

    let name = "forbidden"

    let decode _ _ = []

  end

  (*0x80 to 0xFE*)
  module User_defined = struct

    let name = "user defined"

    let decode _ _ = []

  end

  module Unknown = struct

    type t = string [@@deriving yojson]

    let name = "unknown descriptor"

    let decode _ _ = []

  end

  type t =
    { tag     : int
    ; length  : int
    ; name    : string
    ; content : parsed
    } [@@deriving yojson]

  let decode tag length body off =
    let name, content =
      (match tag with
       | 0x02 -> Video_stream.name, Video_stream.decode body off
       | 0x03 -> Audio_stream.name, Audio_stream.decode body off
       | 0x04 -> Hierarchy.name, Hierarchy.decode body off
       | 0x05 -> Registration.name, Registration.decode body off
       | 0x06 -> Data_stream_alignment.name, Data_stream_alignment.decode body off
       | 0x07 -> Target_background_grid.name,Target_background_grid.decode body off
       | 0x08 -> Video_window.name, Video_window.decode body off
       | 0x09 -> CA.name, CA.decode body off
       | 0x0A -> ISO_639_language.name, ISO_639_language.decode body off
       | 0x0B -> System_clock.name, System_clock.decode body off
       | 0x0C -> Multiplex_buffer_utilization.name, Multiplex_buffer_utilization.decode body off
       | 0x0D -> Copyright.name, Copyright.decode body off
       | 0x0E -> Maximum_bitrate.name, Maximum_bitrate.decode body off
       | 0x0F -> Private_data_indicator.name, Private_data_indicator.decode body off
       | 0x10 -> Smoothing_buffer.name, Smoothing_buffer.decode body off
       | 0x11 -> STD_descriptor.name, STD_descriptor.decode body off
       | 0x12 -> IBP_descriptor.name, IBP_descriptor.decode body off
       | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 | 0x18 | 0x19 | 0x1A -> Unknown.name, Unknown.decode body off
       | 0x1B -> MPEG4_video.name, MPEG4_video.decode body off
       | 0x1C -> MPEG4_audio.name, MPEG4_audio.decode body off
       | 0x1D -> IOD.name, IOD.decode body off
       | 0x1E -> SL.name, SL.decode body off
       | 0x1F -> FMC.name, FMC.decode body off
       | 0x20 -> ES_ID.name, ES_ID.decode body off
       | 0x21 -> Mux_code.name, Mux_code.decode body off
       | 0x22 -> Fmx_buffer_size.name, Fmx_buffer_size.decode body off
       | 0x23 -> Multiplex_buffer.name, Multiplex_buffer.decode body off
       | 0x24 -> Content_labelling.name, Content_labelling.decode body off
       | 0x25 -> Metadata_pointer.name, Metadata_pointer.decode body off
       | 0x26 -> Metadata.name, Metadata.decode body off
       | 0x27 -> Metadata_STD.name, Metadata_STD.decode body off
       | 0x28 -> AVC_video.name, AVC_video.decode body off
       | 0x29 -> IPMP.name, IPMP.decode body off
       | 0x2A -> AVC_HRD.name, AVC_HRD.decode body off
       | 0x2B -> MPEG2_AAC_audio.name, MPEG2_AAC_audio.decode body off
       | 0x2C -> Flex_mux_timing.name, Flex_mux_timing.decode body off
       | 0x2D -> MPEG4_text.name, MPEG4_text.decode body off
       | 0x2E -> MPEG4_audio_ext.name, MPEG4_audio_ext.decode body off
       | 0x2F -> Aux_video_stream.name, Aux_video_stream.decode body off
       | 0x30 -> SVC_ext.name, SVC_ext.decode body off
       | 0x31 -> MVC_ext.name, MVC_ext.decode body off
       | 0x32 -> J2K_video.name, J2K_video.decode body off
       | 0x33 -> MVC_operation_point.name, MVC_operation_point.decode body off
       | 0x34 -> MPEG2_stereo_format.name, MPEG2_stereo_format.decode body off
       | 0x35 -> Stereo_program_info.name, Stereo_program_info.decode body off
       | 0x36 -> Stereo_video_info.name, Stereo_video_info.decode body off
       | 0x37 -> Transport_profile.name, Transport_profile.decode body off
       | 0x38 -> HEVC_video.name, HEVC_video.decode body off
       | 0x39 | 0x3A | 0x3B | 0x3C | 0x3D | 0x3E -> Unknown.name, Unknown.decode body off
       | 0x3F -> Extension_1.name, Extension_1.decode body off
       | 0x40 -> Network_name.name, Network_name.decode body off
       | 0x41 -> Service_list.name, Service_list.decode body off
       | 0x42 -> Stuffing.name, Stuffing.decode body off
       | 0x43 -> Satellite_delivery.name, Satellite_delivery.decode body off
       | 0x44 -> Cable_delivery.name, Cable_delivery.decode body off
       | 0x45 -> VBI_data.name, VBI_data.decode body off
       | 0x46 -> VBI_teletext.name, VBI_teletext.decode body off
       | 0x47 -> Bouquet_name.name, Bouquet_name.decode body off
       | 0x48 -> Service.name, Service.decode body off
       | 0x49 -> Country_availability.name, Country_availability.decode body off
       | 0x4A -> Linkage.name, Linkage.decode body off
       | 0x4B -> NVOD_reference.name, NVOD_reference.decode body off
       | 0x4C -> Time_shifted_service.name, Time_shifted_service.decode body off
       | 0x4D -> Short_event.name, Short_event.decode body off
       | 0x4E -> Extended_event.name, Extended_event.decode body off
       | 0x4F -> Time_shifted_event.name, Time_shifted_event.decode body off
       | 0x50 -> Component.name, Component.decode body off
       | 0x51 -> Mosaic.name, Mosaic.decode body off
       | 0x52 -> Stream_identifier.name, Stream_identifier.decode body off
       | 0x53 -> CA_identifier.name, CA_identifier.decode body off
       | 0x54 -> Content.name, Content.decode body off
       | 0x55 -> Parental_rating.name, Parental_rating.decode body off
       | 0x56 -> Teletext.name, Teletext.decode body off
       | 0x57 -> Telephone.name, Telephone.decode body off
       | 0x58 -> Local_time_offset.name, Local_time_offset.decode body off
       | 0x59 -> Subtitling.name, Subtitling.decode body off
       | 0x5A -> Terrestrial_delivery.name, Terrestrial_delivery.decode body off
       | 0x5B -> Multilingual_network.name, Multilingual_network.decode body off
       | 0x5C -> Multilingual_bouquet.name, Multilingual_bouquet.decode body off
       | 0x5D -> Multilinguql_service.name, Multilinguql_service.decode body off
       | 0x5E -> Multilingual_component.name, Multilingual_component.decode body off
       | 0x5F -> Private_data_specifier.name, Private_data_specifier.decode body off
       | 0x60 -> Service_move.name, Service_move.decode body off
       | 0x61 -> Short_smoothing_buffer.name, Short_smoothing_buffer.decode body off
       | 0x62 -> Frequency_list.name, Frequency_list.decode body off
       | 0x63 -> Partial_transport_stream.name, Partial_transport_stream.decode body off
       | 0x64 -> Data_broadcast.name, Data_broadcast.decode body off
       | 0x65 -> Scrambling.name, Scrambling.decode body off
       | 0x66 -> Data_broadcast_id.name, Data_broadcast_id.decode body off
       | 0x67 -> Transport_stream.name, Transport_stream.decode body off
       | 0x68 -> DSNG.name, DSNG.decode body off
       | 0x69 -> PDC.name, PDC.decode body off
       | 0x6A -> AC3.name, AC3.decode body off
       | 0x6B -> Ancillary_data.name, Ancillary_data.decode body off
       | 0x6C -> Cell_list.name, Cell_list.decode body off
       | 0x6D -> Cell_frequency_link.name, Cell_frequency_link.decode body off
       | 0x6E -> Announcement_support.name, Announcement_support.decode body off
       | 0x6F -> Application_signalling.name, Application_signalling.decode body off
       | 0x70 -> Adaptation_field_data.name, Adaptation_field_data.decode body off
       | 0x71 -> Service_identifier.name, Service_identifier.decode body off
       | 0x72 -> Service_availability.name, Service_availability.decode body off
       | 0x73 -> Default_authority.name, Default_authority.decode body off
       | 0x74 -> Related_content.name, Related_content.decode body off
       | 0x75 -> TVA_id.name, TVA_id.decode body off
       | 0x76 -> Content_identifier.name, Content_identifier.decode body off
       | 0x77 -> Time_slice_fec_id.name, Time_slice_fec_id.decode body off
       | 0x78 -> ECM_repetition_rate.name, ECM_repetition_rate.decode body off
       | 0x79 -> S2_satellite_delivery.name, S2_satellite_delivery.decode body off
       | 0x7A -> Enhanced_AC3.name, Enhanced_AC3.decode body off
       | 0x7B -> DTS.name, DTS.decode body off
       | 0x7C -> AAC.name, AAC.decode body off
       | 0x7D -> XAIT_location.name, XAIT_location.decode body off
       | 0x7E -> FTA_content_management.name, FTA_content_management.decode body off
       | 0x7F -> Extension_2.name, Extension_2.decode body off
       | 0xFF -> Forbidden.name, Forbidden.decode body off
       | x when x > 0x7F && x < 0xFF -> User_defined.name, User_defined.decode body off
       | _  -> Unknown.name, Unknown.decode body off) in
     to_node ~offset:off length name (List content)

  let of_bitstring off bs =
    match%bitstring bs with
    | {| tag    : 8
       ; length : 8
       ; body   : length * 8 : save_offset_to (off_1), bitstring
       ; rest   : -1 : save_offset_to (off_2), bitstring
       |} -> (decode tag length body (off + off_1)),rest, (off + off_2)

  end

module Table_common = struct

  let parse_header bs =
    match%bitstring bs with
    | {| table_id       : 8
       ; ssi            : 1  : save_offset_to (off_1)
       ; rfu            : 1  : save_offset_to (off_2)
       ; reserved_1     : 2  : save_offset_to (off_3)
       ; section_length : 12 : save_offset_to (off_4)
       |} ->
       [ to_node ~offset:0     8  "table_id"        (Hex (Int table_id))
       ; to_node ~offset:off_1 1  "section_syntax_indicator" (Bits (Bool ssi))
       ; to_node ~offset:off_2 1  "'0'"             (Bits (Bool rfu))
       ; to_node ~offset:off_3 2  "reserved"        (Hex (Int reserved_1))
       ; to_node ~offset:off_4 12 "section_length"  (Dec (Int section_length))
       ]

  let rec parse_descriptors = fun off x ->
    if Bitstring.length x = 0 then []
    else let descr,rest, off = Descriptor.of_bitstring off x in
         descr :: parse_descriptors off rest

  let rec parse_ts = fun off x ->
    if Bitstring.length x = 0 then []
    else
      match%bitstring x with
      | {| ts_id       : 16
         ; on_id       : 16 : save_offset_to (off_1)
         ; rfu         : 4  : save_offset_to (off_2)
         ; length      : 12 : save_offset_to (off_3)
         ; descriptors : length * 8 : save_offset_to (off_4), bitstring
         ; rest        : -1 : save_offset_to (off_5), bitstring
         |} ->
         let descriptors = parse_descriptors (off + off_4) descriptors in
         let nodes =
           [ to_node ~offset:off           16 "transport_stream_id" (Dec (Int ts_id))
           ; to_node ~offset:(off_1 + off) 16 "original_network_id" (Dec (Int on_id))
           ; to_node ~offset:(off_2 + off) 4  "reserved_future_use" (Bits (Int rfu))
           ; to_node ~offset:(off_3 + off) 12 "transport_descriptors_length" (Dec (Int length))
           ; to_node ~offset:(off_4 + off) (length * 8) "descriptors" (List descriptors)
           ] in
         let ts_name = Printf.sprintf "transport stream %d" ts_id in
         let node = to_node ~offset:off (parsed_length nodes) ts_name (List nodes) in
         node :: parse_ts (off_5 + off) rest

end

module PAT = struct

  open Table_common
  open Bitstring

  let rec parse_programs = fun off x ->
    let to_name = function
      | 0 -> "network_PID"
      | _ -> "program_map_PID" in
    if length x = 0 then []
    else
      match%bitstring x with
      | {| id       : 16
         ; reserved : 3  : save_offset_to (off_1)
         ; pid      : 13 : save_offset_to (off_2)
         ; rest     : -1 : save_offset_to (off_3), bitstring
         |} ->
         let nodes =
           [ to_node ~offset:off 16 "program_number" (Hex (Int id))
           ; to_node ~offset:(off_1 + off) 3 "reserved" (Bits (Int reserved))
           ; to_node ~offset:(off_2 + off) 13 (to_name id) (Hex (Int pid))
           ] in
         let name = match id with

           | 0 -> "network"
           | x -> Printf.sprintf "program %d" x in
         let node = to_node ~offset:off (parsed_length nodes) name (List nodes) in
         node :: parse_programs (off_3 + off) rest

  let parse buf =
    let bs = bitstring_of_string buf in
    let progs_length off = length bs - off - 8 - 32 in
    match%bitstring bs with
    | {| header           : 24 : bitstring
       ; ts_id            : 16 : save_offset_to (off_1)
       ; reserved         : 2  : save_offset_to (off_2)
       ; version_number   : 5  : save_offset_to (off_3)
       ; current_next_ind : 1  : save_offset_to (off_4)
       ; section_number   : 8  : save_offset_to (off_5)
       ; last_section_num : 8  : save_offset_to (off_6)
       ; programs         : progs_length off_6 : save_offset_to (off_7), bitstring
       ; crc32            : 32 : save_offset_to (off_8)
       |} ->
       let progs  = parse_programs off_7 programs in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 16 "transport_stream_id" (Hex (Int ts_id))
         ; to_node ~offset:off_2 2 "reserved" (Bits (Int reserved))
         ; to_node ~offset:off_3 5 "version_number" (Dec (Int version_number))
         ; to_node ~offset:off_4 1 "current_next_indicator" (Bits (Bool current_next_ind))
         ; to_node ~offset:off_5 8 "section_number" (Dec (Int section_number))
         ; to_node ~offset:off_6 8 "last_section_number" (Dec (Int last_section_num))
         ; to_node ~offset:off_7 (progs_length off_6) "programs" (List progs)
         ; to_node ~offset:off_8 32 "CRC_32" (Hex (Uint32 crc32))
         ] in
       header @ nodes

end

module PMT = struct
  open Table_common

  let rec parse_streams = fun off x ->
    if Bitstring.length x = 0 then []
    else
      (match%bitstring x with
       | {| stream_type    : 8
          ; reserved_1     : 3  : save_offset_to (off_1)
          ; pid            : 13 : save_offset_to (off_2)
          ; reserved_2     : 4  : save_offset_to (off_3)
          ; length         : 12 : save_offset_to (off_4)
          ; descriptors    : length * 8 : save_offset_to (off_5), bitstring
          ; rest           : -1 : save_offset_to (off_6), bitstring
          |} ->
          let dscrs = parse_descriptors (off + off_5) descriptors in
          let nodes =
            [ to_node ~offset:off           8 "stream_type" (Hex (Int stream_type))
            ; to_node ~offset:(off_1 + off) 3 "reserved" (Bits (Int reserved_1))
            ; to_node ~offset:(off_2 + off) 13 "elementary_PID" (Hex (Int pid))
            ; to_node ~offset:(off_3 + off) 4 "reserved" (Bits (Int reserved_2))
            ; to_node ~offset:(off_4 + off) 12 "ES_info_length" (Hex (Int length))
            ; to_node ~offset:(off_5 + off) (length * 8) "descriptors" (List dscrs)
            ] in
          let stream_name = Printf.sprintf "stream %d" pid in
          let node = to_node ~offset:off (parsed_length nodes) stream_name (List nodes) in
          node :: parse_streams (off_6 + off) rest)

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    let streams_len prog_length off = Bitstring.length bs - (prog_length * 8) - off - 32 in
    match%bitstring bs with
    | {| header           : 24 : bitstring
       ; program_number   : 16 : save_offset_to (off_1)
       ; reserved_1       : 2  : save_offset_to (off_2)
       ; version_number   : 5  : save_offset_to (off_3)
       ; current_next_ind : 1  : save_offset_to (off_4)
       ; section_number   : 8  : save_offset_to (off_5)
       ; last_section_num : 8  : save_offset_to (off_6)
       ; reserved_2       : 3  : save_offset_to (off_7)
       ; pcr_pid          : 13 : save_offset_to (off_8)
       ; reserved_3       : 4  : save_offset_to (off_9)
       ; length           : 12 : save_offset_to (off_10)
       ; descriptors      : length * 8 : bitstring, save_offset_to (off_11)
       ; streams          : streams_len length off_11 : bitstring, save_offset_to (off_12)
       ; crc32            : 32 : save_offset_to (off_13)
       |} ->
       let dscrs   = parse_descriptors off_11 descriptors in
       let header  = parse_header header in
       let streams = parse_streams off_12 streams in
       let nodes   =
         [ to_node ~offset:off_1 16 "program_number" (Dec (Int program_number))
         ; to_node ~offset:off_2 2 "reserved" (Bits (Int reserved_1))
         ; to_node ~offset:off_3 5 "version_number" (Dec (Int version_number))
         ; to_node ~offset:off_4 1 "current_next_indicator" (Bits (Bool current_next_ind))
         ; to_node ~offset:off_5 8 "section_number" (Dec (Int section_number))
         ; to_node ~offset:off_6 8 "last_section_number" (Dec (Int last_section_num))
         ; to_node ~offset:off_7 3 "reserved" (Bits (Int reserved_2))
         ; to_node ~offset:off_8 13 "PCR_PID" (Bits (Int pcr_pid))
         ; to_node ~offset:off_9 4 "reserved" (Bits (Int reserved_3))
         ; to_node ~offset:off_10 12 "program_info_length" (Dec (Int length))
         ; to_node ~offset:off_11 (length * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_12 (streams_len length off_11) "streams" (List streams)
         ; to_node ~offset:off_13 32 "CRC_32" (Hex (Uint32 crc32))
         ]
       in
       header @ nodes

end

module CAT = struct

  open Table_common
  open Bitstring

  let parse buf =
    let bs = bitstring_of_string buf in
    let dscrs_length off = length bs - off - 8 -32 in
    match%bitstring bs with
    | {| header              : 24 : bitstring
       ; reserved            : 18 : save_offset_to (off_1)
       ; version_number      : 5  : save_offset_to (off_2)
       ; current_next_ind    : 1  : save_offset_to (off_3)
       ; section_number      : 8  : save_offset_to (off_4)
       ; last_section_number : 8  : save_offset_to (off_5)
       ; descriptors         : dscrs_length off_5 : save_offset_to (off_6), bitstring
       ; crc32               : 32 : save_offset_to (off_7)
       |} ->
       let dscrs  = parse_descriptors off_6 descriptors in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 18 "reserved" (Bits (Int reserved))
         ; to_node ~offset:off_2 5 "version_number" (Dec (Int version_number))
         ; to_node ~offset:off_3 1 "current_next_indicator" (Bits (Bool current_next_ind))
         ; to_node ~offset:off_4 8 "section_number" (Dec (Int section_number))
         ; to_node ~offset:off_5 8 "last_section_number" (Dec (Int last_section_number))
         ; to_node ~offset:off_6 (dscrs_length off_5) "descriptors" (List dscrs)
         ; to_node ~offset:off_7 32 "CRC_32" (Dec (Uint32 crc32))
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
    | {| header              : 24 : bitstring
       ; network_id          : 16 : save_offset_to (off_1)
       ; reserved            : 2  : save_offset_to (off_2)
       ; version_number      : 5  : save_offset_to (off_3)
       ; current_next_ind    : 1  : save_offset_to (off_4)
       ; section_number      : 8  : save_offset_to (off_5)
       ; last_section_number : 8  : save_offset_to (off_6)
       ; rfu_1               : 4  : save_offset_to (off_7)
       ; nw_desc_length      : 12 : save_offset_to (off_8)
       ; descriptors         : nw_desc_length * 8 : save_offset_to (off_9), bitstring
       ; rfu_2               : 4  : save_offset_to (off_10)
       ; ts_loop_len         : 12 : save_offset_to (off_11)
       ; transport_streams   : ts_loop_len * 8 : save_offset_to (off_12),bitstring
       ; crc32               : 32 : save_offset_to (off_13)
       |} ->
       let ts     = parse_ts off_12 transport_streams in
       let dscrs  = parse_descriptors off_9 descriptors in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 16 "network_id" (Hex (Int network_id))
         ; to_node ~offset:off_2 2 "reserved" (Hex (Int reserved))
         ; to_node ~offset:off_3 5 "version_number" (Dec (Int version_number))
         ; to_node ~offset:off_4 1 "current_next_indicator" (Bits (Bool current_next_ind))
         ; to_node ~offset:off_5 8 "section_number" (Dec (Int section_number))
         ; to_node ~offset:off_6 8 "last_section_number" (Dec (Int last_section_number))
         ; to_node ~offset:off_7 4 "reserved_future_use" (Hex (Int rfu_1))
         ; to_node ~offset:off_8 12 "network_desc_length" (Hex (Int nw_desc_length))
         ; to_node ~offset:off_9 (nw_desc_length * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_10 4 "reserved_future_use" (Hex (Int rfu_2))
         ; to_node ~offset:off_11 12 "transport_stream_loop_length" (Dec (Int ts_loop_len))
         ; to_node ~offset:off_12 (ts_loop_len * 8) "transport_streams" (List ts)
         ; to_node ~offset:off_13 32 "CRC_32" (Hex (Uint32 crc32))
         ]
       in
       header @ nodes
end

module BAT = struct
  open Table_common

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    match%bitstring bs with
    | {| header            : 24 : bitstring
       ; bouquet_id        : 16 : save_offset_to (off_1)
       ; reserved          : 2  : save_offset_to (off_2)
       ; version_number    : 5  : save_offset_to (off_3)
       ; current_next_ind  : 1  : save_offset_to (off_4)
       ; section_number    : 8  : save_offset_to (off_5)
       ; last_section_num  : 8  : save_offset_to (off_6)
       ; rfu_1             : 4  : save_offset_to (off_7)
       ; desc_len          : 12 : save_offset_to (off_8)
       ; descriptors       : desc_len * 8 : save_offset_to (off_9), bitstring
       ; rfu_2             : 4  : save_offset_to (off_10)
       ; ts_len            : 12 : save_offset_to (off_11)
       ; transport_streams : ts_len * 8 : save_offset_to (off_12), bitstring
       ; crc32             : 32 : save_offset_to (off_13)
       |} ->
       let ts     = parse_ts off_12 transport_streams in
       let dscrs  = parse_descriptors off_9 descriptors in
       let header = parse_header header in
       let nodes  =
         [ to_node ~offset:off_1 16 "bouquet_id" (Hex (Int bouquet_id))
         ; to_node ~offset:off_2 2 "reserved" (Bits (Int reserved))
         ; to_node ~offset:off_3 5 "version_number" (Dec (Int version_number))
         ; to_node ~offset:off_4 1 "current_next_indicator" (Bits (Bool current_next_ind))
         ; to_node ~offset:off_5 8 "section_number" (Dec (Int section_number))
         ; to_node ~offset:off_6 8 "last_section_number" (Dec (Int last_section_num))
         ; to_node ~offset:off_7 4 "reserved_future_use" (Bits (Int rfu_1))
         ; to_node ~offset:off_8 12 "bouquet_descriptors_length" (Dec (Int desc_len))
         ; to_node ~offset:off_9 (desc_len * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_10 4 "reserved_future_use" (Bits (Int rfu_2))
         ; to_node ~offset:off_11 12 "transport_stream_loop_length" (Dec (Int ts_len))
         ; to_node ~offset:off_12 (ts_len * 8) "transport_streams" (List ts)
         ; to_node ~offset:off_13 32 "CRC_32" (Hex (Uint32 crc32))
         ]
       in
       header @ nodes
end

module SDT = struct
  open Table_common

  let parse_status = function
    | 0 -> "undefined"
    | 1 -> "not running"
    | 2 -> "starts in a few seconds"
    | 3 -> "pausing"
    | 4 -> "running"
    | 5 -> "service off-air"
    | 6 | 7 -> "reserved for future use"
    | _ -> "status error"

  let rec parse_services = fun off x ->
    if Bitstring.length x = 0 then []
    else
      (match%bitstring x with
       | {| service_id     : 16
          ; rfu            : 6  : save_offset_to (off_1)
          ; schedule_flag  : 1  : save_offset_to (off_2)
          ; present_flag   : 1  : save_offset_to (off_3)
          ; running_status : 3  : save_offset_to (off_4)
          ; free_ca_mode   : 1  : save_offset_to (off_5)
          ; length         : 12 : save_offset_to (off_6)
          ; descriptors    : length * 8 : save_offset_to (off_7), bitstring
          ; rest           : -1 : save_offset_to (off_8), bitstring
          |} ->
          let dscrs = parse_descriptors (off + off_7) descriptors in
          let nodes =
            [ to_node ~offset:off            16 "service_id" (Hex (Int service_id))
            ; to_node ~offset:(off_1 + off)  6  "reserved_fuure_use" (Bits (Int rfu))
            ; to_node ~offset:(off_2 + off)  1  "EIT_schedule_flag" (Bits (Bool schedule_flag))
            ; to_node ~offset:(off_3 + off)  1  "EIT_present_following_flag" (Bits (Bool present_flag))
            ; to_node ~offset:(off_4 + off)  3  "runnning_status" (Hex (Int running_status))
            ; to_node ~offset:(off_5 + off)  1  "free_CA_mode" (Bits (Bool free_ca_mode))
            ; to_node ~offset:(off_6 + off)  12 "descriptors_loop_length" (Hex (Int length))
            ; to_node ~offset:(off_7 + off) (length * 8) "descriptors" (List dscrs)
            ]
          in
          let parsed = parse_status running_status in
          let service_name = Printf.sprintf "service %d" service_id in
          let node = to_node ~parsed ~offset:off (parsed_length nodes) service_name (List nodes) in
          node :: parse_services (off_8 + off) rest)

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    let services_length off = Bitstring.length bs - off - 8 - 32 in
    match%bitstring bs with
    | {| header           : 24 : bitstring
       ; ts_id            : 16 : save_offset_to (off_1)
       ; reserved         : 2  : save_offset_to (off_2)
       ; version_number   : 5  : save_offset_to (off_3)
       ; current_next_ind : 1  : save_offset_to (off_4)
       ; section_number   : 8  : save_offset_to (off_5)
       ; last_section_num : 8  : save_offset_to (off_6)
       ; on_id            : 16 : save_offset_to (off_7)
       ; rfu              : 8  : save_offset_to (off_8)
       ; services         : services_length off_8 : save_offset_to (off_9), bitstring
       ; crc32            : 32 : save_offset_to (off_10)
       |} ->
       let services = parse_services off_9 services in
       let header   = parse_header header in
       let nodes =
         [ to_node ~offset:off_1 16 "transport_stream_id" (Hex (Int ts_id))
         ; to_node ~offset:off_2 2 "reserved" (Hex (Int reserved))
         ; to_node ~offset:off_3 5 "version_number" (Dec (Int version_number))
         ; to_node ~offset:off_4 1 "current_next_indicator" (Bits (Bool current_next_ind))
         ; to_node ~offset:off_5 8 "section_number" (Dec (Int section_number))
         ; to_node ~offset:off_6 8 "last_section_number" (Dec (Int last_section_num))
         ; to_node ~offset:off_7 16 "original_network_id" (Hex (Int on_id))
         ; to_node ~offset:off_8 8 "reserved_future_use" (Hex (Int rfu))
         ; to_node ~offset:off_9 (services_length off_8) "services" (List services)
         ; to_node ~offset:off_10 32 "CRC_32" (Hex (Uint32 crc32))
         ]
       in
       header @ nodes
end

module EIT = struct

  open Table_common

  let parse_status = function
    | 0 -> "undefined"
    | 1 -> "not running"
    | 2 -> "starts in a few seconds"
    | 3 -> "pausing"
    | 4 -> "running"
    | 5 -> "service off-air"
    | 6 | 7 -> "reserved for future use"
    | _ -> "status error"

  let rec parse_events = fun off x ->
    if Bitstring.length x = 0 then []
    else
      (match%bitstring x with
       | {| event_id       : 16
          ; start_time     : 40 : save_offset_to (off_1), bitstring
          ; duration       : 24 : save_offset_to (off_2), bitstring
          ; running_status : 3  : save_offset_to (off_3)
          ; free_ca_mode   : 1  : save_offset_to (off_4)
          ; length         : 12 : save_offset_to (off_5)
          ; descriptors    : length * 8 : save_offset_to (off_6), bitstring
          ; rest           : -1 : save_offset_to (off_7), bitstring
          |} ->
          let dscrs = parse_descriptors (off + off_6) descriptors in
          let time = match parse_timestamp start_time with
            | Some x -> Time x
            | None   -> match%bitstring start_time with
                        | {| i : 40 |} -> Dec (Uint64  i) in
          let dur  = match parse_duration duration with
            | Some x -> Duration x
            | None   -> match%bitstring duration with
                        | {| i : 24 |} -> Dec (Uint i) in
          let parsed = parse_status running_status in
          let nodes =
            [ to_node ~offset:off           16 "event_id" (Hex (Int event_id))
            ; to_node ~offset:(off_1 + off) 40 "start_time" time
            ; to_node ~offset:(off_2 + off) 24 "duration" dur
            ; to_node ~offset:(off_3 + off) 3  "running_status" (Dec (Int running_status))
            ; to_node ~offset:(off_4 + off) 1  "free_CA_mode" (Bits (Bool free_ca_mode))
            ; to_node ~offset:(off_5 + off) 12 "decriptors_loop_length" (Dec (Int length))
            ; to_node ~offset:(off_6 + off) (length * 8) "descriptors" (List dscrs)
            ]
          in
          let event_name = Printf.sprintf "event %d" event_id in
          let node = to_node ~parsed ~offset:off (parsed_length nodes) event_name (List nodes) in
          node :: parse_events (off_7 + off) rest)

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    let events_length off = Bitstring.length bs - off - 8 - 32 in
    match%bitstring bs with
    | {| header           : 24 : bitstring
       ; service_id       : 16 : save_offset_to (off_1)
       ; reserved         : 2  : save_offset_to (off_2)
       ; version_number   : 5  : save_offset_to (off_3)
       ; current_next_ind : 1  : save_offset_to (off_4)
       ; section_number   : 8  : save_offset_to (off_5)
       ; last_section_num : 8  : save_offset_to (off_6)
       ; ts_id            : 16 : save_offset_to (off_7)
       ; on_id            : 16 : save_offset_to (off_8)
       ; seg_last_sec_num : 8  : save_offset_to (off_9)
       ; last_table_id    : 8  : save_offset_to (off_10)
       ; events           : events_length off_10 : save_offset_to (off_11), bitstring
       ; crc32            : 32 : save_offset_to (off_12)
       |} ->
       let events = parse_events off_11 events in
       let header = parse_header header in
       let nodes =
         [ to_node ~offset:off_1 16 "service_id" (Hex (Int service_id))
         ; to_node ~offset:off_2 2 "reserved" (Hex (Int reserved))
         ; to_node ~offset:off_3 5 "version_number" (Dec (Int version_number))
         ; to_node ~offset:off_4 1 "current_next_indicator" (Bits (Bool current_next_ind))
         ; to_node ~offset:off_5 8 "section_number" (Dec (Int section_number))
         ; to_node ~offset:off_6 8 "last_section_number" (Dec (Int last_section_num))
         ; to_node ~offset:off_7 16 "transport_stream_id" (Hex (Int ts_id))
         ; to_node ~offset:off_8 16 "original_network_id" (Hex (Int on_id))
         ; to_node ~offset:off_9 8 "segment_last_section_number" (Dec (Int seg_last_sec_num))
         ; to_node ~offset:off_10 8 "last_table_id" (Hex (Int last_table_id))
         ; to_node ~offset:off_11 (events_length off_10) "events" (List events)
         ; to_node ~offset:off_12 32 "CRC_32" (Hex (Uint32 crc32))
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
       ; utc_time : 40 : save_offset_to (off), bitstring
       |} ->
       let utc_time = match parse_timestamp utc_time with
         | Some x -> Time x
         | None   -> match%bitstring utc_time with
                     | {| i : 40 |} -> Dec (Uint64 i) in
       let node =
         [ to_node ~offset:off 40 "utc_time" utc_time
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
       ; utc_time    : 40 : save_offset_to (off_1), bitstring
       ; reserved    : 4  : save_offset_to (off_2)
       ; length      : 12 : save_offset_to (off_3)
       ; descriptors : length * 8 : save_offset_to (off_4), bitstring
       ; crc32       : 32 : save_offset_to (off_5)
       |} ->
       let dscrs  = parse_descriptors off_4 descriptors in
       let header = parse_header header in
       let utc_time = match parse_timestamp utc_time with
         | Some x -> Time x
         | None   -> match%bitstring utc_time with
                     | {| i : 40 |} -> Dec (Uint64 i) in
       let nodes  =
         [ to_node ~offset:off_1 40 "utc_time" utc_time
         ; to_node ~offset:off_2 4  "reserved" (Hex (Int reserved))
         ; to_node ~offset:off_3 12 "descriptors_loop_length" (Hex (Int length))
         ; to_node ~offset:off_4 (length * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_5 32 "CRC_32" (Hex (Uint32 crc32))
         ]
       in
       header @ nodes

end

module RST = struct

  open Table_common

  let rec parse_events = fun off x ->
    if Bitstring.length x = 0 then []
    else
      (match%bitstring x with
       | {| ts_id      : 16
          ; on_id      : 16 : save_offset_to (off_1)
          ; service_id : 16 : save_offset_to (off_2)
          ; event_id   : 16 : save_offset_to (off_3)
          ; rfu        : 5  : save_offset_to (off_4)
          ; status     : 3  : save_offset_to (off_5)
          ; rest       : -1 : save_offset_to (off_6), bitstring
          |} ->
          let nodes =
            [ to_node ~offset:off           16 "transport_stream_id" (Hex (Int ts_id))
            ; to_node ~offset:(off_1 + off) 16 "original_network_id" (Hex (Int on_id))
            ; to_node ~offset:(off_2 + off) 16 "service_id" (Hex (Int service_id))
            ; to_node ~offset:(off_3 + off) 16 "event_id" (Hex (Int event_id))
            ; to_node ~offset:(off_4 + off)  5 "reserved_future_use" (Bits (Int rfu))
            ; to_node ~offset:(off_5 + off)  3 "running_status" (Hex (Int status))
            ]
          in
          let event_name = Printf.sprintf "event %d" event_id in
          let node = to_node ~offset:off (parsed_length nodes) event_name (List nodes) in
          node :: parse_events (off_6 + off) rest)

  let parse buf =
    let bs = Bitstring.bitstring_of_string buf in
    match%bitstring bs with
       | {| header : 24 : bitstring
          ; rest   : -1 : save_offset_to (off), bitstring
          |} ->
          let header = parse_header header in
          header @ ( parse_events off rest)

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
          let char_list = string_of_bitstring rest |> String.to_list in
          let int_list  = List.map (fun x -> Char.code x) char_list in
          let bytes =
            to_node
              ~offset:24
              (length rest)
              "bytes"
              (Bytes int_list)
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
       |} when Bitstring.length rest = 0 ->
       let nodes =
         [ to_node ~offset:off_1 1 "transition_flag" (Bits (Bool transition_flag))
         ; to_node ~offset:off_2 7 "reserved_future_use" (Bits (Int rfu))
         ]
       in
       let header = parse_header header in
       header @ nodes

end

module SIT = struct

  open Table_common
  open Bitstring

  let rec parse_services = fun off x ->
    if Bitstring.length x = 0 then []
    else
      (match%bitstring x with
       | {| service_id     : 16
          ; dvb_rfu        : 1  : save_offset_to (off_1)
          ; running_status : 3  : save_offset_to (off_2)
          ; length         : 12 : save_offset_to (off_3)
          ; descriptors    : length * 8 : save_offset_to (off_4), bitstring
          ; rest           : -1 : save_offset_to (off_5), bitstring
          |} ->
          let dscrs = parse_descriptors (off + off_4) descriptors in
          let nodes =
            [ to_node ~offset:off           16 "service_id" (Hex (Int service_id))
            ; to_node ~offset:(off_1 + off)  1 "reserved_future_use" (Bits (Bool dvb_rfu))
            ; to_node ~offset:(off_2 + off)  3 "running_status" (Dec (Int running_status))
            ; to_node ~offset:(off_3 + off) 12 "service_loop_length" (Dec (Int length))
            ; to_node ~offset:(off_4 + off) (length * 8) "descriptors" (List dscrs) ]
          in
          let service_name = Printf.sprintf "event %d" service_id in
          let node = to_node ~offset:off (parsed_length nodes) service_name (List nodes) in
          node :: (parse_services (off_5 + off) rest))

  let parse buf =
    let bs = bitstring_of_string buf in
    let services_length off till = length bs - off - (till * 8) - 32 in
    match%bitstring bs with
    | {| header           : 24 : bitstring
       ; dvb_rfu_1        : 16 : save_offset_to (off_1)
       ; iso_reserved     : 2  : save_offset_to (off_2)
       ; version_number   : 5  : save_offset_to (off_3)
       ; current_next_ind : 1  : save_offset_to (off_4)
       ; section_number   : 8  : save_offset_to (off_5)
       ; last_section_num : 8  : save_offset_to (off_6)
       ; dvb_rfu_2        : 4  : save_offset_to (off_7)
       ; len              : 12 : save_offset_to (off_8)
       ; descriptors      : len * 8 : bitstring, save_offset_to (off_9)
       ; services         : services_length off_9 len : save_offset_to (off_10), bitstring
       ; crc32            : 32 : save_offset_to (off_11)
       ; rest             : -1 : bitstring
       |} when length rest = 0 ->
       let services = parse_services off_10 services in
       let dscrs = parse_descriptors off_9 descriptors in
       let nodes =
         [ to_node ~offset:off_1 16 "dvb_rfu_1" (Bits (Int dvb_rfu_1))
         ; to_node ~offset:off_2 2  "ISO_reserved" (Bits (Int iso_reserved))
         ; to_node ~offset:off_3 5  "version_number" (Dec (Int version_number))
         ; to_node ~offset:off_4 1  "current_next_indicator" (Bits (Bool current_next_ind))
         ; to_node ~offset:off_5 8  "section_number" (Dec (Int section_number))
         ; to_node ~offset:off_6 8  "last_section_number" (Dec (Int last_section_num))
         ; to_node ~offset:off_7 4  "dvb_rfu_2" (Bits (Int dvb_rfu_2))
         ; to_node ~offset:off_8 12 "transmission_info_loop_length" (Dec (Int len))
         ; to_node ~offset:off_9 (len * 8) "descriptors" (List dscrs)
         ; to_node ~offset:off_10 (services_length off_9 len) "services" (List services)
         ; to_node ~offset:off_11 32 "CRC_32" (Hex (Uint32 crc32))
         ]
       in
       let header = parse_header header in
       header @ nodes

end

let table_to_yojson : string ->
                      Common.Mpeg_ts.table ->
                      parsed option =
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
    |> Option.return
  with _ -> None
