let rfu_to_string x = Printf.sprintf "Reserved (%d)" x



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
                  | l  -> let s = CCString.concat ", " (List.map (fun x -> frame_rate_to_string x) l) in
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
         |} when mpeg_1_only_flag = false -> { mfr_flag; frame_rate; mpeg_1_only_flag; constrained_parameter_flag;
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
      { free_format_flag = false
      ; id               = false
      ; layer            = 0
      ; variable_rate    = false
      ; reserved         = 0 
      }

  end

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

  type descriptor = Video_stream of Video_stream.t
                  | Audio_stream of Audio_stream.t
                  | Unknown      of Unknown.t [@@deriving yojson]

  type t =
    { tag     : int
    ; length  : int
    ; name    : string
    ; content : descriptor
    } [@@deriving yojson]

  let decode tag length body =
    let name,content = (match tag with
                        | 0x02 -> Video_stream.name,Video_stream (Video_stream.decode body)
                        | 0x03 -> Audio_stream.name,Audio_stream (Audio_stream.decode body)
                        | _    -> Unknown.name     ,Unknown (Unknown.decode body)) in
    { tag; length; name; content }

  let of_cbuffer buf =
    let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string buf in
    match%bitstring bs with
    | {| tag : 8
       ; length : 8
       ; body : length * 8 : bitstring
       ; rest : -1 : bitstring
       |} -> (decode tag length body),if Bitstring.bitstring_length rest > 0
                                      then Some (Cbuffer.of_string @@ Bitstring.string_of_bitstring rest)
                                      else None

  (* module Hierarchy = struct *)

  (* end *)

  (* module Registration = struct *)

  (* end *)

  (* module Data_stream_alignment = struct *)

  (* end *)

  (* module Target_background_grid = struct *)

  (* end *)

  (* module Video_window = struct *)

  (* end *)

  (* module CA = struct *)

  (* end *)

  (* module ISO_639_language = struct *)

  (* end *)

  (* module System_clock = struct *)

  (* end *)

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

end

