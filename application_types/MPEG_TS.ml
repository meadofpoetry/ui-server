(* In accordance with FIXME *)
let running_status_to_string = function
  | 0 -> "undefined"
  | 1 -> "not running"
  | 2 -> "starts in a few seconds"
  | 3 -> "pausing"
  | 4 -> "running"
  | _ -> "reserved for future use"

(* In accordance with ISO/IEC 13818-1:2015(E) *)
let stream_type_to_string = function
  | 0x00 -> "Reserved"
  | 0x01 -> "Video MPEG-1"
  | 0x02 -> "Video MPEG-2"
  | 0x03 -> "Audio MPEG-1"
  | 0x04 -> "Audio MPEG-2"
  | 0x05 -> "Private sections"
  | 0x06 -> "Private PES packets"
  | 0x07 -> "MHEG"
  | 0x08 -> "DSM-CC"
  | 0x09 -> "H.222.1"
  | 0x0A -> "DSM-CC Type A"
  | 0x0B -> "DSM-CC Type B"
  | 0x0C -> "DSM-CC Type C"
  | 0x0D -> "DSM-CC Type D"
  | 0x0E -> "Auxiliary"
  | 0x0F -> "Audio AAC ADTS"
  | 0x10 -> "Video MPEG-4"
  | 0x11 -> "Audio AAC LATM"
  | 0x12 -> "SL FlexMux PES packets"
  | 0x13 -> "SL FlexMux sections"
  | 0x14 -> "Synchronized download"
  | 0x15 -> "Metadata PES packets"
  | 0x16 -> "Metadata sections"
  | 0x17 -> "Metadata data carousel"
  | 0x18 -> "Metadata object carousel"
  | 0x19 -> "Metadata synchronized download"
  | 0x1A -> "MPEG-2 IPMP"
  | 0x1B -> "Video H264"
  | 0x1C -> "Audio AAC Clean"
  | 0x1D -> "MPEG-4 timed text"
  | 0x1E -> "Video RVC"
  | 0x1F -> "Video H264 SVC sub-bitstream"
  | 0x20 -> "Video H264 MVC sub-bitstream"
  | 0x21 -> "Video JP2K"
  | 0x22 -> "Video MPEG-2 stereo additional view"
  | 0x23 -> "Video H264 stereo additional view"
  | 0x24 -> "Video HEVC"
  | 0x25 -> "Video HEVC temporal subset"
  | 0x26 -> "Video H264 MVCD sub-bitstream"
  | x when x >= 0x27 && x <= 0x7E -> "Reserved"
  | 0x7F -> "IPMP stream"
  | x when x >= 0x80 && x <= 0xFF -> "User Private"
  | _    -> "Unknown"

(* In accordance with EN 300 468 V1.15.1 *)
let service_type_to_string = function
  | 0x00 -> "rfu"
  | 0x01 -> "digital television"
  | 0x02 -> "digital radio"
  | 0x03 -> "teletext"
  | 0x04 -> "NVOD reference"
  | 0x05 -> "NVOD time-shifted"
  | 0x06 -> "mosaic"
  | 0x07 -> "FM radio"
  | 0x08 -> "DVB SRM"
  | 0x09 -> "rfu"
  | 0x0A -> "adv. codec digital radio"
  | 0x0B -> "H.264/AVC mosaic"
  | 0x0C -> "data broadcasting"
  | 0x0D -> "reserved for Common Interface Usage"
  | 0x0E -> "RCS Map"
  | 0x0F -> "RCS FLS"
  | 0x10 -> "DVB MHP"
  | 0x11 -> "MPEG-2 HD digital TV"
  | x when x >= 0x12 && x <= 0x15 -> "rfu"
  | 0x16 -> "H.264/AVC SD digital TV"
  | 0x17 -> "H.264/AVC SD NVOD time-shifted"
  | 0x18 -> "H.264/AVC SD NVOD reference"
  | 0x19 -> "H.264/AVC HD digital TV"
  | 0x1A -> "H.264/AVC HD NVOD time-shifted"
  | 0x1B -> "H.264/AVC HD NVOD reference"
  | 0x1C -> "H.264/AVC 3D HD digital TV"
  | 0x1D -> "H.264/AVC 3D HD NVOD time-shifted"
  | 0x1E -> "H.264/AVC 3D HD NVOD reference"
  | 0x1F -> "HEVC digital TV"
  | x when x >= 0x20 && x <= 0x7F -> "rfu"
  | x when x >= 0x80 && x <= 0xFE -> "user defined"
  | _    -> "rfu"

module ETR290_error = struct

  type t =
    | Sync_loss
    | Sync_byte_error
    | PAT_error
    | CC_error
    | PMT_error
    | PID_error
    | Transport_error
    | CRC_error
    | PCR_error
    | PCR_accuracy_error
    | PTS_error
    | CAT_error
    | NIT_error
    | SI_repetition_error
    | Unreferenced_pid
    | SDT_error
    | EIT_error
    | RST_error
    | TDT_error

  let priority : t -> int = function
    | (Sync_loss | Sync_byte_error | PAT_error
      | CC_error | PMT_error | PID_error) -> 1
    | (Transport_error | CRC_error | PCR_error
      | PCR_accuracy_error | PTS_error | CAT_error) -> 2
    | (NIT_error | SI_repetition_error | Unreferenced_pid
      | SDT_error | EIT_error | RST_error | TDT_error) -> 3

  let name : t -> string = function
    | Sync_loss -> "TS sync loss"
    | Sync_byte_error -> "Sync byte error"
    | PAT_error -> "PAT error"
    | CC_error -> "Continuity count error"
    | PMT_error -> "PMT error"
    | PID_error -> "PID error"
    | Transport_error -> "Transport error"
    | CRC_error -> "CRC error"
    | PCR_error -> "PCR error"
    | PCR_accuracy_error -> "PCR accuracy error"
    | PTS_error -> "PTS error"
    | CAT_error -> "CAT error"
    | NIT_error -> "NIT error"
    | SI_repetition_error -> "SI repetition error"
    | Unreferenced_pid -> "Unreferenced PID"
    | SDT_error -> "SDT error"
    | EIT_error -> "EIT error"
    | RST_error -> "RST error"
    | TDT_error -> "TDT error"

  let number : t -> string = function
    | Sync_loss -> "1.1"
    | Sync_byte_error -> "1.2"
    | PAT_error -> "1.3"
    | CC_error -> "1.4"
    | PMT_error -> "1.5"
    | PID_error -> "1.6"
    | Transport_error -> "2.1"
    | CRC_error -> "2.2"
    | PCR_error -> "2.3"
    | PCR_accuracy_error -> "2.4"
    | PTS_error -> "2.5"
    | CAT_error -> "2.6"
    | NIT_error -> "3.1"
    | SI_repetition_error -> "3.2"
    | Unreferenced_pid -> "3.4"
    | SDT_error -> "3.5"
    | EIT_error -> "3.6"
    | RST_error -> "3.7"
    | TDT_error -> "3.8"

end

module SI_PSI = struct

  type t =
    [ `PAT
    | `CAT
    | `PMT
    | `TSDT
    | `NIT of ao
    | `SDT of ao
    | `BAT
    | `EIT of ao * ps
    | `TDT
    | `RST
    | `ST
    | `TOT
    | `DIT
    | `SIT
    | `Unknown of int
    ]
  and ao = [ `Actual | `Other ]
  and ps = [ `Present | `Schedule ]

  let of_table_id : int -> t = function
    | 0x00 -> `PAT
    | 0x01 -> `CAT
    | 0x02 -> `PMT
    | 0x03 -> `TSDT
    | 0x40 -> `NIT `Actual
    | 0x41 -> `NIT `Other
    | 0x42 -> `SDT `Actual
    | 0x46 -> `SDT `Other
    | 0x4A -> `BAT
    | 0x4E -> `EIT (`Actual, `Present)
    | 0x4F -> `EIT (`Other,  `Present)
    | 0x70 -> `TDT
    | 0x71 -> `RST
    | 0x72 -> `ST
    | 0x73 -> `TOT
    | 0x7E -> `DIT
    | 0x7F -> `SIT
    | x when x >= 0x50 && x <= 0x5F -> `EIT (`Actual, `Schedule)
    | x when x >= 0x60 && x <= 0x6F -> `EIT (`Other,  `Schedule)
    | x -> `Unknown x

  let name ?(short = false) : t -> string = function
    | `PAT -> "PAT"
    | `CAT -> "CAT"
    | `PMT -> "PMT"
    | `TSDT -> "TSDT"
    | `BAT -> "BAT"
    | `TDT -> "TDT"
    | `RST -> "RST"
    | `ST -> "ST"
    | `TOT -> "TOT"
    | `DIT -> "DIT"
    | `SIT -> "SIT"
    | `Unknown x -> Printf.sprintf "Unknown (0x%02X)" x
    | `NIT x ->
      if short then "NIT"
      else (match x with
          | `Actual -> "NIT actual"
          | `Other -> "NIT other")
    | `SDT x ->
      if short then "SDT"
      else (match x with
          | `Actual -> "SDT actual"
          | `Other -> "SDT other")
    | `EIT x ->
      if short then "EIT"
      else (match x with
          | `Actual, `Present -> "EIT actual present"
          | `Other, `Present -> "EIT other present"
          | `Actual, `Schedule -> "EIT actual schedule"
          | `Other, `Schedule -> "EIT other schedule")

end

module PID = struct

  module Type = struct

    type t =
      | SEC of int list
      | PES of pes
      | ECM of ecm
      | EMM of emm
      | Private
      | Null
    and emm =
      { ca_sys_id : int
      }
    and ecm = emm
    and pes =
      { stream_type : int
      ; stream_id : int
      } [@@deriving yojson, eq, show, ord]

    let to_string : t -> string = function
      | SEC l ->
        let s =
          List.map (fun x -> SI_PSI.name @@ SI_PSI.of_table_id x) l
          |> String.concat ", " in
        "SEC -> " ^ s
      | PES x ->
        let s = stream_type_to_string x.stream_type in
        "PES -> " ^ s
      | ECM x -> "ECM -> " ^ (string_of_int x.ca_sys_id)
      | EMM x -> "EMM -> " ^ (string_of_int x.ca_sys_id)
      | Null -> "Null"
      | Private -> "Private"

  end
end
