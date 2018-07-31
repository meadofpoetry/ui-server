type table =
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
and ao = [ `Actual  | `Other    ]
and ps = [ `Present | `Schedule ]

let table_of_int : int -> table = function
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
  | x when x >= 0x50 && x <= 0x5F ->
     `EIT (`Actual, `Schedule)
  | x when x >= 0x60 && x <= 0x6F ->
     `EIT (`Other,  `Schedule)
  | 0x70 -> `TDT
  | 0x71 -> `RST
  | 0x72 -> `ST
  | 0x73 -> `TOT
  | 0x7E -> `DIT
  | 0x7F -> `SIT
  | x    -> `Unknown x

let table_to_string : ?simple:bool -> table -> string =
  fun ?(simple=false) ->
  function
  | `PAT   -> "PAT"
  | `CAT   -> "CAT"
  | `PMT   -> "PMT"
  | `TSDT  -> "TSDT"
  | `NIT x ->
     if simple then "NIT"
     else (match x with
           | `Actual -> "NIT actual"
           | `Other  -> "NIT other")
  | `SDT x ->
     if simple then "SDT"
     else (match x with
           | `Actual -> "SDT actual"
           | `Other  -> "SDT other")
  | `BAT   -> "BAT"
  | `EIT x ->
     if simple then "EIT"
     else (match x with
           | `Actual, `Present  -> "EIT actual present"
           | `Other , `Present  -> "EIT other present"
           | `Actual, `Schedule -> "EIT actual schedule"
           | `Other , `Schedule -> "EIT other schedule")
  | `TDT   -> "TDT"
  | `RST   -> "RST"
  | `ST    -> "ST"
  | `TOT   -> "TOT"
  | `DIT   -> "DIT"
  | `SIT   -> "SIT"
  | `Unknown _ -> "Unknown"

(* In accordance to ISO/IEC 13818-1:2015(E) *)
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

(* In accordance to FIXME *)
let running_status_to_string = function
  | 0 -> "undefined"
  | 1 -> "not running"
  | 2 -> "starts in a few seconds"
  | 3 -> "pausing"
  | 4 -> "running"
  | _ -> "reserved for future use"

(* In accordance to EN 300 468 V1.15.1 *)
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
