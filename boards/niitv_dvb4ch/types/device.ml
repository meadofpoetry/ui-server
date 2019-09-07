type info =
  { serial : int
  ; hw_ver : int
  ; fw_ver : int
  ; fpga_ver : int
  ; asi : bool
  ; receivers : int list }
[@@deriving yojson, eq]

type standard =
  | T2 [@value 1]
  | T
  | C
[@@deriving yojson, eq, enum]

type bw =
  | Bw8 [@value 1]
  | Bw7
  | Bw6
[@@deriving yojson, eq, enum]

type channel =
  { bw : bw
  ; freq : int
  ; plp : int }
[@@deriving yojson, eq]

type mode =
  { standard : standard
  ; channel : channel }
[@@deriving yojson, eq]

type mode_rsp =
  { mode : mode
  ; hw_present : bool
  ; lock : bool }
[@@deriving yojson]

type config =
  { mode : (int * mode) list
  ; source : int }
[@@deriving yojson, eq]

let standard_to_string ?(full = false) x =
  let suffix =
    match x with
    | T2 -> "T2"
    | T -> "T"
    | C -> "C"
  in
  if full then "DVB-" ^ suffix else suffix

let standard_of_string = function
  | "DVB-T2" | "T2" -> Some T2
  | "DVB-T" | "T" -> Some T
  | "DVB-C" | "C" -> Some C
  | _ -> None

let bw_to_string = function
  | Bw8 -> "8 MHz"
  | Bw7 -> "7 MHz"
  | Bw6 -> "6 MHz"

let channel_to_string ?(plp = true) (x : channel) =
  let base =
    Printf.sprintf "bandwidth: %s, frequency: %d Hz" (bw_to_string x.bw) x.freq
  in
  if not plp then base else Printf.sprintf "%s, PLP ID: %d" base x.plp

let mode_to_string (x : mode) =
  match x.standard with
  | T2 ->
      Printf.sprintf
        "%s, %s"
        (standard_to_string x.standard)
        (channel_to_string ~plp:true x.channel)
  | T | C ->
      Printf.sprintf
        "%s, %s"
        (standard_to_string x.standard)
        (channel_to_string ~plp:false x.channel)

let mode_rsp_to_string (x : mode_rsp) =
  Printf.sprintf
    "hw present: %b, lock: %b, mode: %s"
    x.hw_present
    x.lock
    (mode_to_string x.mode)

let info_to_string (x : info) =
  Printf.sprintf
    "serial: %d, hw version: %d, fpga version: %d, fw version: %d, asi: %b, receivers: \
     [%s]"
    x.serial
    x.hw_ver
    x.fpga_ver
    x.fw_ver
    x.asi
    (String.concat ", " @@ List.map string_of_int x.receivers)
