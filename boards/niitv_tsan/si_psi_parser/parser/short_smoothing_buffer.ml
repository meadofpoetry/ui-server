let name = "short_smoothing_buffer_descriptor"

let parse_size size = match size with 1 -> "1 536" | _ -> "DVB_reserved"

let parse_leak_rate = function
  | 0 -> "DVB_reserved"
  | 1 -> "0,0009"
  | 2 -> "0,0018"
  | 3 -> "0,0036"
  | 4 -> "0,0072"
  | 5 -> "0,0108"
  | 6 -> "0,0144"
  | 7 -> "0,0216"
  | 8 -> "0,0288"
  | 9 -> "0,075"
  | 10 -> "0,5"
  | 11 -> "0,5625"
  | 12 -> "0,8437"
  | 13 -> "1,0"
  | 14 -> "1,1250"
  | 15 -> "1,5"
  | 16 -> "1,6875"
  | 17 -> "2,0"
  | 18 -> "2,25"
  | 19 -> "2,5"
  | 20 -> "3,0"
  | 21 -> "3,3750"
  | 22 -> "3,5"
  | 23 -> "4,0"
  | 24 -> "4,5"
  | 25 -> "5,0"
  | 26 -> "5,5"
  | 27 -> "6,0"
  | 28 -> "6,5"
  | 29 -> "6,75"
  | 30 -> "7,0"
  | 31 -> "7,5"
  | 32 -> "8,0"
  | 38 -> "13,5"
  | 48 -> "27"
  | 56 -> "44"
  | 57 -> "48"
  | 58 -> "54"
  | 59 -> "72"
  | 60 -> "108"
  | x when x > 60 && x < 64 -> "DVB_reserved"
  | x when x > 32 && x < 38 -> Printf.sprintf "%d" ((x - 16) / 2)
  | x when x > 38 && x < 44 -> Printf.sprintf "%d" (x - 24)
  | x when x > 43 && x < 48 -> Printf.sprintf "%d" (x - 25)
  | x when x > 48 && x < 56 -> Printf.sprintf "%d" ((x - 34) * 2)
  | _ -> assert false

let parse bs off =
  match%bitstring bs with
  | {| sb_size : 2
     ; sb_lr   : 6  : save_offset_to (off_1)
     ; rest    : -1 : save_offset_to (off_2), bitstring
     |}
    ->
      let size = parse_size sb_size in
      let lr = parse_leak_rate sb_lr in
      let nodes =
        [
          Node.make ~parsed:size ~offset:off 2 "sb_size" (Dec (Int sb_size));
          Node.make ~parsed:lr ~offset:(off + off_1) 6 "sb_leak_rate"
            (Dec (Int sb_lr));
        ]
      in
      nodes @ Bytes.parse ~offset:(off + off_2) rest "DVB_reserved"
