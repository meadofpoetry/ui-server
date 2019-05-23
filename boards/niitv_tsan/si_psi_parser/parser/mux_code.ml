let name = "MuxCode_descriptor"

let rec f_2 ~offset ~i ~k x =
  if Bitstring.bitstring_length x = 0 then []
  else match%bitstring x with
    | {| channel : 8
       ; num     : 8  : save_offset_to (off_1)
       ; rest    : -1 : save_offset_to (off_2), bitstring
       |} ->
      let s_1 = Printf.sprintf "flexMuxChannel[%d][%d]" i k in
      let s_2 = Printf.sprintf "numberOfBytes[%d][%d]" i k in
      let nodes =
        [ Node.make ~offset 8 s_1 (Dec (Int channel))
        ; Node.make ~offset:(offset + off_1) 8 s_2 (Dec (Int num)) ]
      in
      let channel_name = Printf.sprintf "Channel %s" (string_of_int channel) in
      let node = Node.make ~offset 16 channel_name (List nodes) in
      node :: f_2 ~offset:(offset + off_2) ~i ~k:(k + 1) rest

let rec f_1 ~offset ~i x =
  if Bitstring.bitstring_length x = 0 then []
  else  match%bitstring x with
    | {| slot_count : 5
       ; rep_count  : 3 : save_offset_to (off_1)
       ; channels   : slot_count * 16 : save_offset_to (off_2), bitstring
       ; rest       : -1 : save_offset_to (off_3), bitstring
       |} ->
      let nodes =
        [ Node.make ~offset 5 "slotCount" (Dec (Int slot_count))
        ; Node.make ~offset:(offset + off_1) 3 "repetitionCount" (Dec (Int rep_count)) ]
      in
      let channels = nodes @ f_2 ~offset:(offset + off_2) ~i ~k:0 channels in
      let slot_name = Printf.sprintf "Slot %s" (string_of_int slot_count) in
      let node = Node.make ~offset (8 + slot_count * 16) slot_name (List channels) in
      node :: f_1 ~offset:(offset + off_3) ~i:(i + 1) rest

(* ISO/IEC 14496-1 11.2.4.3 *)
let parse bs off =
  match%bitstring bs with
  | {| length    : 8
     ; mux_code  : 4  : save_offset_to (off_1)
     ; version   : 4  : save_offset_to (off_2)
     ; sub_count : 8  : save_offset_to (off_3)
     ; rest      : -1 : save_offset_to (off_4), bitstring
     |} ->
    let nodes =
      [ Node.make ~offset:off 8 "length" (Dec (Int length))
      ; Node.make ~offset:(off + off_1) 4 "MuxCode" (Dec (Int mux_code))
      ; Node.make ~offset:(off + off_2) 4 "version" (Dec (Int version))
      ; Node.make ~offset:(off + off_3) 8 "substructureCount" (Dec (Int sub_count))
      ]
    in
    nodes @ f_1 ~offset:(off + off_4) ~i:0 rest
