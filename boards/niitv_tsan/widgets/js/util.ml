type widget_state =
  | Fine
  | No_sync
  | No_response

module PID = struct
  let to_hex_string =
    Printf.sprintf "0x%04X"

  let to_dec_string =
    Printf.sprintf "%d"
end

let make_timestamp_string (timestamp : Ptime.t option) =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let s = match timestamp with
    | None -> "-"
    | Some t -> Time.to_human_string ?tz_offset_s t
  in
  "Обновлено: " ^ s
