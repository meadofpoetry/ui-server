include Board_niitv_tsan_widgets_tyxml.Util

type widget_state =
  | Fine
  | No_sync
  | No_response

let make_timestamp_string (timestamp : Ptime.t option) =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let s = match timestamp with
    | None -> "-"
    | Some t -> Time.to_human_string ?tz_offset_s t
  in
  "Обновлено: " ^ s
