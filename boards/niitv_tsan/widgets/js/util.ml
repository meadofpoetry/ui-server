open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Util

let bitrate_for_pids (br : Bitrate.t) (pids : int list) =
  List.fold_left
    (fun acc pid ->
      match List.assoc_opt pid br.pids with
      | None -> acc
      | Some b -> (pid, b) :: acc)
    []
    pids

let total_bitrate_for_pids (br : Bitrate.t) (pids : int list) =
  List.fold_left (fun acc x -> acc + snd x) 0 (bitrate_for_pids br pids)

let make_timestamp_string (timestamp : Ptime.t option) =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let s =
    match timestamp with
    | None -> "-"
    | Some t -> Time.to_human_string ?tz_offset_s t
  in
  "Обновлено: " ^ s
