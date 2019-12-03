open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Util

let bitrate_for_pids_ ~get (br : 'a Bitrate.t) (pids : int list) =
  List.fold_left
    (fun acc pid ->
      match List.assoc_opt pid br.pids with
      | None -> acc
      | Some b -> (pid, get b) :: acc)
    []
    pids

let sum_bitrates x = List.fold_left (fun acc x -> acc + snd x) 0 x

let bitrate_for_pids (br : int Bitrate.t) (pids : int list) =
  bitrate_for_pids_ ~get:(fun x -> x) br pids

let cur_bitrate_for_pids (br : Bitrate.ext) (pids : int list) =
  bitrate_for_pids_ ~get:(fun (x : Bitrate.value) -> x.cur) br pids

let min_bitrate_for_pids (br : Bitrate.ext) (pids : int list) =
  bitrate_for_pids_ ~get:(fun (x : Bitrate.value) -> x.min) br pids

let max_bitrate_for_pids (br : Bitrate.ext) (pids : int list) =
  bitrate_for_pids_ ~get:(fun (x : Bitrate.value) -> x.max) br pids

let total_bitrate_for_pids (br : int Bitrate.t) (pids : int list) =
  List.fold_left (fun acc x -> acc + snd x) 0 (bitrate_for_pids br pids)
