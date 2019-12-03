module CSS = struct
  let root = "board-niitv-tsan"
end

let pid_to_hex_string = Printf.sprintf "0x%04X"

let pid_to_dec_string = Printf.sprintf "%d"

let pid_to_string ~hex pid = if hex then pid_to_hex_string pid else pid_to_dec_string pid

let service_pids (info : Board_niitv_tsan_types.Service.t) =
  if info.has_pmt then info.pmt_pid :: info.elements else info.elements
