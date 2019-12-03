type measure_type =
  [ `Power
  | `Mer
  | `Ber
  | `Freq
  | `Bitrate ]
[@@deriving yojson, eq]

let module_name = "Модуль"

let bw_to_string : Board_niitv_dvb4ch_types.Device.bw -> string = function
  | Bw8 -> "8 МГц"
  | Bw7 -> "7 МГц"
  | Bw6 -> "6 МГц"

let hz_in_mhz = 1_000_000

let hz_in_khz = 1_000

let freq_to_string (x : int) =
  if x mod hz_in_mhz = 0
  then Printf.sprintf "%d МГц" @@ (x / hz_in_mhz)
  else if x mod hz_in_khz = 0
  then Printf.sprintf "%d кГц" @@ (x / hz_in_khz)
  else Printf.sprintf "%d Гц" x

let measure_type_to_string = function
  | `Power -> "Мощность"
  | `Mer -> "MER"
  | `Ber -> "BER"
  | `Freq -> "Отклонение частоты"
  | `Bitrate -> "Битрейт"

let measure_type_to_unit = function
  | `Power -> "дБм"
  | `Mer -> "дБ"
  | `Ber -> ""
  | `Freq -> "Гц"
  | `Bitrate -> "Мбит/с"
