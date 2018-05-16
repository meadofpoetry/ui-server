type measure_type = [ `Power | `Mer | `Ber | `Freq | `Bitrate ] [@@deriving yojson]

let measure_type_to_string = function
  | `Power   -> "Мощность"
  | `Mer     -> "MER"
  | `Ber     -> "BER"
  | `Freq    -> "Отклонение частоты"
  | `Bitrate -> "Битрейт"

let measure_type_to_unit = function
  | `Power   -> "дБм"
  | `Mer     -> "дБ"
  | `Ber     -> ""
  | `Freq    -> "Гц"
  | `Bitrate -> "Мбит/с"
