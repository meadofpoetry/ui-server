type measure_type = [ `Power | `Mer | `Ber | `Freq | `Bitrate ]

(* type _ typ = Power   : float typ
 *            | Mer     : float typ
 *            | Ber     : float typ
 *            | Freq    : Int32.t typ
 *            | Bitrate : Int32.t typ
 * 
 * let type_to_string : type a. a typ -> string = function
 *   | Power   -> "Мощность"
 *   | Mer     -> "MER"
 *   | Ber     -> "BER"
 *   | Freq    -> "Отклонение частоты"
 *   | Bitrate -> "Битрейт"
 * 
 * let type_to_unit : type a. a typ -> string = function
 *   | Power   -> "дБм"
 *   | Mer     -> "дБ"
 *   | Ber     -> ""
 *   | Freq    -> "Гц"
 *   | Bitrate -> "Мбит/с" *)

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
