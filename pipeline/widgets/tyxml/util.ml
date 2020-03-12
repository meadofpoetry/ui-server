type measure_typ =
  [ `Black | `Luma | `Freeze | `Diff | `Blocky | `Shortt | `Moment ]
[@@deriving yojson]

let equal_measure_typ a b =
  match (a, b) with
  | `Black, `Black -> true
  | `Luma, `Luma -> true
  | `Freeze, `Freeze -> true
  | `Diff, `Diff -> true
  | `Blocky, `Blocky -> true
  | `Shortt, `Shortt -> true
  | `Moment, `Moment -> true
  | _ -> false

let measure_typ_to_string : measure_typ -> string = function
  | `Black -> "black"
  | `Luma -> "luma"
  | `Freeze -> "freeze"
  | `Diff -> "diff"
  | `Blocky -> "blocky"
  | `Shortt -> "shortt"
  | `Moment -> "moment"

let measure_typ_of_string : string -> measure_typ option = function
  | "black" -> Some `Black
  | "luma" -> Some `Luma
  | "freeze" -> Some `Freeze
  | "diff" -> Some `Diff
  | "blocky" -> Some `Blocky
  | "shortt" -> Some `Shortt
  | "moment" -> Some `Moment
  | _ -> None

let measure_typ_to_human_string : measure_typ -> string = function
  | `Black -> "Чёрный кадр"
  | `Luma -> "Средняя яркость"
  | `Freeze -> "Заморозка видео"
  | `Diff -> "Средняя разность"
  | `Blocky -> "Блочность"
  | `Shortt -> "Громкость (short term)"
  | `Moment -> "Громкость (momentary)"

let measure_typ_to_unit_string : measure_typ -> string = function
  | `Black | `Freeze | `Blocky -> "%"
  | `Luma | `Diff -> ""
  | `Shortt | `Moment -> "LUFS"
