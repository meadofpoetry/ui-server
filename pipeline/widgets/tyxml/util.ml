type measure_typ =
  [ `Black
  | `Luma
  | `Freeze
  | `Diff
  | `Blocky
  | `Shortt
  | `Moment
  ]

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
