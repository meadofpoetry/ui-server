let pp_user_human ppf = function
  | `Guest -> Format.pp_print_string ppf "Гость"
  | `Operator -> Format.pp_print_string ppf "Оператор"
  | `Root -> Format.pp_print_string ppf "Администратор"

let user_icon_path = function
  | `Guest -> Components_tyxml.Svg_icons.account
  | `Operator -> Components_tyxml.Svg_icons.worker
  | `Root -> Components_tyxml.Svg_icons.account_tie
