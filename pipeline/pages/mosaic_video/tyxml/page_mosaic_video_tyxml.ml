open Components_tyxml

type hotkeys_group = (string * string) list

type hotkeys = (string * hotkeys_group) list

let (volume_hotkeys : hotkeys_group) =
  [ "Увеличить громкость звука", "▲"
  ; "Уменьшить громкость звука", "▼"
  ; "Включить или отключить звук", "m"
  ]

let (playback_hotkeys : hotkeys_group) =
  ["Начать или приостановить воспроизведение", "Пробел"]

let (general_hotkeys : hotkeys_group) =
  [ "Включить или выключить полноэкранный режим", "f"
  ; "Выключить полноэкранный режим", "Esc"
  ]

let (hotkeys : hotkeys) =
  [ "Воспроизведение", playback_hotkeys
  ; "Звук", volume_hotkeys
  ; "Общие", general_hotkeys
  ]

module Player = Player
module Hotkeys = Hotkeys

module CSS = struct
  let root = "mosaic"

  let side_sheet_icon = BEM.add_element root "side-sheet-icon"

  let menu_icon = BEM.add_element root "menu-icon"

  let edit = BEM.add_element root "edit"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  let create_hotkeys ?classes ?attrs () : 'a Html.elt =
    let module M = Hotkeys.Make(Xml)(Svg)(Html) in
    let create_option (label, hotkey) =
      let label = M.create_label label () in
      let hotkey = M.create_hotkey hotkey () in
      M.create_option ~label ~hotkey () in
    let create_section (title, (gp : hotkeys_group)) =
      let title = M.create_section_title title () in
      let options = List.map create_option gp in
      M.create_section ~title ~options () in
    M.create ?classes ?attrs ~sections:(List.map create_section hotkeys) ()
end

