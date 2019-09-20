open Components_tyxml
open Components_lab_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-table-overview"

  let header = BEM.add_element root "header"

  let title = BEM.add_element root "title"

  let placeholder = BEM.add_element root "placeholder"

  let bitrate_reset = BEM.add_element root "bitrate-reset"

  let menu_icon = BEM.add_element root "menu-icon"

  let back_action = BEM.add_element root "back-action"

  let table = BEM.add_element root "table"

  let no_sync = BEM.add_modifier root "no-sync"

  let no_response = BEM.add_modifier root "no-response"

  let row = BEM.add_element root "row"

  let row_lost = BEM.add_modifier row "lost"

  let hex = BEM.add_modifier root "hex"

  let with_details = BEM.add_modifier root "with-details"

  let details_view = BEM.add_modifier root "details-view"
end

type pid_flags =
  { has_pcr : bool
  ; scrambled : bool }

let compare_pid_flags (a : pid_flags as 'a) (b : 'a) =
  let res = compare a.has_pcr b.has_pcr in
  if res = 0 then compare a.scrambled b.scrambled else res

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Box_markup = Box.Make (Xml) (Svg) (Html)
  module Data_table_markup = Data_table.Make (Xml) (Svg) (Html)
  module Divider_markup = Divider.Make (Xml) (Svg) (Html)
  module Icon_markup = Icon.Make (Xml) (Svg) (Html)
  module Icon_button_markup = Icon_button.Make (Xml) (Svg) (Html)
  module Placeholder_markup = Placeholder.Make (Xml) (Svg) (Html)
  module Menu_markup = Menu.Make (Xml) (Svg) (Html)

  let create_title ?(classes = []) ?(a = []) ?title ?(children = []) () =
    let classes = CSS.title :: classes in
    h3 ~a:(a_class classes :: a) (Utils.map_cons_option txt title children)

  let create_menu_selection_icon ?(classes = []) ?a () =
    Icon_markup.SVG.icon
      ~classes:([Item_list.CSS.item_graphic; Menu.CSS.selection_group_icon] @ classes)
      ?a
      ~d:Svg_icons.check
      ()

  let create_menu_mode_item ?(classes = []) ?(a = []) ?(selected = false) ~mode () =
    let classes = if selected then Menu.CSS.item_selected :: classes else classes in
    Menu_markup.Item_list.list_item
      ~classes
      ~a:
        (a_user_data
           "mode"
           (match mode with
           | `Hex -> "hex"
           | `Dec -> "dec")
         :: a
        |> Utils.cons_if_lazy selected (fun () -> a_aria "selected" ["true"]))
      ~graphic:(create_menu_selection_icon ())
      ~primary_text:
        (`Text
          (match mode with
          | `Hex -> "Hex ID"
          | `Dec -> "Dec ID"))
      ()

  let create_menu ?classes ?a ?(hex = false) () =
    Menu_markup.menu
      ?classes
      ?a
      ~list_children:
        Menu_markup.Item_list.
          [ li
              [ ul
                  ~a:[a_class [Menu.CSS.selection_group]]
                  [ create_menu_mode_item ~selected:hex ~mode:`Hex ()
                  ; create_menu_mode_item ~selected:(not hex) ~mode:`Dec () ] ]
          ; Divider_markup.divider_li ()
          ; list_item
              ~classes:[CSS.bitrate_reset]
              ~primary_text:(`Text "Сброс битрейта")
              () ]
      ()

  let create_header ?(classes = []) ?(a = []) ?hex ?back ?title ?children () =
    let classes = CSS.header :: classes in
    let title =
      match title with
      | None -> None
      | Some (`Text s) -> Some (create_title ~title:s ())
      | Some (`Element e) -> Some e
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          Utils.(
            back
            ^:: title
            ^:: [ div
                    ~a:[a_class [Menu_surface.CSS.anchor]]
                    [ Icon_button_markup.icon_button
                        ~classes:[CSS.menu_icon]
                        ~icon:(Icon_markup.SVG.icon ~d:Svg_icons.dots_vertical ())
                        ()
                    ; create_menu ?hex () ] ])
    in
    header ~a:(a_class classes :: a) children

  let create_empty_placeholder
      ?(classes = [])
      ?a
      ?(icon = Icon_markup.SVG.icon ~d:Svg_icons.emoticon_sad ())
      ?(text = `Text "Таблица пуста")
      () =
    let classes = CSS.placeholder :: classes in
    Placeholder_markup.placeholder ~classes ?a ~icon ~text ()

  let create_back_action ?(classes = []) ?a () =
    let classes = CSS.back_action :: classes in
    Icon_button_markup.icon_button
      ~classes
      ?a
      ~icon:(Icon_markup.SVG.icon ~d:Svg_icons.arrow_left ())
      ()

  let create
      ?(classes = [])
      ?(a = [])
      ?(dense = true)
      ?(hex = false)
      ?(with_details = false)
      ?(data = [])
      ?title
      ~format
      ~control
      () =
    let classes =
      classes
      |> Utils.cons_if hex CSS.hex
      |> Utils.cons_if with_details CSS.with_details
      |> List.cons CSS.root
    in
    let placeholder =
      match data with
      | [] -> Some (create_empty_placeholder ())
      | _ -> None
    in
    let back = if with_details then Some (create_back_action ()) else None in
    let header = create_header ~hex ?back ?title () in
    let table =
      Data_table_markup.data_table_of_fmt ~dense ~classes:[CSS.table] ~format ~data ()
    in
    div ~a:(a_class classes :: a_user_data "control" (string_of_int control) :: a)
    @@ Utils.([header; table] @ placeholder ^:: [])
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)