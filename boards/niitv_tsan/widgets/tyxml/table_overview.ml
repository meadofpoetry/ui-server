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
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module Box_markup = Box.Make (Xml) (Svg) (Html)
  module Data_table_markup = Data_table.Make (Xml) (Svg) (Html)
  module Divider_markup = Divider.Make (Xml) (Svg) (Html)
  module Icon_markup = Icon.Make (Xml) (Svg) (Html)
  module Icon_button_markup = Icon_button.Make (Xml) (Svg) (Html)
  module Placeholder_markup = Placeholder.Make (Xml) (Svg) (Html)
  module Menu_markup = Menu.Make (Xml) (Svg) (Html)

  let ( @:: ) x l = cons (return x) l

  let ( ^:: ) x l = Option.fold ~none:l ~some:(fun x -> x @:: l) x

  let create_title ?(a = []) ?title ?(children = nil ()) () =
    let children =
      match title with
      | None -> children
      | Some x -> txt x @:: children
    in
    h3 ~a:(a_class (return [CSS.title]) :: a) children

  let create_menu_selection_icon ?a () =
    Icon_markup.SVG.icon
      ~classes:(return [Item_list.CSS.item_graphic; Menu.CSS.selection_group_icon])
      ?a
      ~d:(return Svg_icons.check)
      ()

  let create_menu_mode_item ?(a = []) ?(selected = return false) ~mode () =
    let classes = fmap (fun x -> if x then [Menu.CSS.item_selected] else []) selected in
    Menu_markup.Item_list.list_item
      ~classes
      ~a:
        (a_user_data
           "mode"
           (match mode with
           | `Hex -> return "hex"
           | `Dec -> return "dec")
        :: a_aria "selected" (fmap (fun x -> [string_of_bool x]) selected)
        :: a)
      ~graphic:(return @@ create_menu_selection_icon ())
      ~primary_text:
        (`Text
          (match mode with
          | `Hex -> return "Hex ID"
          | `Dec -> return "Dec ID"))
      ()

  let create_menu ?classes ?a ?(hex = return false) () =
    Menu_markup.menu
      ?classes
      ?a
      ~list_children:
        (li
           (singleton
              (return
                 (ul
                    ~a:[a_class (return [Menu.CSS.selection_group])]
                    (create_menu_mode_item ~selected:hex ~mode:`Hex ()
                    @:: create_menu_mode_item ~selected:(fmap not hex) ~mode:`Dec ()
                    @:: nil ()))))
        @:: Divider_markup.divider_li ()
        @:: Menu_markup.Item_list.list_item
              ~classes:(return [CSS.bitrate_reset])
              ~primary_text:(`Text (return "Сброс битрейта"))
              ()
        @:: nil ())
      ()

  let create_header ?(a = []) ?hex ?back ?title ?children () =
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
          back
          ^:: title
          ^:: div
                ~a:[a_class (return [Menu_surface.CSS.anchor])]
                (Icon_button_markup.icon_button
                   ~classes:(return [CSS.menu_icon])
                   ~icon:
                     (return
                     @@ Icon_markup.SVG.icon ~d:(return Svg_icons.dots_vertical) ())
                   ()
                @:: create_menu ?hex ()
                @:: nil ())
          @:: nil ()
    in
    header ~a:(a_class (return [CSS.header]) :: a) children

  let create_empty_placeholder
      ?a
      ?(icon = Icon_markup.SVG.icon ~d:(return Svg_icons.emoticon_sad) ())
      ?(text = `Text (return "Таблица пуста"))
      () =
    Placeholder_markup.placeholder
      ~classes:(return [CSS.placeholder])
      ?a
      ~icon:(return icon)
      ~text
      ()

  let create_back_action ?a () =
    Icon_button_markup.icon_button
      ~classes:(return [CSS.back_action])
      ?a
      ~icon:(return (Icon_markup.SVG.icon ~d:(return Svg_icons.arrow_left) ()))
      ()

  let create
      ?(a = [])
      ?(dense = true)
      ?(hex = return false)
      ?(with_details = false)
      ?(data = nil ())
      ?title
      ~format
      ~control
      () =
    let classes = Utils.cons_if with_details CSS.with_details [CSS.root] in
    let classes = fmap (fun x -> Utils.cons_if x CSS.hex classes) hex in
    let placeholder = create_empty_placeholder () in
    let back = if with_details then Some (create_back_action ()) else None in
    let header = create_header ~hex ?back ?title () in
    let table =
      Data_table_markup.data_table_of_fmt
        ~dense
        ~classes:(return [CSS.table])
        ~format
        ~data
        ()
    in
    div
      ~a:(a_class classes :: a_user_data "control" (return (string_of_int control)) :: a)
      (header @:: table @:: placeholder @:: nil ())
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
