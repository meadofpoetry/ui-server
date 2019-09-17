open Components_tyxml
open Components_lab_tyxml
open Application_types
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-pid-overview"

  let header = BEM.add_element root "header"

  let title = BEM.add_element root "title"

  let bitrate_reset = BEM.add_element root "bitrate-reset"

  let menu_icon = BEM.add_element root "menu-icon"

  let table = BEM.add_element root "table"

  let no_sync = BEM.add_modifier root "no-sync"

  let no_response = BEM.add_modifier root "no-response"

  let row = BEM.add_element root "row"

  let row_lost = BEM.add_modifier row "lost"
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
  module Fmt = Data_table.Make_fmt (Xml)
  module Icon_markup = Icon.Make (Xml) (Svg) (Html)
  module Icon_button_markup = Icon_button.Make (Xml) (Svg) (Html)
  module Placeholder_markup = Placeholder.Make (Xml) (Svg) (Html)
  module Menu_markup = Menu.Make (Xml) (Svg) (Html)

  let create_title ?(classes = []) ?(attrs = []) ?title ?(children = []) () =
    let classes = CSS.title :: classes in
    h3 ~a:([a_class classes] @ attrs) (Utils.map_cons_option txt title children)

  let create_menu_selection_icon ?(classes = []) ?attrs () =
    Icon_markup.SVG.create
      ~classes:([Item_list.CSS.item_graphic; Menu.CSS.selection_group_icon] @ classes)
      ?attrs
      ~d:Svg_icons.check
      ()

  let create_menu_mode_item ?(classes = []) ?(attrs = []) ?(selected = false) ~mode () =
    let classes = if selected then Menu.CSS.item_selected :: classes else classes in
    Menu_markup.Item_list.create_item
      ~classes
      ~attrs:
        ([ a_user_data
             "mode"
             (match mode with
             | `Hex -> "hex"
             | `Dec -> "dec") ]
         @ attrs
        |> Utils.cons_if_lazy selected (fun () -> a_aria "selected" ["true"]))
      ~graphic:(create_menu_selection_icon ())
      ~primary_text:
        (`Text
          (match mode with
          | `Hex -> "Hex ID"
          | `Dec -> "Dec ID"))
      ()

  let create_menu ?classes ?attrs ?(hex = false) () =
    Menu_markup.create
      ?classes
      ?attrs
      ~list_children:
        Menu_markup.Item_list.
          [ li
              [ ul
                  ~a:[a_class [Menu.CSS.selection_group]]
                  [ create_menu_mode_item ~selected:hex ~mode:`Hex ()
                  ; create_menu_mode_item ~selected:(not hex) ~mode:`Dec () ] ]
          ; Divider_markup.create_li ()
          ; create_item
              ~classes:[CSS.bitrate_reset]
              ~primary_text:(`Text "Сброс битрейта")
              () ]
      ()

  let create_header ?(classes = []) ?(attrs = []) ?hex ?children () =
    let classes = CSS.header :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          [ create_title ~title:"Список PID" ()
          ; div
              ~a:[a_class [Menu_surface.CSS.anchor]]
              [ Icon_button_markup.create
                  ~classes:[CSS.menu_icon]
                  ~icon:(Icon_markup.SVG.create ~d:Svg_icons.dots_vertical ())
                  ()
              ; create_menu ?hex () ] ]
    in
    header ~a:([a_class classes] @ attrs) children

  let pid_type_fmt : MPEG_TS.PID.Type.t Fmt.custom =
    MPEG_TS.PID.Type.
      { to_string
      ; of_string = (fun _ -> failwith "Not implemented")
      ; compare
      ; is_numeric = false }

  let dec_pid_fmt = Fmt.Int

  let hex_pid_fmt =
    Fmt.Custom
      { to_string = Util.pid_to_hex_string
      ; of_string = int_of_string
      ; compare
      ; is_numeric = true }

  let pid_fmt ~hex = if hex then hex_pid_fmt else dec_pid_fmt

  let create_pid_flags ?classes ?attrs {has_pcr; scrambled} =
    let pcr =
      match has_pcr with
      | false -> None
      | true -> Some (Icon_markup.SVG.create ~d:Svg_icons.clock_outline ())
    in
    let scr =
      match scrambled with
      | false -> None
      | true -> Some (Icon_markup.SVG.create ~d:Svg_icons.lock ())
    in
    let ( ^:: ) x l =
      match x with
      | None -> l
      | Some x -> x :: l
    in
    let children = scr ^:: pcr ^:: [] in
    Box_markup.create ?classes ?attrs ~children ()

  let pid_flags_fmt : pid_flags Fmt.custom_elt =
    { to_elt = Utils.(Html.toelt % create_pid_flags)
    ; of_elt = (fun _ -> failwith "Not implemented")
    ; compare = compare_pid_flags
    ; is_numeric = false }

  let pct_fmt =
    Fmt.Custom
      { to_string = (fun x -> Printf.sprintf "%.2f" x)
      ; of_string = float_of_string
      ; compare
      ; is_numeric = true }

  let create_table_format ?(hex = false) () : _ Data_table_markup.Fmt.format =
    let br_fmt = Fmt.Option (Float, "-") in
    let pct_fmt = Fmt.Option (pct_fmt, "-") in
    Fmt.
      [ make_column ~sortable:true ~title:"PID" (pid_fmt ~hex)
      ; make_column ~sortable:true ~title:"Тип" (Custom pid_type_fmt)
      ; make_column ~title:"Доп. инфо" (Custom_elt pid_flags_fmt)
      ; make_column ~sortable:true ~title:"Сервис" (Option (String, ""))
      ; make_column ~sortable:true ~title:"Битрейт, Мбит/с" br_fmt
      ; make_column ~sortable:true ~title:"%" pct_fmt
      ; make_column ~sortable:true ~title:"Min, Мбит/с" br_fmt
      ; make_column ~sortable:true ~title:"Max, Мбит/с" br_fmt ]

  let data_of_pid_info (pid, (info : PID.t)) : _ Fmt.data =
    let flags = {has_pcr = info.has_pcr; scrambled = info.scrambled} in
    Fmt.[pid; info.typ; flags; info.service_name; None; None; None; None]

  let create_empty_placeholder ?classes ?attrs () =
    Placeholder_markup.create
      ?classes
      ?attrs
      ~icon:(Icon_markup.SVG.create ~d:Svg_icons.emoticon_sad ())
      ~text:(`Text "Не найдено ни одного PID")
      ()

  let create ?(classes = []) ?(attrs = []) ?(dense = true) ?hex ?init () =
    let classes = CSS.root :: classes in
    let init, placeholder =
      match init with
      | None -> [], Some (create_empty_placeholder ())
      | Some ({data; _} : _ Board_niitv_tsan_types.ts) -> data, None
    in
    let table =
      Data_table_markup.create_of_fmt
        ~dense
        ~classes:[CSS.table]
        ~format:(create_table_format ?hex ())
        ~data:(List.map data_of_pid_info init)
        ()
    in
    div ~a:([a_class classes] @ attrs)
    @@ Utils.(
         [create_header ?hex (); Divider_markup.create_hr (); table] @ placeholder ^:: [])
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
