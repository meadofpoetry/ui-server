open Components_tyxml
open Application_types
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-si-psi-overview"

  let bitrate_reset = BEM.add_element root "bitrate-reset"

  let table = BEM.add_element root "table"

  let menu_icon = BEM.add_element root "menu-icon"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Data_table_markup = Data_table.Make (Xml) (Svg) (Html)
  module Divider_markup = Divider.Make (Xml) (Svg) (Html)
  module Fmt = Data_table.Make_fmt (Xml)
  module Icon_button_markup = Icon_button.Make (Xml) (Svg) (Html)
  module Icon_markup = Icon.Make (Xml) (Svg) (Html)
  module Placeholder_markup = Components_lab_tyxml.Placeholder.Make (Xml) (Svg) (Html)

  let dec_pid_fmt = Fmt.Int

  let hex_pid_fmt =
    Fmt.Custom
      { to_string = Util.pid_to_hex_string
      ; of_string = int_of_string
      ; compare
      ; is_numeric = true }

  let pid_fmt ~hex = if hex then hex_pid_fmt else dec_pid_fmt

  let pct_fmt =
    Fmt.Custom
      { to_string = (fun x -> Printf.sprintf "%.2f" x)
      ; of_string = float_of_string
      ; compare
      ; is_numeric = true }

  (** Returns HTML element to insert into 'Extra' table column *)
  let create_table_id_ext ?(hex = false) (id : SI_PSI_table.id) =
    let to_id_string =
      match hex with
      | true -> Printf.sprintf "0x%02X"
      | false -> Printf.sprintf "%d"
    in
    let specific =
      match MPEG_TS.SI_PSI.of_table_id id.table_id with
      | `PAT -> ["tsid", id.table_id_ext]
      | `PMT -> ["program", id.table_id_ext]
      | `NIT _ -> ["network_id", id.table_id_ext]
      | `SDT _ -> ["tsid", id.table_id_ext; "onid", id.id_ext_1]
      | `BAT -> ["bid", id.table_id_ext]
      | `EIT _ -> ["onid", id.id_ext_1; "tsid", id.id_ext_2; "sid", id.table_id_ext]
      | _ -> []
    in
    let data = Yojson.Safe.to_string (SI_PSI_table.id_to_yojson id) in
    let length = List.length specific in
    span
      ~a:[a_user_data "id" data]
      (List.mapi
         (fun i (s, v) ->
           let v = to_id_string v in
           let v = if i = pred length then v else v ^ ", " in
           span [span ~a:[a_class [Typography.CSS.subtitle2]] [txt (s ^ ": ")]; txt v])
         specific)

  let id_ext_fmt ~hex =
    Fmt.Custom_elt
      { is_numeric = false
      ; compare = SI_PSI_table.compare_id
      ; to_elt = toelt % create_table_id_ext ~hex
      ; of_elt = (fun _ -> assert false) }

  let create_table_format ?(hex = false) () : _ Data_table_markup.Fmt.format =
    let br_fmt = Fmt.Option (Float, "-") in
    let id_ext_fmt = id_ext_fmt ~hex in
    Fmt.
      [ make_column ~sortable:true ~title:"ID" (pid_fmt ~hex)
      ; make_column ~sortable:true ~title:"PID" (pid_fmt ~hex)
      ; make_column ~sortable:true ~title:"Имя" String
      ; make_column ~title:"Доп. инфо" id_ext_fmt
      ; make_column ~sortable:true ~title:"Версия" Int
      ; make_column ~sortable:true ~title:"Сервис" (Option (String, ""))
      ; make_column ~title:"Кол-во секций" Int
      ; make_column ~title:"LSN" Int
      ; make_column ~title:"Битрейт, Мбит/с" br_fmt
      ; make_column ~title:"%" pct_fmt
      ; make_column ~title:"Min, Мбит/с" br_fmt
      ; make_column ~title:"Max, Мбит/с" br_fmt ]

  let create_empty_placeholder ?classes ?attrs () =
    Placeholder_markup.create
      ?classes
      ?attrs
      ~icon:(Icon_markup.SVG.create ~d:Svg_icons.emoticon_sad ())
      ~text:(`Text "Не найдено ни одного PID")
      ()

  let create ?(classes = []) ?(attrs = []) () =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] @ attrs) []
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
