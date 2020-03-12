open Components_tyxml
open Application_types

module CSS = struct
  let root = "application-log"

  let table = BEM.add_element root "table"

  let footer = BEM.add_element root "footer"

  let has_more = BEM.add_element root "has-more"

  let entry = BEM.add_element root "entry"

  let entry_log_level level =
    BEM.add_modifier entry (Stream.Log_message.level_to_string level)
end

let log_level_to_human_string : Stream.Log_message.level -> string = function
  | Info -> "Инфо"
  | Warn -> "Предупреждение"
  | Err -> "Ошибка"
  | Fatal -> "Авария"

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module Fmt = Data_table.Make_fmt (Xml) (Svg) (Html)
  module Data_table_markup = Data_table.Make (Xml) (Svg) (Html)
  module Button_markup = Button.Make (Xml) (Svg) (Html)
  module Card_markup = Card.Make (Xml) (Svg) (Html)

  let timestamp_fmt =
    Fmt.Custom
      {
        to_string = Time.to_human_string;
        of_string = (fun _ -> assert false) (* TODO *);
        compare = Ptime.compare;
        is_numeric = false;
      }

  let pid_to_hex_string = Printf.sprintf "0x%04X"

  let dec_pid_fmt = Fmt.Int

  let hex_pid_fmt =
    Fmt.Custom
      {
        to_string = pid_to_hex_string;
        of_string = int_of_string;
        compare;
        is_numeric = true;
      }

  let pid_fmt ~hex =
    let open Stream.Log_message in
    let to_string (pid : pid) =
      let f = if hex then Printf.sprintf "0x%04X" else Printf.sprintf "%04d" in
      let s = f pid.id in
      match pid.typ with
      | None -> s
      | Some typ -> Printf.sprintf "%s - %s" s typ
    in
    let of_string _ = assert false in
    let compare { id = a; _ } { id = b; _ } = Int.compare a b in
    Fmt.Custom { to_string; of_string; compare; is_numeric = false }

  let create_table_format ?(hex = return false) () : _ Fmt.format =
    let pid_fmt = fmap (fun hex -> Fmt.Option (pid_fmt ~hex, "-")) hex in
    Fmt.
      [
        make_column ~sortable:true ~title:(return "Время")
          (return timestamp_fmt);
        make_column ~sortable:true ~title:(return "Уровень")
          (return Fmt.String);
        make_column ~title:(return "Событие") (return Fmt.String);
        make_column ~sortable:true ~title:(return "PID") pid_fmt;
        make_column ~sortable:true ~title:(return "Сервис")
          (return Fmt.(Option (String, "-")));
        make_column ~title:(return "Подробности") (return Fmt.String);
      ]

  let data_of_log_entry (entry : Stream.Log_message.t) : _ Fmt.data =
    Fmt.
      [
        return entry.time;
        return (log_level_to_human_string entry.level);
        return entry.message;
        return entry.pid;
        return entry.service;
        return entry.info;
      ]

  let row_of_log_entry ~format (entry : Stream.Log_message.t) =
    let data = data_of_log_entry entry in
    let cells = Data_table_markup.data_table_cells_of_fmt format data in
    Data_table_markup.data_table_row
      ~classes:(return [ CSS.entry; CSS.entry_log_level entry.level ])
      ~children:cells ()

  let create_footer ?a ?(classes = return []) ?(has_more = false) () =
    let classes = fmap (fun x -> CSS.footer :: x) classes in
    let more =
      Button_markup.button
        ~classes:(return [ CSS.has_more ])
        ~disabled:(not has_more)
        ~label:(return "Загрузить еще")
        ()
    in
    let actions =
      Card_markup.card_action_buttons ~children:(Xml.Wutils.const [ more ]) ()
    in
    Card_markup.card_actions ?a ~classes
      ~children:(Xml.Wutils.const [ actions ])
      ()

  let create ?(a = []) ?(classes = return []) ?(dense = true)
      ?(hex = return false) ?has_more ?(init = nil ()) () =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let format = create_table_format ~hex () in
    let rows = Xml.W.map (fun x -> row_of_log_entry ~format x) init in
    let table =
      Data_table_markup.data_table_of_fmt
        ~classes:(return [ CSS.table ])
        ~dense ~format ~rows
        ~footer:(create_footer ?has_more ())
        ()
    in
    let children = Xml.Wutils.const [ table ] in
    Card_markup.card ~a:(a_class classes :: a) ~children ()
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
