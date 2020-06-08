open Components_tyxml

let id = "time-config"

let time_id = "time"

let date_id = "date"

module type Clock = sig
  val current_tz_offset_s : unit -> int option
end

module Make
    (Clock : Clock)
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Textfield.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let create ?classes ?(a = []) (v : Pc_control_types.Timedate_config.t) =
    let (y, m, d), ((hours, minutes, _), _) =
      Ptime.to_date_time
        ?tz_offset_s:(Clock.current_tz_offset_s ())
        v.local_time
    in
    let date =
      let date_input_id = date_id ^ "-input" in
      let date = Printf.sprintf "%04d-%02d-%02d" y m d in
      print_endline date;
      textfield ~typ:`Date ~a:[ a_id date_id ] ~input_id:date_input_id
        ~label:"Дата" ~value:date ()
    in
    let time =
      let time_input_id = time_id ^ "-input" in
      let time = Printf.sprintf "%02d:%02d" hours minutes in
      textfield ~typ:`Time ~a:[ a_id time_id ] ~input_id:time_input_id
        ~label:"Время" ~value:time ()
    in
    let header =
      create_section_header ~title:(`Text "Дата и время") ()
    in
    create_section ?classes ~a:(a_id id :: a) ~header ~children:[ date; time ]
      ()
end
