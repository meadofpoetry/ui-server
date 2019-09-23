open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  include Table_overview.CSS

  let services = BEM.add_modifier root "services"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  include Table_overview.Make (Xml) (Svg) (Html)
  module Fmt = Data_table.Make_fmt (Xml) (Svg) (Html)

  let dec_id_fmt = Fmt.Int

  let hex_id_fmt =
    Fmt.Custom
      { to_string = Util.pid_to_hex_string
      ; of_string = int_of_string
      ; compare
      ; is_numeric = true }

  let id_fmt ~hex = if hex then hex_id_fmt else dec_id_fmt

  let br_fmt =
    Fmt.Custom
      { to_string = (fun x -> Printf.sprintf "%f" (float_of_int x /. 1_000_000.))
      ; of_string = (fun x -> int_of_float (float_of_string x *. 1_000_000.))
      ; compare
      ; is_numeric = true }

  let pct_fmt =
    Fmt.Custom
      { to_string = (fun x -> Printf.sprintf "%.2f" x)
      ; of_string = float_of_string
      ; compare
      ; is_numeric = true }

  let create_table_format ?(hex = false) () : _ Fmt.format =
    let id_fmt = id_fmt ~hex in
    let br_fmt = Fmt.Option (br_fmt, "-") in
    let pct_fmt = Fmt.Option (pct_fmt, "-") in
    Fmt.
      [ make_column ~sortable:true ~title:(return "ID") id_fmt
      ; make_column ~sortable:true ~title:(return "Сервис") String
      ; make_column ~sortable:true ~title:(return "PMT PID") id_fmt
      ; make_column ~sortable:true ~title:(return "PCR PID") id_fmt
      ; make_column ~sortable:true ~title:(return "Битрейт, Мбит/с") br_fmt
      ; make_column ~sortable:true ~title:(return "%") pct_fmt
      ; make_column ~sortable:true ~title:(return "Min, Мбит/с") br_fmt
      ; make_column ~sortable:true ~title:(return "Max, Мбит/с") br_fmt ]

  let data_of_service_info (id, (info : Service.t)) : _ Fmt.data =
    Fmt.
      [ return id
      ; return info.name
      ; return info.pmt_pid
      ; return info.pcr_pid
      ; return None
      ; return None
      ; return None
      ; return None ]

  let table_fmt = create_table_format ()

  let create ?a ?dense ?hex ?(init = nil ()) ~control () =
    let data = Xml.W.map data_of_service_info init in
    create
      ~classes:(return [CSS.services])
      ?a
      ?dense
      ?hex
      ~title:(return "Список сервисов")
      ~format:(create_table_format ())
      ~with_details:true
      ~data
      ~control
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
