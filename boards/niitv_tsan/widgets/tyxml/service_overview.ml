open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  include Table_overview.CSS

  let services = BEM.add_modifier root "services"
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  include Table_overview.Make (Xml) (Svg) (Html)
  module Service_info_markup = Service_info.Make (Xml) (Svg) (Html)
  module Fmt = Data_table.Make_fmt (Xml) (Svg) (Html)

  let dec_id_fmt = Fmt.Int

  let hex_id_fmt =
    Fmt.Custom
      { to_string = Util.pid_to_hex_string
      ; of_string = int_of_string
      ; compare
      ; is_numeric = true
      }

  let id_fmt ~hex = if hex then hex_id_fmt else dec_id_fmt

  let br_fmt =
    Fmt.Custom
      { to_string = (fun x -> Printf.sprintf "%f" (float_of_int x /. 1_000_000.))
      ; of_string = (fun x -> int_of_float (float_of_string x *. 1_000_000.))
      ; compare
      ; is_numeric = true
      }

  let pct_fmt =
    Fmt.Custom
      { to_string = (fun x -> Printf.sprintf "%.2f" x)
      ; of_string = float_of_string
      ; compare
      ; is_numeric = true
      }

  let create_table_format ?(hex = return false) () : _ Fmt.format =
    let id_fmt = fmap (fun hex -> id_fmt ~hex) hex in
    let br_fmt = return (Fmt.Option (br_fmt, "-")) in
    let pct_fmt = return (Fmt.Option (pct_fmt, "-")) in
    Fmt.
      [ make_column ~sortable:true ~title:(return "ID") id_fmt
      ; make_column ~sortable:true ~title:(return "Сервис") (return String)
      ; make_column ~sortable:true ~title:(return "PMT PID") id_fmt
      ; make_column ~sortable:true ~title:(return "PCR PID") id_fmt
      ; make_column ~sortable:true ~title:(return "Битрейт, Мбит/с") br_fmt
      ; make_column ~sortable:true ~title:(return "%") pct_fmt
      ; make_column ~sortable:true ~title:(return "Min, Мбит/с") br_fmt
      ; make_column ~sortable:true ~title:(return "Max, Мбит/с") br_fmt
      ]

  let data_of_service_info ?(bitrate = return None) (id, (info : Service.t)) : _ Fmt.data
      =
    Fmt.
      [ return id
      ; return info.name
      ; return info.pmt_pid
      ; return info.pcr_pid
      ; fmap
          (function
            | None -> None
            | Some (_, { Bitrate.cur; _ }) -> Some cur)
          bitrate
      ; fmap
          (function
            | None -> None
            | Some ({ Bitrate.cur = tot; _ }, { Bitrate.cur; _ }) ->
                Some (100. *. float_of_int cur /. float_of_int tot))
          bitrate
      ; fmap
          (function
            | None -> None
            | Some (_, { Bitrate.min; _ }) -> Some min)
          bitrate
      ; fmap
          (function
            | None -> None
            | Some (_, { Bitrate.max; _ }) -> Some max)
          bitrate
      ]

  let row_of_service_info ?bitrate ~format (id, (info : Service.t)) =
    let data = data_of_service_info ?bitrate (id, info) in
    let cells = Data_table_markup.data_table_cells_of_fmt format data in
    let value = Yojson.Safe.to_string (Service.to_yojson info) in
    Data_table_markup.data_table_row
      ~a:
        [ Html.a_user_data "value" (return value)
        ; Html.a_user_data "id" (return (string_of_int id))
        ]
      ~children:cells
      ()

  let table_fmt = create_table_format ()

  let create
      ?a
      ?dense
      ?(hex = return false)
      ?(bitrate = return None)
      ?(init = nil ())
      ?pids
      ?(selected = return None)
      ~control
      () =
    let format = create_table_format ~hex () in
    let title =
      fmap
        (function
          | None -> "Список сервисов"
          | Some (_, (x : Board_niitv_tsan_types.Service.t)) -> x.name)
        selected
    in
    let service_info =
      Service_info_markup.create ?pids ~hex ~info:selected ~bitrate ~control ()
    in
    let details =
      fmap
        (function
          | None -> None
          | Some x -> Some service_info)
        selected
    in
    let rows =
      Xml.W.map
        (fun ((id, _) as x) ->
          let bitrate =
            fmap
              (function
                | None -> None
                | Some (rate : Bitrate.ext) -> (
                    match List.assoc_opt id rate.services with
                    | None -> None
                    | Some x -> Some (rate.total, x)))
              bitrate
          in
          row_of_service_info ~bitrate ~format x)
        init
    in
    create
      ~classes:(return [ CSS.services ])
      ?a
      ?dense
      ~details
      ~hex
      ~title
      ~format
      ~rows
      ~control
      ()
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
