open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  include Table_overview.CSS

  let services = BEM.add_modifier root "services"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  include Table_overview.Make (Xml) (Svg) (Html)

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
      [ make_column ~sortable:true ~title:"ID" id_fmt
      ; make_column ~sortable:true ~title:"Сервис" String
      ; make_column ~sortable:true ~title:"PMT PID" id_fmt
      ; make_column ~sortable:true ~title:"PCR PID" id_fmt
      ; make_column ~sortable:true ~title:"Битрейт, Мбит/с" br_fmt
      ; make_column ~sortable:true ~title:"%" pct_fmt
      ; make_column ~sortable:true ~title:"Min, Мбит/с" br_fmt
      ; make_column ~sortable:true ~title:"Max, Мбит/с" br_fmt ]

  let data_of_service_info (id, (info : Service.t)) : _ Fmt.data =
    Fmt.[id; info.name; info.pmt_pid; info.pcr_pid; None; None; None; None]

  let table_fmt = create_table_format ()

  let create_info_header ?(classes = []) ?(attrs = []) ?service_name ?children () =
    let children =
      match children with
      | Some x -> x
      | None ->
          let back =
            Icon_button_markup.create
              ~icon:(Icon_markup.SVG.create ~d:Svg_icons.arrow_left ())
              ()
          in
          let title = Option.map (fun name -> span [txt name]) service_name in
          Utils.(back :: (title ^:: []))
    in
    div ~a:([a_class classes] @ attrs) children

  let create ?(classes = []) ?attrs ?dense ?hex ?init ~control () =
    let classes = CSS.services :: classes in
    let data =
      match init with
      | None -> []
      | Some ({data; _} : _ ts) -> List.map data_of_service_info data
    in
    create
      ~classes
      ?attrs
      ?dense
      ?hex
      ~title:(`Text "Список сервисов")
      ~format:(create_table_format ?hex ())
      ~with_details:true
      ~data
      ~control
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
