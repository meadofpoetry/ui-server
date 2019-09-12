open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-service-overview"

  let table = BEM.add_element root "table"

  let no_sync = BEM.add_modifier root "no-sync"

  let no_response = BEM.add_modifier root "no-response"

  let info_header = BEM.add_element root "info-header"

  let service_name = BEM.add_element root "service-name"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Data_table_markup = Data_table.Make (Xml) (Svg) (Html)
  module Fmt = Data_table.Make_fmt (Xml)
  module Icon_markup = Icon.Make (Xml) (Svg) (Html)
  module Icon_button_markup = Icon_button.Make (Xml) (Svg) (Html)
  module Placeholder_markup = Components_lab_tyxml.Placeholder.Make (Xml) (Svg) (Html)

  let hex_id_fmt =
    Fmt.Custom
      { to_string = Util.pid_to_hex_string
      ; of_string = int_of_string
      ; compare
      ; is_numeric = true }

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

  let create_table_format ?(is_hex = false) () : _ Fmt.format =
    let br_fmt = Fmt.Option (br_fmt, "-") in
    let pct_fmt = Fmt.Option (pct_fmt, "-") in
    let id_fmt = if is_hex then hex_id_fmt else Fmt.Int in
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

  let create_empty_placeholder ?classes ?attrs () =
    Placeholder_markup.create
      ?classes
      ?attrs
      ~icon:(Icon_markup.SVG.create ~d:Svg_icons.emoticon_sad ())
      ~text:(`Text "Не найдено ни одного сервиса")
      ()

  let create_info_header ?(classes = []) ?(attrs = []) ?service_name ?children () =
    let classes = CSS.info_header :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let back =
            Icon_button_markup.create
              ~icon:(Icon_markup.SVG.create ~d:Svg_icons.arrow_left ())
              ()
          in
          let title =
            Option.map
              (fun name -> span ~a:[a_class [CSS.service_name]] [txt name])
              service_name
          in
          Utils.(back :: (title ^:: []))
    in
    div ~a:([a_class classes] @ attrs) children

  let create ?(classes = []) ?(attrs = []) ?(dense = true) ?init () =
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
        ~format:table_fmt
        ~data:(List.map data_of_service_info init)
        ()
    in
    div ~a:([a_class classes] @ attrs) @@ List.rev Utils.(placeholder ^:: [table])
end
