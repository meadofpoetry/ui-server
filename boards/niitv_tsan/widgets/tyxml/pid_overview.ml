open Components_tyxml
open Application_types

module CSS = struct
  let root = Util.CSS.root ^ "-pid-overview"

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
  module Fmt = Data_table.Make_fmt (Xml)
  module Icon_markup = Icon.Make (Xml) (Svg) (Html)
  module Placeholder_markup = Components_lab_tyxml.Placeholder.Make (Xml) (Svg) (Html)

  let pid_type_fmt : MPEG_TS.PID.Type.t Fmt.custom =
    MPEG_TS.PID.Type.
      { to_string
      ; of_string = (fun _ -> failwith "Not implemented")
      ; compare
      ; is_numeric = false }

  let hex_pid_fmt =
    Fmt.Custom
      { to_string = Util.pid_to_hex_string
      ; of_string = int_of_string
      ; compare
      ; is_numeric = true }

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

  let create_table_format ?(is_hex = false) () : _ Data_table_markup.Fmt.format =
    let br_fmt = Fmt.Option (Float, "-") in
    let pct_fmt = Fmt.Option (Float, "-") in
    let pid_fmt = if is_hex then hex_pid_fmt else Fmt.Int in
    Fmt.
      [ make_column ~sortable:true ~title:"PID" pid_fmt
      ; make_column ~sortable:true ~title:"Тип" (Custom pid_type_fmt)
      ; make_column ~title:"Доп. инфо" (Custom_elt pid_flags_fmt)
      ; make_column ~sortable:true ~title:"Сервис" (Option (String, ""))
      ; make_column ~sortable:true ~title:"Битрейт, Мбит/с" br_fmt
      ; make_column ~sortable:true ~title:"%" pct_fmt
      ; make_column ~sortable:true ~title:"Min, Мбит/с" br_fmt
      ; make_column ~sortable:true ~title:"Max, Мбит/с" br_fmt ]

  let data_of_pid_info (pid, (info : Board_niitv_tsan_types.PID_info.t)) : _ Fmt.data =
    let flags = {has_pcr = info.has_pcr; scrambled = info.scrambled} in
    Fmt.[pid; info.typ; flags; info.service_name; None; None; None; None]

  let table_fmt = create_table_format ()

  let create_empty_placeholder ?classes ?attrs () =
    Placeholder_markup.create
      ?classes
      ?attrs
      ~icon:(Icon_markup.SVG.create ~d:Svg_icons.emoticon_sad ())
      ~text:(`Text "Не найдено ни одного PID")
      ()

  let create ?(classes = []) ?(attrs = []) ?dense ?init () =
    let classes = CSS.root :: classes in
    let init, placeholder =
      match init with
      | None -> [], Some (create_empty_placeholder ())
      | Some ({data; _} : _ Board_niitv_tsan_types.ts) -> data, None
    in
    let table =
      Data_table_markup.create_of_fmt
        ?dense
        ~classes:[CSS.table]
        ~format:table_fmt
        ~data:(List.map data_of_pid_info init)
        ()
    in
    div ~a:([a_class classes] @ attrs) Utils.(placeholder ^:: [table])
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
