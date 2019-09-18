open Components_tyxml
open Application_types
open Board_niitv_tsan_types

module CSS = struct
  include Table_overview.CSS

  let pids = BEM.add_modifier root "pids"
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
  include Table_overview.Make (Xml) (Svg) (Html)

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

  let create ?(classes = []) ?attrs ?dense ?hex ?init ~control () =
    let classes = CSS.pids :: classes in
    let data =
      match init with
      | None -> []
      | Some ({data; _} : _ ts) -> List.map data_of_pid_info data
    in
    create
      ~classes
      ?attrs
      ?dense
      ?hex
      ~title:(`Text "Список PID")
      ~format:(create_table_format ?hex ())
      ~data
      ~control
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
