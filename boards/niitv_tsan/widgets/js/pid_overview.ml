open Js_of_ocaml
open Application_types
open Board_niitv_tsan_types
open Components

let name = "PID overview"

module Attr = struct
  let lost = "data-lost"

  let set_bool elt attr = function
    | true -> Element.set_attribute elt attr ""
    | false -> Element.remove_attribute elt attr
end

type pid_flags =
  { has_pcr : bool
  ; scrambled : bool }
[@@deriving ord]

let make_pid_flags_element {has_pcr; scrambled} =
  let pcr =
    match has_pcr with
    | false -> None
    | true -> Some Icon.SVG.(Markup_js.create ~d:Path.clock_outline ())
  in
  let scr =
    match scrambled with
    | false -> None
    | true -> Some Icon.SVG.(Markup_js.create ~d:Path.lock ())
  in
  let ( ^:: ) x l =
    match x with
    | None -> l
    | Some x -> x :: l
  in
  let children = scr ^:: pcr ^:: [] in
  Js_of_ocaml_tyxml.Tyxml_js.Html.toelt @@ Box.Markup_js.create ~children ()

let pid_type_fmt : MPEG_TS.PID.Type.t Gadt_data_table.Fmt_js.custom =
  MPEG_TS.PID.Type.
    {to_string; of_string = (fun _ -> assert false); compare; is_numeric = false}

let pid_flags_fmt : pid_flags Gadt_data_table.Fmt_js.custom_elt =
  { to_elt = make_pid_flags_element
  ; of_elt = (fun _ -> assert false)
  ; compare = compare_pid_flags
  ; is_numeric = false }

let hex_pid_fmt =
  Gadt_data_table.Fmt_js.Custom
    { to_string = Util.pid_to_hex_string
    ; of_string = int_of_string
    ; compare
    ; is_numeric = true }

let make_table_fmt ?(is_hex = false) () : _ Gadt_data_table.Fmt_js.format =
  let open Gadt_data_table in
  let br_fmt = Fmt_js.Option (Float, "-") in
  let pct_fmt = Fmt_js.Option (Float, "-") in
  let pid_fmt = if is_hex then hex_pid_fmt else Fmt_js.Int in
  Fmt_js.
    [ make_column ~sortable:true ~title:"PID" pid_fmt
    ; make_column ~sortable:true ~title:"Тип" (Custom pid_type_fmt)
    ; make_column ~title:"Доп. инфо" (Custom_elt pid_flags_fmt)
    ; make_column ~sortable:true ~title:"Сервис" (Option (String, ""))
    ; make_column ~sortable:true ~title:"Битрейт, Мбит/с" br_fmt
    ; make_column ~sortable:true ~title:"%" pct_fmt
    ; make_column ~sortable:true ~title:"Min, Мбит/с" br_fmt
    ; make_column ~sortable:true ~title:"Max, Мбит/с" br_fmt ]

let add_row (table : 'a Gadt_data_table.t) ((pid, info) : int * PID_info.t) =
  let open Gadt_data_table in
  let flags = {has_pcr = info.has_pcr; scrambled = info.scrambled} in
  let (data : _ Fmt_js.data) =
    Fmt_js.[pid; info.typ; flags; info.service_name; None; None; None; None]
  in
  let (_ : Dom_html.tableRowElement Js.t) = table#insert_row (-1) data in
  (* Attr.set_bool row#root Attr.lost info.present; *)
  ()

let update_row (table : 'a Gadt_data_table.t) row total br =
  let pct = 100. *. float_of_int br /. float_of_int total in
  let br = float_of_int br /. 1_000_000. in
  let min, max =
    Gadt_data_table.Fmt_js.(
      match table#get_row_data_lazy row with
      | [_; _; _; _; _; _; min; max] -> min, max)
  in
  let min =
    match min () with
    | None -> Some (Some br)
    | Some v -> if br < v then Some (Some br) else None
  in
  let max =
    match max () with
    | None -> Some (Some br)
    | Some v -> if br > v then Some (Some br) else None
  in
  let data =
    Gadt_data_table.Fmt_js.
      [None; None; None; None; Some (Some br); Some (Some pct); min; max]
  in
  table#set_row_data_some data row;
  br, pct
