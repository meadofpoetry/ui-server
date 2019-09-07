open Application_types
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
    | true -> Some Icon.SVG.(Markup_js.create_of_d Path.clock_outline)
  in
  let scr =
    match scrambled with
    | false -> None
    | true -> Some Icon.SVG.(Markup_js.create_of_d Path.lock)
  in
  let ( ^:: ) x l =
    match x with
    | None -> l
    | Some x -> x :: l
  in
  let widgets = scr ^:: pcr ^:: [] in
  Js_of_ocaml_tyxml.Tyxml_js.Html.toelt @@ Box.Markup_js.create widgets

let pid_type_fmt : MPEG_TS.PID.Type.t Data_table.Fmt.custom =
  MPEG_TS.PID.Type.
    {to_string; of_string = (fun _ -> assert false); compare; is_numeric = false}

let pid_flags_fmt : pid_flags Data_table.Fmt.custom_elt =
  { to_elt = make_pid_flags_element
  ; of_elt = (fun _ -> assert false)
  ; compare = compare_pid_flags
  ; is_numeric = false }

let dec_pid_fmt = Data_table.Fmt.Int

let hex_pid_fmt =
  Data_table.Fmt.Custom
    { to_string = Util.pid_to_hex_string
    ; of_string = int_of_string
    ; compare
    ; is_numeric = true }

let make_table_fmt ?(is_hex = false) () =
  let open Data_table in
  let open Data_table.Fmt in
  let open Format in
  let br_fmt = Option (Float, "-") in
  let pct_fmt = Option (Float, "-") in
  let pid_fmt = if is_hex then hex_pid_fmt else dec_pid_fmt in
  [ make_column ~sortable:true ~title:"PID" (), pid_fmt
  ; make_column ~sortable:true ~title:"Тип" (), Custom pid_type_fmt
  ; make_column ~title:"Доп. инфо" (), Custom_elt pid_flags_fmt
  ; make_column ~sortable:true ~title:"Сервис" (), Option (String, "")
  ; make_column ~sortable:true ~title:"Битрейт, Мбит/с" (), br_fmt
  ; make_column ~sortable:true ~title:"%" (), pct_fmt
  ; make_column ~sortable:true ~title:"Min, Мбит/с" (), br_fmt
  ; make_column ~sortable:true ~title:"Max, Мбит/с" (), br_fmt ]

(* let add_row (table : 'a Data_table.t) ((pid, info) : int * PID_info.t) =
 *   let open Data_table in
 *   let flags =
 *     { has_pcr = info.has_pcr
 *     ; scrambled = info.scrambled
 *     } in
 *   let data = Data.(
 *       pid :: info.typ :: flags :: info.service_name
 *       :: None :: None :: None :: None :: []) in
 *   let row = table#push data in
 *   Attr.set_bool row#root Attr.lost info.present;
 *   row *)

(* let update_row row total br =
 *   let cur, per, min, max =
 *     let open Data_table in
 *     match row#cells with
 *     | _ :: _ :: _ :: _ :: a :: b :: c :: d :: _ ->
 *       a, b, c, d in
 *   let pct = 100. *. (float_of_int br)
 *             /. (float_of_int total) in
 *   let br = (float_of_int br) /. 1_000_000. in
 *   cur#set_value @@ Some br;
 *   per#set_value @@ Some pct;
 *   (match min#value with
 *    | None -> min#set_value (Some br)
 *    | Some v -> if br < v then min#set_value (Some br));
 *   (match max#value with
 *    | None -> max#set_value (Some br)
 *    | Some v -> if br > v then max#set_value (Some br));
 *   br, pct *)
