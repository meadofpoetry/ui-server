open Board_types
open Containers

type table_info =
  { section : int
  ; id      : int
  ; id_ext  : int
  }

type ts_error_hr =
  { name        : string
  ; priority    : [ `P1 | `P2 | `P3 ]
  ; description : string
  ; count       : int
  ; multi_pid   : bool
  ; pid         : int
  ; service     : string option
  ; packet      : int32
  }

let table_info_of_ts_error (e:ts_error) =
  { section = Int32.to_int @@ Int32.((e.param_2 land 0xFF00_0000l) lsr 24)
  ; id      = Int32.to_int @@ Int32.((e.param_2 land 0x00FF_0000l) lsr 16)
  ; id_ext  = Int32.to_int @@ Int32.(e.param_2 land 0x0000_FFFFl)
  }

module Description = struct

  type interval      = [`Seconds of float | `Milliseconds of float | `Nanoseconds of float |
                        `Seconds_unk      | `Milliseconds_unk      | `Nanoseconds_unk
                       ]
  type possible_pids = [`One of int | `List of int list | `Range of int * int | `Unknown ]

  let possible_si_pids : Structure_types.table_label -> possible_pids = function
    | `PAT -> `One 0x00
    | `CAT -> `One 0x01
    | `PMT -> `One 0x02
    | `NIT -> `List [0x40;0x41;0x72]
    | `SDT -> `List [0x42;0x46;0x4A;0x72]
    | `EIT -> `Range (0x4E,0x6F)
    | `RST -> `List [0x71;0x72]
    | `TDT -> `List [0x70;0x72;0x73]
    | _    -> `Unknown

  let table_id_to_interval : int -> interval = function
    | x when x >= 0x00 && x <= 0x02    -> `Seconds 0.5
    | 0x42 | 0x4E                      -> `Seconds 2.0
    | 0x40 | 0x41 | 0x46 | 0x4A | 0x4F -> `Seconds 10.0
    | x when x >= 0x50 && x <= 0x70    -> `Seconds 30.0
    | 0x73                             -> `Seconds 30.0
    | _                                -> `Seconds_unk

  let table_name = Structure_types.table_label_to_string

  let period_to_unit_name : interval -> string = function
    | `Seconds _      | `Seconds_unk      -> "с"
    | `Milliseconds _ | `Milliseconds_unk -> "мс"
    | `Nanoseconds _  | `Nanoseconds_unk  -> "нс"

  let period_to_float = function
    | `Seconds x | `Milliseconds x | `Nanoseconds x -> x | _ -> 0.0

  let crc_err ?(hex=false) ?(short=false) ?table (e:ts_error) =
    let base = "Ошибка CRC" in
    let prefix = match table with
      | Some t -> Printf.sprintf "%s в %s" base @@ table_name t
      | None   -> base
    in
    if short then prefix
    else let to_s = Printf.sprintf (if hex then "0x%08lX" else "%lu") in
         let computed = to_s e.param_1 in
         let actual   = to_s e.param_2 in
         Printf.sprintf "%s, CRC = %s, должно быть %s" prefix computed actual

  let interval_err ?(short=false) ~prefix ~cmp_word ~period (e:ts_error) =
    let unit = period_to_unit_name period in
    let got  = Int32.to_float e.param_1 *. 0.1 in
    match period with
    | `Seconds_unk | `Milliseconds_unk | `Nanoseconds_unk ->
       Printf.sprintf "%s %g %s" prefix got unit
    | _        ->
       let must = period_to_float period in
       let ext  = Printf.sprintf "%s %g %s" cmp_word must unit in
       if short then Printf.sprintf "%s %s" prefix ext
       else Printf.sprintf "%s %g %s (%s)" prefix got unit ext

  let table_short_interval ?(short=false) ~period (e:ts_error) table =
    let prefix = Printf.sprintf "Период следования таблицы %s -" @@ table_name table in
    interval_err ~short ~prefix ~cmp_word:"менее" ~period e

  let table_long_interval ?(short=false) ~period (e:ts_error) table =
    let prefix = Printf.sprintf "Таблица %s отсутствует в потоке" @@ table_name table in
    interval_err ~short ~prefix ~cmp_word:"более" ~period e

  let table_scrambled table =
    Printf.sprintf "Таблица %s скремблирована" @@ table_name table

  let table_crc ?short ?hex e table = crc_err ?short ?hex ~table e

  let table_id ?(short=false) ?(hex=false) (e:ts_error) table =
    let to_s     = if hex then Printf.sprintf "0x%02X" else Printf.sprintf "%u" in
    let got      = to_s @@ Int32.to_int e.param_1 in
    let ppids    = possible_si_pids table in
    let possible = match ppids with
      | `One x       -> to_s x
      | `List l      -> List.map (fun x -> to_s x) l |> String.concat ", "
      | `Range (f,t) -> Printf.sprintf "%s .. %s" (to_s f) (to_s t)
      | `Unknown     -> ""
    in
    match ppids with
    | `Unknown -> Printf.sprintf "Поле table_id = %s" got
    | _        -> if short then Printf.sprintf "Поле table_id не равно %s" possible
                  else Printf.sprintf "Поле table_id = %s, должно быть %s" got possible

  let table_ext_unknown _ = ""

  let of_ts_error ?(hex=true) ?(short=false) (e:ts_error) = match e.err_code with
    (* First priority *)
    | 0x11 -> "Пропадание синхронизации"
    | 0x12 -> let s = Printf.sprintf (if hex then "0x%02X" else "%u") 0x47 in
              "Байт синхронизации не равен " ^ s
    | 0x13 -> let f = match e.err_ext with
                | 0x31 -> table_scrambled
                | 0x32 -> table_id ~short ~hex e
                | 0x33 -> table_long_interval ~short ~period:(`Seconds 0.5) e
                | 0x35 -> table_crc ~short ~hex e
                | _    -> table_ext_unknown
              in f `PAT
    | 0x14 -> (match e.err_ext with
               | 0x31 -> "Повторение пакета более двух раз"
               | 0x32 -> "Потеря пакета (пакетов)"
               | 0x33 -> "Неправильный порядок пакетов"
               | 0x34 -> "Счётчик изменился при отсутствии полезной нагрузки"
               | 0x35 -> "Пакеты с повторившимся 'continuity_counter' не идентичны"
               | _    -> "")
    | 0x15 -> let f = match e.err_ext with
                | 0x31 -> table_scrambled
                | 0x32 -> table_id ~short ~hex e
                | 0x33 -> table_long_interval ~short ~period:(`Seconds 0.5) e
                | 0x34 -> table_crc ~short ~hex e
                | _    -> table_ext_unknown
              in f `PMT
    | 0x16 -> "В потоке отсутствует PID"
    (* Second priority *)
    | 0x21 -> "В пакете установлен флаг transport_error_indicator"
    | 0x22 -> crc_err ~short ~hex e
    | 0x23 -> (match e.err_ext with
               | 0x31 -> let prefix = "Период повторения PCR" in
                         interval_err ~prefix ~cmp_word:"больше" ~period:(`Milliseconds 40.) e
               | 0x32 -> interval_err ~prefix:"Разрыв PCR" ~cmp_word:"больше" ~period:(`Milliseconds 100.) e
               | 0x33 -> Printf.sprintf "Отсутствует PCR для программы %04u" @@ Int32.to_int e.param_1
               | _    -> "")
    | 0x24 -> interval_err ~prefix:"Неравномерность PCR -" ~cmp_word:"больше" ~period:(`Nanoseconds 500.) e
    | 0x25 -> interval_err ~prefix:"Период повторения PTS -" ~cmp_word:"больше" ~period:(`Milliseconds 700.) e
    | 0x26 -> let f = match e.err_ext with
                | 0x31 -> table_scrambled
                | 0x32 -> table_id ~short ~hex e
                | 0x33 -> table_crc ~short ~hex e
                | 0x34 -> fun _ -> "Есть скремблирование, но нет CAT"
                | _    -> table_ext_unknown
              in f `CAT
    (* Third priority *)
    | 0x31 -> let f = match e.err_ext with
                | 0x31 -> table_id ~short ~hex e
                | 0x32 -> table_long_interval ~short ~period:(`Seconds 10.) e
                | 0x34 -> table_scrambled
                | _    -> table_ext_unknown
              in f `NIT
    | 0x32 -> let table_info = table_info_of_ts_error e in
              let f = match e.err_ext with
               | 0x31 -> table_short_interval ~short ~period:(`Milliseconds 25.) e
               | 0x32 -> table_long_interval ~short ~period:(table_id_to_interval table_info.id) e
               | _    -> table_ext_unknown
              in f @@ Structure_types.table_label_of_int table_info.id
    | 0x34 -> "Пакет с неизвестным PID"
    | 0x35 -> let f = match e.err_ext with
                | 0x31 -> table_id ~short ~hex e
                | 0x32 -> table_long_interval ~short ~period:(`Seconds 2.) e
                | 0x34 -> table_scrambled
                | _    -> table_ext_unknown
              in f `SDT
    | 0x36 -> let f = match e.err_ext with
                | 0x31 -> table_id ~short ~hex e
                | 0x32 -> table_long_interval ~short ~period:(`Seconds 2.) e
                | _    -> table_ext_unknown
              in f `EIT
    | 0x37 -> let f = match e.err_ext with
                | 0x31 -> table_id ~short ~hex e
                | 0x32 -> table_scrambled
                | _    -> table_ext_unknown
              in f `RST
    | 0x38 -> let f = match e.err_ext with
                | 0x31 -> table_id ~short ~hex e
                | 0x32 -> table_long_interval ~short ~period:(`Seconds 30.) e
                | 0x33 -> table_scrambled
                | _    -> table_ext_unknown
              in f `TDT
    | _    -> assert false


end

let ts_error_to_error_name (e:ts_error) = match e.err_code with
  | 0x11 -> "TS sync loss"
  | 0x12 -> "Sync byte error"
  | 0x13 -> "PAT error"
  | 0x14 -> "Continuity count error"
  | 0x15 -> "PMT error"
  | 0x16 -> "PID error"
  | 0x21 -> "Transport error"
  | 0x22 -> "CRC error"
  | 0x23 -> "PCR error"
  | 0x24 -> "PCR accuracy error"
  | 0x25 -> "PTS error"
  | 0x26 -> "CAT error"
  | 0x31 -> "NIT error"
  | 0x32 -> "SI repetition error"
  | 0x34 -> "Unreferenced pid"
  | 0x35 -> "SDT error"
  | 0x36 -> "EIT error"
  | 0x37 -> "RST error"
  | 0x38 -> "TDT error"
  | _    -> assert false

let ts_error_to_priority (e:ts_error) = match e.err_code with
  | x when x >= 0x11 && x <= 0x16 -> `P1
  | x when x >= 0x21 && x <= 0x26 -> `P2
  | x when x >= 0x31 && x <= 0x38 -> `P3
  | _ -> assert false

let priority_name = function
  | `P1 -> "1 приоритет"
  | `P2 -> "2 приоритет"
  | `P3 -> "3 приоритет"

let convert_ts_error ?hex ?short (e:ts_error) : ts_error_hr =
  { name        = ts_error_to_error_name e
  ; priority    = ts_error_to_priority e
  ; description = Description.of_ts_error ?hex ?short e
  ; count       = e.count
  ; multi_pid   = e.multi_pid
  ; pid         = e.pid
  ; service     = None
  ; packet      = e.packet
  }
