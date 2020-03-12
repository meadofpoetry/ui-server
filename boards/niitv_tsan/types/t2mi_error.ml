open Board_niitv_tsan_types
open Application_types

let etr290_error_of_code : int -> T2MI.ETR290_error.t option = function
  | 0x00 -> Some T2MI_packet_type_error_1
  | 0x01 -> Some T2MI_packet_type_error_2
  | 0x02 -> Some T2MI_packet_count_error
  | 0x03 -> Some T2MI_CRC_error
  | 0x04 -> Some T2MI_payload_error
  | 0x05 -> Some T2MI_plp_num_blocks_error
  | 0x06 -> Some T2MI_transmission_order_error
  | 0x07 -> Some T2MI_DVB_T2_timestamp_error
  | 0x08 -> Some T2MI_DVB_T2_timestamp_discontinuity
  | 0x09 -> Some T2MI_T2_frame_length_error
  | 0x14 -> Some T2MI_DVB_T2_signalling_inconsistency_error
  | _ -> None

let name_of_code = function
  | 0xF0 -> "T2-MI over TS parser error"
  | 0xF1 -> "T2-MI parser error"
  | code -> (
      match etr290_error_of_code code with
      | None -> "Unknown T2-MI error"
      | Some e ->
          Printf.sprintf "%s %s"
            (T2MI.ETR290_error.number e)
            (T2MI.ETR290_error.name e) )

module Info = struct
  let of_error (e : 'a Error.e) =
    match e.err_code with
    | 0x00 -> ""
    | 0x01 -> ""
    | 0x04 ->
        let plp = Int32.logand 0xFFl e.param_1 in
        Printf.sprintf "Обнаружен BB-кадр с PLP_ID = %ld" plp
    | 0x09 ->
        let sec = e.param_1 in
        Printf.sprintf
          "Длительность T2-кадра равна %ld мс \
           (больше 250 мс)"
          sec
    | 0xF0 -> (
        match e.err_ext with
        | 0 ->
            "Нет места для pointer_field и начала T2-MI \
             пакета"
        | 1 ->
            "Длина поля адаптации больше 182 байт"
        | 2 ->
            "pointer_field указывает за пределы пакета"
        | 3 -> "Наложение T2-MI пакетов"
        | _ -> "" )
    | 0xF1 -> (
        match e.err_ext with
        | 0 -> "Поток предназначен не для DVB-T2"
        | 1 ->
            "В L1 обнаружен код FFT, не разрешённый \
             для режима T2-Lite"
        | 2 ->
            "Некорректное значение защитного \
             интервала"
        | 3 ->
            "Значение защитного интервала не \
             совместимо с режимом FFT"
        | 4 ->
            "Количество T2-кадров в суперкадре \
             меньше двух"
        | 5 ->
            "Длина поля L1_Post_Configurable слишком \
             велика"
        | 6 -> "Длина поля L1_Dynamic слишком велика"
        | 7 | 8 ->
            "Длина пакета \"L1-Current\" слишком мала"
        | 9 ->
            "В пакете \"L1-Current\" отсутствует поле \
             \"Dynamic\""
        | 10 -> "Некорректная длина поля \"Dynamic\""
        | 11 -> "Длина пакета \"L1-Future\" слишком мала"
        | 12 ->
            "Получен пакет \"L1-Future\", не \
             предусмотренный режимом"
        | 13 ->
            "Длина пакета \"DVB-T2 Timestamp\" не равна 11 \
             байт"
        | 15 ->
            "Пакет \"L1-Current\" отсутствует в потоке \
             более 3 секунд"
        | _ -> "" )
    | _ -> ""
end
