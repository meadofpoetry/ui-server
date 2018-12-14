open Containers
open Common.Stream.Log_message

(* TODO merge Audio and Video *)

let id_in id list =
  List.exists (Common.Stream.ID.equal id) list
   
module Video = struct
  open Qoe_errors.Video_data

  (* *)

  let convert sources structures x =
    let (_, stream) = List.find
                        (fun (_,s) -> Common.Stream.(ID.equal s.id x.stream))
                        !sources
    in
    let input     = Common.Stream.get_input stream in
    let stream    = Some x.stream in
    let service   =
      let open Option.Infix in
      let open Structure in
      React.S.value structures
      |> List.find_opt (fun s -> Common.Stream.ID.equal s.id x.stream)
      >>= fun s ->
      List.find_opt (fun channel -> channel.number = x.channel) s.channels
      >>= fun ch ->
      Some ch.service_name
    in
    let pid       = Some { typ = Some (Printf.sprintf "%d-PES" x.pid)
                         ; id  = x.pid } in
    let node      = Some (Cpu "Анализатор QoE") in
    let level     = Err in
    let info      = "" in
    let create flag time message info =
      if not flag then []
      else [ { time; input; stream; service; pid; node; level; info; message } ]
    in
    let errors = x.errors in
    List.concat
      [ create errors.black.peak_flag errors.black.timestamp
          "Черный кадр" info
      ; create errors.black.cont_flag errors.black.timestamp
          "Черный кадр (Продолжительная)" info
      ; create errors.luma.peak_flag errors.luma.timestamp
          "Превышено пороговое значение средней яркости" info
      ; create errors.luma.cont_flag errors.luma.timestamp
          "Превышено пороговое значение средней яркости (Продолжительная)" info
      ; create errors.freeze.peak_flag errors.freeze.timestamp
          "Заморозка видео" info
      ; create errors.freeze.cont_flag errors.freeze.timestamp
          "Заморозка видео (Продолжительная)" info
      ; create errors.diff.peak_flag errors.diff.timestamp
          "Средняя разность кадров ниже заданного значения" info
      ; create errors.diff.cont_flag errors.diff.timestamp
          "Средняя разность кадров ниже заданного значения (Продолжительная)" info
      ; create errors.blocky.peak_flag errors.blocky.timestamp
          "Блочность" info
      ; create errors.blocky.cont_flag errors.blocky.timestamp
          "Блочность (Продолжительная)" info
      ]

  let to_log_messages sources structures filter x =
    match filter with
    | `All -> convert sources structures x
    | `Id ids when id_in x.stream ids ->
       convert sources structures x
    | _ -> []

end
             
module Audio = struct
  open Qoe_errors.Audio_data

  (* *)
  let convert sources structures x =
    let (_, stream) = List.find
                        (fun (_,s) -> Common.Stream.(ID.equal s.id x.stream))
                        !sources
    in
    let input     = Common.Stream.get_input stream in
    let stream    = Some x.stream in
    let service   =
      let open Option.Infix in
      let open Structure in
      React.S.value structures
      |> List.find_opt (fun s -> Common.Stream.ID.equal s.id x.stream)
      >>= fun s ->
      List.find_opt (fun channel -> channel.number = x.channel) s.channels
      >>= fun ch ->
      Some ch.service_name
    in
    let pid       = Some { typ = Some (Printf.sprintf "%d-PES" x.pid)
                         ; id  = x.pid } in
    let node      = Some (Cpu "Анализатор QoE") in
    let level     = Err in
    let info      = "" in
    let create flag time message info =
      if not flag then []
      else [ { time; input; stream; service; pid; node; level; info; message } ]
    in
    let errors = x.errors in
    List.concat
      [ create errors.loudness_shortt.peak_flag errors.loudness_shortt.timestamp
          "Превышено значение кратковременной громкости" info
      ; create errors.loudness_moment.cont_flag errors.loudness_moment.timestamp
          "Превышено значение моментальной громкости" info
      ; create errors.silence_shortt.peak_flag errors.silence_shortt.timestamp
          "Кратковременная громкость: тишина" info
      ; create errors.silence_moment.cont_flag errors.silence_moment.timestamp
          "Моментальная громкость: тишина" info
      ]

  let to_log_messages sources structures filter x =
    match filter with
    | `All -> convert sources structures x
    | `Id ids when id_in x.stream ids ->
       convert sources structures x
    | _ -> []

end

module Status = struct
  open Qoe_status

  let convert sources structures x =
    let (_, stream) = List.find
                        (fun (_,s) -> Common.Stream.(ID.equal s.id x.stream))
                        !sources
    in
    let input     = Common.Stream.get_input stream in
    let stream    = Some x.stream in
    let service   =
      let open Option.Infix in
      let open Structure in
      React.S.value structures
      |> List.find_opt (fun s -> Common.Stream.ID.equal s.id x.stream)
      >>= fun s ->
      List.find_opt (fun channel -> channel.number = x.channel) s.channels
      >>= fun ch ->
      Some ch.service_name
    in
    let pid       = Some { typ = Some (Printf.sprintf "%d-PES" x.pid)
                         ; id  = x.pid } in
    let node      = Some (Cpu "Анализатор QoE") in
    let info      = "" in
    let time      = Ptime_clock.now () in
    let level, message =
      if x.playing
      then Info, "Поток обнаружен"
      else Err, "Поток потерян"
    in [ { time; input; stream; service; pid; node; level; info; message } ]

  let to_log_messages sources structures filter x =
    match filter with
    | `All -> convert sources structures x
    | `Id ids when id_in x.stream ids ->
       convert sources structures x
    | _ -> []
end
