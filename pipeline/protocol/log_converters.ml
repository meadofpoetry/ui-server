open Application_types
open Application_types.Stream.Log_message
open Pipeline_types
   
(* TODO remove 4.08 *)

let (>>=) m f = match m with
  | None -> None
  | Some x -> f x

let opt_map f m = match m with
  | None -> None
  | Some v -> Some (f v)
   
(* TODO merge Audio and Video *)
   
let id_in id list =
  List.exists (Stream.ID.equal id) list
   
module Video = struct
  open Qoe_errors.Video_data

  (* *)

  let convert sources structures x =
    let (_, stream) = List.find
                        (fun (_,s) -> Stream.(ID.equal s.id x.stream))
                        !sources
    in
    let input     = Stream.get_input stream in
    let stream    = Some x.stream in
    let info      =
      let open Structure in
      React.S.value structures
      |> List.find_opt (fun s -> Stream.ID.equal s.id x.stream)
      >>= fun s ->
      List.find_opt (fun channel -> channel.number = x.channel) s.channels
      >>= fun ch ->
      let service_name = ch.service_name in
      List.find_opt (fun (pid : pid) -> pid.pid = x.pid) ch.pids
      >>= fun pid -> Some (service_name, pid.stream_type)
    in
    let service = opt_map fst info in
    let stream_type = opt_map snd info in
    let typ       = opt_map (fun x ->
                        MPEG_TS.PID.Type.to_string
                        @@ PES { stream_type = x
                               ; stream_id = 0 })
                      stream_type in
    let pid       = Some { typ; id  = x.pid } in
    let node      = Some (Cpu "pipeline") in
    let level     = Err in
    let create flag time message info =
      if not flag then []
      else [{ time; input; stream; service; pid; node; level; info = info (); message }]
    in
    let errors = x.errors in
    let info (params : Qoe_errors.params) typ peak_cont () =
      match peak_cont with
      | `Peak -> Printf.sprintf
                   "Пиковая ошибка, значения: среднее %.3f %s, макс. %.3f %s, мин. %.3f %s"
                   params.avg typ params.max typ params.min typ
      | `Cont -> Printf.sprintf
                   "Продолжительная ошибка, значения: среднее %.3f %s, макс. %.3f %s, мин. %.3f %s"
                   params.avg typ params.max typ params.min typ
    in
    List.concat
      [ create errors.black.peak_flag errors.black.timestamp
          "Черный кадр"
          (info errors.black.params "%" `Peak)
      ; create errors.black.cont_flag errors.black.timestamp
          "Черный кадр"
          (info errors.black.params "%" `Cont)
      ; create errors.luma.peak_flag errors.luma.timestamp
          "Превышено пороговое значение средней яркости"
          (info errors.luma.params "" `Peak)
      ; create errors.luma.cont_flag errors.luma.timestamp
          "Превышено пороговое значение средней яркости"
          (info errors.luma.params "" `Cont)
      ; create errors.freeze.peak_flag errors.freeze.timestamp
          "Заморозка видео"
          (info errors.freeze.params "%" `Peak)
      ; create errors.freeze.cont_flag errors.freeze.timestamp
          "Заморозка видеo"
          (info errors.freeze.params "%" `Cont)
      ; create errors.diff.peak_flag errors.diff.timestamp
          "Средняя разность кадров ниже заданного значения"
          (info errors.diff.params "" `Peak)
      ; create errors.diff.cont_flag errors.diff.timestamp
          "Средняя разность кадров ниже заданного значения"
          (info errors.diff.params "" `Cont)
      ; create errors.blocky.peak_flag errors.blocky.timestamp
          "Блочность"
          (info errors.blocky.params "%" `Peak)
      ; create errors.blocky.cont_flag errors.blocky.timestamp
          "Блочность"
          (info errors.blocky.params "%" `Cont)
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
                        (fun (_,s) -> Stream.(ID.equal s.id x.stream))
                        !sources
    in
    let input     = Stream.get_input stream in
    let stream    = Some x.stream in
    let info      =
      let open Structure in
      React.S.value structures
      |> List.find_opt (fun s -> Stream.ID.equal s.id x.stream)
      >>= fun s ->
      List.find_opt (fun channel -> channel.number = x.channel) s.channels
      >>= fun ch ->
      let service_name = ch.service_name in
      List.find_opt (fun (pid : pid) -> pid.pid = x.pid) ch.pids
      >>= fun pid -> Some (service_name, pid.stream_type)
    in
    let service = opt_map fst info in
    let stream_type = opt_map snd info in
    let typ       = opt_map (fun x ->
                        MPEG_TS.PID.Type.to_string
                        @@ PES { stream_type = x
                               ; stream_id = 0 })
                      stream_type in
    let pid       = Some { typ; id  = x.pid } in
    let node      = Some (Cpu "pipeline") in
    let level     = Err in
    let create flag time message info =
      if not flag then []
      else [{ time; input; stream; service; pid; node; level; info = info (); message }]
    in
    let errors = x.errors in
    let info (params : Qoe_errors.params) cont_peak () =
      match cont_peak with
      | `Peak -> Printf.sprintf
                   "Пиковая ошибка, значения: среднее %.3f LUFS, макс. %.3f LUFS, мин. %.3f LUFS"
                   params.avg params.max params.min 
      | `Cont -> Printf.sprintf
                   "Продолжительная ошибка, значения: среднее %.3f LUFS, макс. %.3f LUFS, мин. %.3f LUFS"
                   params.avg params.max params.min
    in
    List.concat
      [ create errors.loudness_shortt.peak_flag errors.loudness_shortt.timestamp
          "Превышено значение кратковременной громкости"
          (info errors.loudness_shortt.params `Peak)
      ; create errors.loudness_moment.cont_flag errors.loudness_moment.timestamp
          "Превышено значение моментальной громкости"
          (info errors.loudness_moment.params `Peak)
      ; create errors.silence_shortt.peak_flag errors.silence_shortt.timestamp
          "Кратковременная громкость: тишина"
          (info errors.silence_shortt.params `Peak)
      ; create errors.silence_moment.cont_flag errors.silence_moment.timestamp
          "Моментальная громкость: тишина"
          (info errors.silence_moment.params `Peak)
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
                        (fun (_,s) -> Stream.(ID.equal s.id x.stream))
                        !sources
    in
    let input     = Stream.get_input stream in
    let stream    = Some x.stream in
    let service   =
      let open Structure in
      React.S.value structures
      |> List.find_opt (fun s -> Stream.ID.equal s.id x.stream)
      >>= fun s ->
      List.find_opt (fun channel -> channel.number = x.channel) s.channels
      >>= fun ch ->
      Some ch.service_name
    in
    let pid       = Some { typ = Some (Printf.sprintf "%d-PES" x.pid)
                         ; id  = x.pid } in
    let node      = Some (Cpu "pipeline") in
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
