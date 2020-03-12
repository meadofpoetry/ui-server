open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_protocol

let ( % ) f g x = f (g x)

let error_to_log_entry id (error : Error.t) =
  let open Stream.Log_message in
  let message =
    if error.is_t2mi then ""
    else
      match Ts_error.etr290_error_of_code error.err_code with
      | None -> "Unknown MPEG-TS error"
      | Some e ->
          Printf.sprintf "%s %s"
            (MPEG_TS.ETR290_error.number e)
            (MPEG_TS.ETR290_error.name e)
  in
  let pid = { typ = None; id = error.pid } in
  make ~time:error.timestamp ~level:Err ~message
    ~info:(Ts_error.Info.of_error error)
    ~stream:id ~pid ()

let extended_error_to_log_entry id (error : Error.t_ext) =
  let open Stream.Log_message in
  let message =
    if error.is_t2mi then ""
    else
      match Ts_error.etr290_error_of_code error.err_code with
      | None -> "Unknown MPEG-TS error"
      | Some e ->
          Printf.sprintf "%s %s"
            (MPEG_TS.ETR290_error.number e)
            (MPEG_TS.ETR290_error.name e)
  in
  let service, typ =
    match snd error.pid with
    | None -> (None, None)
    | Some pid -> (pid.service_name, Some (MPEG_TS.PID.Type.to_string pid.typ))
  in
  let pid = { typ; id = fst error.pid } in
  make ?service ~time:error.timestamp ~level:Warn ~message
    ~info:(Ts_error.Info.of_error error)
    ~stream:id ~pid ()

let log_of_errors event source : Stream.Log_message.t list React.event =
  let filter l =
    List.filter
      (fun (id, _) ->
        match source with
        | `All -> true
        | `Id ids -> List.exists (Stream.ID.equal id) ids)
      l
  in
  let map ((id : Stream.ID.t), (errors : Error.t list)) =
    List.map (error_to_log_entry id) errors
  in
  React.E.map (List.concat % List.map map % filter) event

let log_of_device (control : int) errors state =
  let open Stream.Log_message in
  let make = make ~node:(Board control) in
  let state =
    React.E.fmap (function
      | `No_response | `Init -> None
      | `Detect ->
          Some
            [
              make ~time:(Ptime_clock.now ()) ~level:Info
                ~message:"Пытаюсь обнаружить плату"
                ~info:"" ();
            ]
      | `Fine ->
          let msg =
            make ~time:(Ptime_clock.now ()) ~level:Info
              ~message:
                "Восстановление после \
                 внутреннего сбоя"
              ~info:"" ()
          in
          Some [ msg ])
    @@ React.S.changes state
  in
  let errors =
    React.E.map
      (fun (x : Deverr.t list) ->
        List.map
          (fun (e : Deverr.t) ->
            let prefix =
              match e.source with
              | Hardware -> "Hardware error"
              | Protocol -> "Protocol error"
            in
            let info =
              Printf.sprintf "%s: code = %d, count = %d" prefix e.code e.count
            in
            let info =
              match e.param with
              | None -> info
              | Some p -> Printf.sprintf "%s, param = %d" info p
            in
            make ~time:(Ptime_clock.now ()) ~level:Fatal
              ~message:"Внутренний сбой" ~info ())
          x)
      errors
  in
  React.E.merge ( @ ) [] [ state; errors ]

let log_of_streams (control : int) event source =
  let s =
    match source with
    | `All -> event
    | `Id ids ->
        React.S.fmap
          ~eq:(Util_equal.List.equal Stream.equal)
          (fun streams ->
            List.filter
              (fun (s : Stream.t) -> List.exists (Stream.ID.equal s.id) ids)
              streams
            |> function
            | [] -> None
            | l -> Some l)
          [] event
  in
  React.S.diff
    (fun cur old ->
      let open Stream.Log_message in
      let mem s = List.exists (Stream.equal s) in
      let found = List.filter (fun s -> not @@ mem s old) cur in
      let lost = List.filter (fun s -> not @@ mem s cur) old in
      let time = Ptime_clock.now () in
      (* FIXME should be time from status *)
      let make ~message ~level (s : Stream.t) =
        make ~time ~message ~level
          ~info:(Stream.Source.to_string s.source.info)
          ~stream:s.id ?input:(Stream.get_input s) ~node:(Board control) ()
      in
      let found' =
        List.map
          (make ~level:Info ~message:"Обнаружен поток")
          found
      in
      let lost' =
        List.map
          (make ~level:Err ~message:"Пропадание потока")
          lost
      in
      found' @ lost')
    s

let create (control : int) (api : Protocol.api) source =
  let errors = log_of_errors api.notifs.errors source in
  let device = log_of_device control api.notifs.deverr api.notifs.state in
  let streams = log_of_streams control api.notifs.streams source in
  React.E.merge ( @ ) [] [ errors; device; streams ]
