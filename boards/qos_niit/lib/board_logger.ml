open Containers
open Common
open Board_qos_types

module Make(Logs : Logs.LOG) = struct

  let log_of_errors event source
      : Stream.Log_message.t list React.event =
    let open Stream.Log_message in
    let filter l =
      List.filter (fun (id, _) ->
          match source with
          | `All -> true
          | `Id ids -> List.mem ~eq:Stream.ID.equal id ids) l in
    let map = fun ((id : Stream.ID.t), (errors : Error.t_ext list)) ->
      List.map (fun (error : Error.t_ext) ->
          let num, name = Ts_error.Kind.of_error error in
          let service = Option.flat_map (fun (x : Pid.info) -> x.service_name)
                          (snd error.pid) in
          let pid =
            { typ = Option.map (fun (x : Pid.info) ->
                        Pid.typ_to_string x.typ)
                      (snd error.pid)
            ; id = fst error.pid
            } in
          make ?service
            ~time:error.time
            ~level:Warn
            ~message:(num ^ " " ^ name)
            ~info:(Ts_error.Info.of_error error)
            ~stream:id
            ~pid
            ()) errors in
    React.E.map Fun.(List.concat % List.map map % filter) event

  let log_of_device (control : int) (events : device_events) =
    let open Stream.Log_message in
    let make = make ~node:(Board control) in
    let state =
      React.E.fmap (function
          | `Fine ->
             make ~time:(Ptime_clock.now ())
               ~level:Info
               ~message:"Восстановление после внутреннего сбоя"
               ~info:""
               ()
             |> List.return
             |> Option.return
          | _ -> None)
      @@ React.S.changes events.state in
    let errors =
      React.E.map (fun (x : Board_error.t list) ->
          List.map (fun (e : Board_error.t) ->
              let prefix = match e.source with
                | Hardware -> "Hardware error"
                | Protocol -> "Protocol error" in
              let info = Printf.sprintf "%s: code = %d, count = %d"
                           prefix e.code e.count in
              let info = match e.param with

                | None -> info
                | Some p -> Printf.sprintf "%s, param = %d" info p in
              make ~time:(Ptime_clock.now ())
                ~level:Fatal
                ~message:"Внутренний сбой"
                ~info
                ()) x) events.errors in
    React.E.merge (@) [] [state; errors]

  let log_of_streams (control : int) event source =
    let s = match source with
      | `All -> event
      | `Id ids ->
         React.S.fmap ~eq:(Equal.list Stream.equal) (fun streams ->
             List.filter (fun (s : Stream.t) ->
                 List.mem ~eq:Stream.ID.equal s.id ids) streams
             |> function [] -> None | l -> Some l)
           [] event in
    React.S.diff (fun cur old ->
        let open Stream.Log_message in
        let eq = Stream.equal in
        let found = List.filter (fun s -> not @@ List.mem ~eq s old) cur in
        let lost = List.filter (fun s -> not @@ List.mem ~eq s cur) old in
        let time = Ptime_clock.now () in (* FIXME should be time from status *)
        let make ~message ~level (s : Stream.t) =
          make ~time
            ~message
            ~level
            ~info:(Stream.Source.to_string s.source.info)
            ~stream:s.id
            ?input:(Stream.get_input s)
            ~node:(Board control)
            () in
        let found' =
          List.map (fun (s : Stream.t) ->
              make ~level:Info ~message:"Обнаружен поток" s) found in
        let lost' =
          List.map (fun (s : Stream.t) ->
              make ~level:Err ~message:"Пропадание потока" s) lost in
        found' @ lost') s

  let make_event (control : int) (events : events) source
      : Stream.Log_message.t list React.event =
    let errors = log_of_errors events.ts.errors source in
    (* let device = log_of_device control events.device in *)
    let streams = log_of_streams control events.streams source in
    React.E.merge (@) [] [errors; (* device; *) streams]

end
