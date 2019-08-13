open Pipeline_protocol

(*
let connect_db streams_events dbs =
  Lwt_main.run @@ Storage.init dbs;
  E.map_s (fun s -> Storage.request dbs (Storage.Store_structures s)) streams_events
 *)
let typ = "pipeline"

let create kv db =
  let (>>=) = Lwt.bind in
  let (>>=?) = Lwt_result.bind in
(*
  Kv.RW.parse ~default:Pipeline_settings.default Pipeline_settings.of_string kv ["pipeline";"settings"]
  >>=? fun cfg ->
 *)
  Pipeline_protocol.Protocol.create db kv
  >>=? fun state ->

  let reset state ss =
    Pipeline_protocol.Protocol.reset state ss
    >>= function
    | Ok () -> Lwt.return_unit
    | Error (`Qoe_backend e) -> Lwt.fail_with e
  in
  (*React.E.keep @@ connect_db (S.changes api.streams) dbs;*)

  Logs.info (fun m -> m "(Pipeline) created");
  Lwt.return_ok @@ object
    val state = state
    method reset ss = reset state ss
    method http () = Pipeline_http.handlers state
    method ws () = Pipeline_http.ws state
    method pages () = Pipeline_http.pages ()
    method finalize () = Pipeline_protocol.Protocol.finalize state
    method log_source  = (fun filter ->
      let sf =
        Log_converters.Status.to_log_messages state.sources state.options.structures#s filter
      in
      (* TODO
      let vf =
        Log_converters.Video.to_log_messages state.sources state.options.structures#s filter
      in
      let af =
        Log_converters.Audio.to_log_messages state.sources state.options.structures#s filter
      in
       *)
      Util_react.E.aggregate_merge
        ~merge:(fun acc x -> x @ acc)
        (fun () -> Lwt_unix.sleep 1.0)
        [ (*React.E.map vf state.notifs.vdata
        ; React.E.map af state.notifs.adata
        ; *) React.E.map sf state.notifs.status_raw
        ])
  end
