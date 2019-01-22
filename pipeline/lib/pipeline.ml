open Lwt.Infix
open Containers
open Pipeline_protocol

let (%) = Fun.(%)

module Conf = Storage.Config.Make(Pipeline_settings)

(*
let connect_db streams_events dbs =
  Lwt_main.run @@ Storage.init dbs;
  E.map_s (fun s -> Storage.request dbs (Storage.Store_structures s)) streams_events
 *)
let typ = "pipeline"
  
let create (config:Storage.Config.config) (db_conf:Storage.Database.t) =
  let cfg = Conf.get config in
  let api, state, recv = Pipeline_protocol.create db_conf config cfg.sock_in cfg.sock_out in
  let reset = Pipeline_protocol.reset cfg.bin_path cfg.bin_name in
  (*React.E.keep @@ connect_db (S.changes api.streams) dbs;*)
  (* polling loop *)
  let rec loop () =
    recv () >>= loop
  in
  Logs.info (fun m -> m "(Pipeline) created");
  object
    val loop  = loop ()
    val api   = api
    val state = state
    method reset ss = reset api state ss
    method handlers () = Pipeline_api.handlers api
    method template () = Pipeline_template.create ()
    method finalize () = Pipeline_protocol.finalize state
    method log_source  = (fun filter ->
      let sf =
        Log_converters.Status.to_log_messages api.sources api.notifs.applied_structs filter in
      let vf =
        Log_converters.Video.to_log_messages api.sources api.notifs.applied_structs filter in
      let af =
        Log_converters.Audio.to_log_messages api.sources api.notifs.applied_structs filter in
      Storage.Database.aggregate_merge ~merge:(fun acc x -> x @ acc) 1.0
        [ React.E.map vf api.notifs.vdata
        ; React.E.map af api.notifs.adata
        ; React.E.map sf api.notifs.status_raw
        ])
  end
