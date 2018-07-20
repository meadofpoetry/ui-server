open Containers
open Lwt_react
open Lwt.Infix
open Containers
open Msg_conv
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
  let api, state, recv, reset =
    match cfg.msg_fmt with
    | `Json    ->
       let api, state, recv, send = Pipeline_protocol.create Json db_conf config cfg.sock_in cfg.sock_out in
       let reset = Pipeline_protocol.reset Json send cfg.bin_path cfg.bin_name cfg.msg_fmt in
       api, state, recv, reset
    | `Msgpack ->
       let api, state, recv, send = Pipeline_protocol.create Msgpack db_conf config cfg.sock_in cfg.sock_out in
       let reset = Pipeline_protocol.reset Msgpack send cfg.bin_path cfg.bin_name cfg.msg_fmt in
       api, state, recv, reset
  in
  (*Lwt_react.E.keep @@ connect_db (S.changes api.streams) dbs;*)
  (* polling loop *)
  let rec loop () =
    recv () >>= loop
  in
  Logs.info (fun m -> m "(Pipeline) created");
  object
    val loop  = loop ()
    val api   = api
    val state = state
    method reset ss    = reset api state ss
    method handlers () = List.return @@ Pipeline_api.handlers api
    method template () = Pipeline_template.create ()
    method finalize () = Pipeline_protocol.finalize state
  end
