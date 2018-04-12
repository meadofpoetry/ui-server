open Containers
open Lwt_react
open Lwt.Infix
open Containers
open Msg_conv
open Pipeline_protocol
   
let (%) = Fun.(%)

module Conf = Storage.Config.Make(Pipeline_settings)
        
module Pipeline_model : sig
  type _ req =
    | Store_structures : Structure.t list -> unit req
    
  include (Storage.Database.MODEL with type 'a req := 'a req)
end = Pipeline_storage

module Database = Storage.Database.Make(Pipeline_model)
(*
let connect_db streams_events dbs =
  Lwt_main.run @@ Storage.init dbs;
  E.map_s (fun s -> Storage.request dbs (Storage.Store_structures s)) streams_events
 *)
let typ = "pipeline"
  
let create config db_conf =
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
  object
    val loop  = loop ()
    val api   = api
    val state = state
    method reset ss    = reset state ss
    method handlers () = Pipeline_api.handlers api
    method template () = Pipeline_template.create ()
    method finalize () = Pipeline_protocol.finalize state
  end
