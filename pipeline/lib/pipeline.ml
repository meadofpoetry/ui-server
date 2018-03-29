open Containers
open Lwt_react
open Lwt.Infix
open Containers
open Msg_conv
open Pipeline_protocol
   
let (%) = Fun.(%)

module Conf = Storage.Config.Make(Pipeline_settings)
        
module Storage : sig
  type _ req =
    | Store_structures : Structure.t list -> unit Lwt.t req
    
  include (Storage.Database.STORAGE with type 'a req := 'a req)
end = Pipeline_storage

let connect_db streams_events dbs =
  Lwt_main.run @@ Storage.init dbs;
  E.map_s (fun s -> Storage.request dbs (Storage.Store_structures s)) streams_events

let typ = "pipeline"
  
let create config dbs =
  let cfg = Conf.get config in
  let api, state, recv =
    match cfg.msg_fmt with
    | `Json    -> Pipeline_protocol.create Json config cfg.sock_in cfg.sock_out
    | `Msgpack -> Pipeline_protocol.create Msgpack config cfg.sock_in cfg.sock_out
  in
  Lwt_react.E.keep @@ connect_db (S.changes api.streams) dbs;
    (* polling loop *)
  let rec loop () =
    recv () >>= loop
  in
  object
    val loop  = loop ()
    val api   = api
    val state = state
    method reset ss    = Pipeline_protocol.reset cfg.bin_path cfg.bin_name cfg.msg_fmt state ss
    method handlers () = Pipeline_api.handlers api
    method template () = Pipeline_template.create ()
    method finalize () = Pipeline_protocol.finalize state
  end
