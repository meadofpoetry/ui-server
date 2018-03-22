open Containers
open Lwt_react
open Lwt.Infix
open Containers
open Msg_conv
open Pipeline_protocol
   
let (%) = Fun.(%)

type pipe = { api       : Pipeline_protocol.api 
            ; state     : Pipeline_protocol.state
            }
          
module PSettings = struct
  type format = [ `Json | `Msgpack ]
  let format_of_yojson = function `String "json" -> Ok `Json | `String "msgpack" -> Ok `Msgpack | _ -> Error "Wrong fmt"
  let format_to_yojson = function `Json -> `String "json" | `Msgpack -> `String "msgpack"
  let format_to_string = function `Json -> "json" | `Msgpack -> "msgpack"
  type t = { bin_name  : string
           ; bin_path  : string
           ; sources   : string list
           ; msg_fmt   : format
           ; sock_in   : string
           ; sock_out  : string
           } [@@deriving yojson]
  let default = { bin_name  = "ats3-backend"
                ; bin_path  = "/home/freyr/Documents/soft/dev/ats-analyzer/build/"
                ; sources   = ["udp://224.1.2.2:1234"]
                ; msg_fmt   = `Json
                ; sock_in   = "ipc:///tmp/ats_qoe_in"
                ; sock_out  = "ipc:///tmp/ats_qoe_out"
                }
  let domain = "pipeline"
end

module Conf = Storage.Config.Make(PSettings)

module Storage : sig
  type _ req =
    | Store_structures : Structure.t list -> unit Lwt.t req
    
  include (Storage.Database.STORAGE with type 'a req := 'a req)
end = Pipeline_storage
    
type t = pipe

let connect_db streams_events dbs =
  Lwt_main.run @@ Storage.init dbs;
  E.map_s (fun s -> Storage.request dbs (Storage.Store_structures s)) streams_events
  
let create config dbs (hardware_streams : Common.Stream.source list React.signal) =
  match Conf.get_opt config with
  | None     -> None, None
  | Some cfg ->
     let exec_path = (Filename.concat cfg.bin_path cfg.bin_name) in
     let exec_opts = Array.of_list (cfg.bin_name
                                    :: "-m"
                                    :: (PSettings.format_to_string cfg.msg_fmt)
                                    :: cfg.sources)
     in
     let api, state, recv =
       match cfg.msg_fmt with
       | `Json    -> Pipeline_protocol.create Json config cfg.sock_in cfg.sock_out hardware_streams
       | `Msgpack -> Pipeline_protocol.create Msgpack config cfg.sock_in cfg.sock_out hardware_streams
     in
     Lwt_react.E.keep @@ connect_db (S.changes api.streams) dbs;
     let obj = { api; state } in
     (* polling loop *)
     let rec loop () =
       recv () >>= loop
     in
     (* finalizer *)
     let fin () =
       Lwt_process.exec (exec_path, exec_opts) >>= fun _ ->
       Lwt.fail_with "Child'd died for some reason"
     in
     Some obj, Some (Lwt.pick [loop (); fin ()])

let finalize pipe =
  Pipeline_protocol.finalize pipe.state

let get_streams pipe =
  let open Structure in
  let get_stream = function
    | Unknown  -> None
    | Stream s ->
       match s.id with
       | `Ts _ -> Some s
       | `Ip _ ->
          match s.source with
          | Input _  -> Some s
          | Parent p -> Some p 
  in
  React.S.map (List.filter_map (fun x -> get_stream x.source)) pipe.api.streams

let get_channels pipe =
  let open Structure in
  let get_channels str =
    let id = str.id in
    List.map (fun c -> (id, c)) str.channels
  in
  React.S.map (List.fold_left (fun acc x -> (get_channels x.structure) @ acc) []) pipe.api.streams
