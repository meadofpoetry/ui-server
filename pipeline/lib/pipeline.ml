open Lwt_react
open Lwt.Infix
open Containers
open Pipeline_protocol
   
let (%) = CCFun.(%)

type pipe = { api       : Pipeline_protocol.api 
            ; state     : Pipeline_protocol.state
            ; db_events : unit event
            ; _e        : unit event
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
     let converter = Msg_conv.get_converter cfg.msg_fmt in
     let exec_path = (Filename.concat cfg.bin_path cfg.bin_name) in
     let exec_opts = Array.of_list (cfg.bin_name :: "-m" :: (PSettings.format_to_string cfg.msg_fmt) :: cfg.sources) in   
     match Unix.fork () with
     | -1   -> failwith "Ooops, fork failed"
     | 0    -> Unix.execv exec_path exec_opts
     | pid  ->
        let api, state, recv = Pipeline_protocol.create cfg.sock_in cfg.sock_out converter hardware_streams in
        let db_events = connect_db (S.changes api.structure) dbs in
        let _e = E.map (fun e ->
                     Structure.t_list_to_yojson e
                     |> Yojson.Safe.pretty_to_string
                     |> Lwt_io.printlf "Got stream from pipeline:\n %s\n"
                     |> ignore)
                   (S.changes api.structure)
        in
        let obj = { api; state; db_events; _e } in
        (* polling loop *)
        let rec loop () =
          recv () >>= loop
        in
        (* finalizer *)
        let fin () =
          Lwt_unix.waitpid [] pid >>= fun _ ->
          Lwt.fail_with "Child'd died for some reason"
        in
        Some obj, Some (Lwt.pick [loop (); fin ()])

let finalize pipe =
  Pipeline_protocol.finalize pipe.state
