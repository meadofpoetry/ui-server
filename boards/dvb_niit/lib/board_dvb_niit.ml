open Containers
open Common.Topology
open Api.Interaction
open Boards.Board
open Board_types

(* TODO remove *)
let (>) = Pervasives.(>)
   
module Api_handler = Api.Handler.Make (Common.User)
   
open Lwt.Infix

module Data = struct
  type t      = config
  let default = config_default
  let dump    = config_to_string
  let restore = config_of_string
end

module Config_storage = Storage.Options.Make (Data)

module Board_model : sig
  type _ req =
    | Store_measures : measures -> unit req
  include (Storage.Database.MODEL with type 'a req := 'a req)
end = Db

module Database = Storage.Database.Make(Board_model)


let log_fmt control = Printf.sprintf "(Board DVB: %d) %s" control

let create (b:topo_board) _ convert_streams send db_conf base step =
  let storage = Config_storage.create base ["board"; (string_of_int b.control)] in
  let events, api, step = Board_protocol.SM.create (log_fmt b.control) send storage step in
  let handlers = Board_api.handlers b.control api events in (* XXX temporary *)
  let db = Result.get_exn @@ Database.create db_conf in
  let _s = Lwt_react.E.map_p (fun m -> Database.request db (Board_model.Store_measures m))
           @@ React.E.changes events.measures in
  let s_streams = React.S.fold
                    (fun (streams : Common.Stream.stream list)
                         (m : measures) ->
                      let open Common.Stream in
                      let plp = List.find_map (fun (x,c) -> if m.id = x then Some c else None) storage#get
                                |> Option.get_exn
                                |> (fun x -> match x.standard with
                                             | T2 -> x.t2.plp
                                             | _  -> 0)
                      in
                      let (stream : stream) = { source      = Port 0
                                              ; id          = `Ts (Dvb (m.id,plp)) (* TODO fix this *)
                                              ; description = Some ""
                                              } in
                      match m.lock,m.bitrate with
                      | true,Some x when x > 0 -> List.add_nodup ~eq:(Common.Stream.equal_stream) stream streams
                      | _                      -> List.remove ~eq:(Common.Stream.equal_stream) ~x:stream streams)
                    [] events.measures in
  let state = (object
                 method _s = _s;
                 method db = db;
                 method finalize () = ()
               end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = convert_streams s_streams b
  ; step           = step
  ; connection     = events.state
  ; ports_active   = (List.fold_left (fun acc p ->
                          (match p.port with
                           | 0 -> React.S.const true
                           | x -> raise (Invalid_port ("Board_dvb_niit: invalid port " ^ (string_of_int x))))
                          |> fun x -> Ports.add p.port x acc)
                                     Ports.empty b.ports)
  ; settings_page  = ("DVB", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("DVB", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
