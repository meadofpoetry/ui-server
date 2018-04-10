open Containers
open Common.Topology
open Api.Interaction
open Meta_board

(* TODO remove *)
let (>) = Pervasives.(>)
   
module Api_handler = Api.Handler.Make (Common.User)
   
open Lwt.Infix

module Data = struct
  type t = Board_types.config
  let default = Board_types.config_default
  let dump = Board_types.config_to_string
  let restore = Board_types.config_of_string
end

module Config_storage = Storage.Options.Make (Data)

module Board_model : sig
  type _ req =
    | Store_measures : Board_types.measure_response -> unit req
  include (Storage.Database.MODEL with type 'a req := 'a req)
end = Db

module Database = Storage.Database.Make(Board_model)
    
type 'a request = 'a Board_protocol.request

let create_sm = Board_protocol.SM.create

let create (b:topo_board) _ convert_streams send db_conf base step =
  let storage      = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush = React.S.create `No_response in
  let events, api, step = create_sm send storage spush step in
  let handlers = Board_api.handlers b.control api events s_state in (* XXX temporary *)
  let db = Result.get_exn @@ Database.create db_conf in
  let _s = Lwt_react.E.map_p (fun m -> Database.request db (Board_model.Store_measures m))
           @@ React.E.changes events.measure in
  let s_streams = React.S.fold
                    (fun (streams : Common.Stream.stream list)
                         ((id,m) : Board_types.measure_response) ->
                      let open Common.Stream in
                      let plp = List.find_map (fun (x,c) -> if id = x then Some c else None) storage#get
                                |> Option.get_exn
                                |> (fun x -> match x. mode with
                                             | T2 -> x.t2.plp
                                             | _  -> 0)
                      in
                      let (stream : stream) = { source      = Port 0
                                              ; id          = `Ts (Dvb (id,plp)) (* TODO fix this *)
                                              ; description = Some ""
                                              } in
                      match m.lock,m.bitrate with
                      | true,Some x when Int32.(x > 0l) -> List.add_nodup ~eq:(Common.Stream.equal_stream) stream streams
                      | _                       -> List.remove ~eq:(Common.Stream.equal_stream) ~x:stream streams)
                    [] events.measure in
  (*  let sms = convert_streams s_streams b in
  let _e = React.E.map (fun s ->
               `List (List.map Common.Stream.to_yojson s)
               |> Yojson.Safe.pretty_to_string
               |> Lwt_io.printf "QOS sms: %s\n"
               |> ignore;) @@ React.S.changes sms in *)
  let state = (object
                 method _s = _s;
                 method db = db;
                 method finalize () = Database.finalize db
               end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = convert_streams s_streams b
  ; step           = step
  ; connection     = s_state
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
