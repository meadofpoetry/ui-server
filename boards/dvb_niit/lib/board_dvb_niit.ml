open Common.Topology
open Api.Interaction
open Meta_board

module Api_handler = Api.Handler.Make (Common.User)
   
open Lwt.Infix

module Data = struct
  type t = Board_types.config
  let default = Board_types.config_default
  let dump = Board_types.config_to_string
  let restore = Board_types.config_of_string
end

module Config_storage = Storage.Options.Make (Data)

module Storage : sig
  type _ req =
    | Store_measures : Board_types.measure -> unit Lwt.t req
  include (Storage.Database.STORAGE with type 'a req := 'a req)
end = Db
    
type 'a request = 'a Board_protocol.request

let create_sm = Board_protocol.SM.create

let create (b:topo_board) convert_streams send db base step =
  let storage      = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush = React.S.create `No_response in
  let events, api, step = create_sm send storage spush step in
  let handlers = Board_api.handlers b.control api events in (* XXX temporary *)
  Lwt_main.run @@ Storage.init db;
  let _s = Lwt_react.E.map_p (fun m -> Storage.request db (Storage.Store_measures m))
           @@ React.E.changes events.measure in
  let s_streams = React.S.fold
                    (fun (streams : Common.Stream.stream list)
                         ((id,meas) : int * Board_types.rsp_measure) ->
                      let open Common.Stream in
                      let (stream : stream) = { source      = Port 0
                                              ; id          = `Ts (Dvb (id, id mod 3)) (* TODO fix this *)
                                              ; description = Some ""
                                              } in
                      match meas.lock,meas.bitrate with
                      | true,Some x when x > 0l -> CCList.add_nodup stream streams
                      | _                       -> CCList.remove ~x:stream streams)
                    [] events.measure in
  (*  let sms = convert_streams s_streams b in
  let _e = React.E.map (fun s ->
               `List (List.map Common.Stream.to_yojson s)
               |> Yojson.Safe.pretty_to_string
               |> Lwt_io.printf "QOS sms: %s\n"
               |> ignore;) @@ React.S.changes sms in *)
  let state = (object
                 method _s = _s;
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
  ; state          = (state :> < >)
  }
