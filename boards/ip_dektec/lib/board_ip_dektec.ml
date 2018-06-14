open Containers
open Common.Topology
open Api.Interaction
open Boards.Board
open Board_types

module Api_handler = Api.Handler.Make(Common.User)

module Data = struct
  type t = Board_types.config
  let default = Board_types.config_default
  let dump = Board_types.config_to_string
  let restore = Board_types.config_of_string
end

module Config_storage = Storage.Options.Make (Data)

module Board_model : sig
  type _ req =
    | Store_status : Board_types.board_status -> unit req
  include (Storage.Database.MODEL with type 'a req := 'a req)
end = Db

module Database = Storage.Database.Make(Board_model)

type 'a request = 'a Board_protocol.request

let create_sm = Board_protocol.SM.create

let create (b:topo_board) _ convert_streams send db_conf base step =
  let storage               = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, s_state_push = React.S.create `No_response in
  let events, api, step     = create_sm send storage s_state_push step in
  let s_strms,s_strms_push  = React.S.create [] in
  let e_status = React.E.map (fun (x : board_status) ->
                     let open Common.Stream in
                     let (stream : stream) = { source      = Port 0
                                             ; id          = `Ts Single
                                             ; description = Some ""
                                             } in
                     s_strms_push @@ (if x.asi_bitrate > 0 then [stream] else []))
                 @@ React.E.changes events.status in
  let handlers = Board_api.handlers b.control api events s_state in
  let db       = Result.get_exn @@ Database.create db_conf in
  let _s = Lwt_react.E.map_p (fun s -> Database.request db (Board_model.Store_status s)) @@ React.E.changes events.status in
 (* let sms = convert_streams s_strms b in
  let _e = React.E.map (fun s ->
      `List (List.map Common.Stream.to_yojson s)
      |> Yojson.Safe.pretty_to_string
      |> Lwt_io.printf "IP sms: %s\n"
      |> ignore;) @@ React.S.changes sms in *)
  let state = object
      method s = _s;
      method e_status = e_status;
      method db = db;
      method finalize () = ()
    end in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = convert_streams s_strms b
  ; step           = step
  ; connection     = s_state
  ; ports_active   = (List.fold_left (fun acc (p : topo_port) ->
                          (match p.port with
                           | 0 -> React.S.const true
                           | x -> raise (Invalid_port ("Board_ip_dektec: invalid_port " ^ (string_of_int x))))
                          |> fun x -> Ports.add p.port x acc)
                        Ports.empty b.ports)
  ; settings_page  = ("IP", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("IP", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
