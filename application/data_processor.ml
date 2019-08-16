open Application_types

type url = Netlib.Uri.t

module Api_http = Api_cohttp.Make (User) (Body)
module Api_events = Api_websocket.Make (User) (Body) (Body_ws)
module Api_template = Api_cohttp_template.Make (User)

type tab =
  < stylesheets : string list
  ; pre_scripts : Api_template.script list
  ; post_scripts : Api_template.script list
  ; content : Tyxml.Xml.elt list
  ; title : string
  ; path : Netlib.Uri.Path.t
  >

type tag =
  [ `Input of Topology.topo_input
  | `Stream
  ]

type t =
  < reset : (url * Stream.t) list -> unit Lwt.t
  ; http : Api_http.t list
  ; ws : Api_events.t list
  ; pages : Api_template.topmost Api_template.item list
  ; tabs : (tag * tab list) list
  ; log_source : Stream.Log_message.source
  ; finalize : unit -> unit Lwt.t >

type error = [ Db.conn_error | Kv.RW.parse_error | Kv_v.error ]

module type PROCESS = sig
  val typ    : string
  val create : Topology.topo_cpu -> Kv.RW.t -> Db.t -> (t, [> error]) Lwt_result.t
end

let create_dispatcher l =
  let tbl = Hashtbl.create 10 in
  List.iter (fun (module P : PROCESS) -> Hashtbl.add tbl P.typ (module P : PROCESS)) l;
  tbl

let pp_error _ppf = failwith "todo"

let create tbl (cpu : Topology.topo_cpu) config db =
  let (>>=) = Lwt.bind in
  match Hashtbl.find_opt tbl cpu.process with
  | None -> Lwt.return_none
  | Some (module P : PROCESS) ->
     P.create cpu config db
     >>= function
     | Error e ->
        Logs.err (fun m -> m "Software data processor error %a" pp_error e);
        Lwt.return_none
     | Ok v ->
        Lwt.return_some v
