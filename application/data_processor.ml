type url = Netlib.Uri.t

module Api_http = Api_cohttp.Make (Application_types.User) (Application_types.Body)
module Api_template = Api_cohttp_template.Make (Application_types.User)

type t =
  < reset    : (url * Application_types.Stream.t) list -> unit Lwt.t
  ; http : unit -> Api_http.t list
  ; ws : unit -> Api_http.t list
  ; pages : unit -> Api_template.topmost Api_template.item list
  ; log_source : Application_types.Stream.Log_message.source
  ; finalize : unit -> unit Lwt.t >

type error = [ Db.conn_error | Kv.RW.read_error | Kv_v.error ]
                         
module type PROCESS = sig
  val typ    : string
  val create : Kv.RW.t -> Db.t -> (t, [> error]) Lwt_result.t
end

let create_dispatcher l =
  let tbl = Hashtbl.create 10 in
  List.iter (fun (module P : PROCESS) -> Hashtbl.add tbl P.typ (module P : PROCESS)) l;
  tbl

let pp_error _ppf = failwith "todo"
  
let create tbl typ config db =
  let (>>=) = Lwt.bind in
  match Hashtbl.find_opt tbl typ with
  | None -> Lwt.return_none
  | Some (module P : PROCESS) ->
     P.create config db
     >>= function
     | Error e ->
        Logs.err (fun m -> m "Software data processor error %a" pp_error e);
        Lwt.return_none
     | Ok v ->
        Lwt.return_some v
