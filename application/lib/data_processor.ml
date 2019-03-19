type url = Netlib.Uri.t

module Api_http = Api_cohttp.Make (Application_types.User) (Application_types.Body)
(*module Api_template = Api_cohttp_template.Make (Application_types.User)*)

type t =
  < reset    : (url * Application_types.Stream.t) list -> unit
  ; handlers : unit -> Api_http.t list
  ; template :
      unit -> Api_http.node list
  ; log_source : Application_types.Stream.Log_message.source
  ; finalize : unit -> unit >
                   
module type PROCESS = sig
  val typ    : string
  val create : Kv.RW.t -> Db.t -> t
end

let create_dispatcher l =
  let tbl = Hashtbl.create 10 in
  List.iter (fun (module P : PROCESS) -> Hashtbl.add tbl P.typ (module P : PROCESS)) l;
  tbl

let create tbl typ config db =
  match Hashtbl.find_opt tbl typ with
  | None -> None
  | Some (module P : PROCESS) ->
     Some (P.create config db)
