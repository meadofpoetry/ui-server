open Containers
open Api.Interaction

type url = string

module Api_handler = Api.Handler.Make(Common.User)

type t = < reset    : (url * Common.Stream.source) list -> unit
         ; handlers : unit -> (module Api_handler.HANDLER) list
         ; template : unit -> Api.Template.upper Api.Template.ordered_item list Common.User.user_table
         ; finalize : unit -> unit >
                   
module type PROCESS = sig
  val typ    : string
  val create : Storage.Config.config ->
               Storage.Database.t -> t
end

module Table = Hashtbl.Make(String)

let create_dispatcher l =
  let tbl = Table.create 10 in
  List.iter (fun (module P : PROCESS) -> Table.add tbl P.typ (module P : PROCESS)) l;
  tbl

let create tbl typ config db =
  let open Option in
  Table.find_opt tbl typ >|= fun (module P : PROCESS) ->
  P.create config db
