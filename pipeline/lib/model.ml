open Containers
open Qoe_errors
open Common

open Lwt.Infix

module Structure_model : sig
  type _ req =
    | Store : Structure.t list -> unit req
    | Get_input : Common.Topology.topo_input -> (Structure.t list * Time.t) req
    | Get_input_between : (Common.Topology.topo_input * Time.t * Time.t) -> (Structure.t list * Time.t) list req
  include (Storage.Database.MODEL with type 'a req := 'a req)
end = Structure_storage

module Qoe_model : sig
  type _ req =
  | Store_video : Video_data.t -> unit req
  | Store_audio : Audio_data.t -> unit req
  include (Storage.Database.MODEL with type 'a req := 'a req)
end = Qoe_storage

module Database_str = Storage.Database.Make(Structure_model)
module Database_qoe = Storage.Database.Make(Qoe_model)

type struct_api = { get_input : Common.Topology.topo_input -> (Structure.t list * Time.t) Lwt.t
                  ; get_input_between : Common.Topology.topo_input -> Time.t -> Time.t -> (Structure.t list * Time.t) list Lwt.t
                  }

type qoe_api = ()
                    
type t = { struct_api : struct_api
         ; qoe_api    : qoe_api
         }
                    
let create db_conf s_struct e_video e_audio =
  let db_str = Result.get_exn @@ Database_str.create db_conf in
  let db_qoe = Result.get_exn @@ Database_qoe.create db_conf in
  Lwt_react.S.keep @@
    Lwt_react.S.map (fun x -> Lwt.catch (fun () -> Database_str.(request db_str (Store x)))
                                  (function Failure e -> Lwt_io.printf "str error: %s\n" e)) s_struct;
  Lwt_react.E.keep @@
    Lwt_react.E.map_p (fun x -> Lwt.catch (fun () -> Database_qoe.(request db_qoe (Store_video x)))
                                  (function Failure e -> Lwt_io.printf "vdata error: %s\n" e)) e_video;
  Lwt_react.E.keep @@
    Lwt_react.E.map_p (fun x -> Lwt.catch (fun () -> Database_qoe.(request db_qoe (Store_audio x)))
                                  (function Failure e -> Lwt_io.printf "adata error: %s\n" e)) e_audio;
  let struct_api = { get_input = (fun i -> Database_str.(request db_str (Get_input i)))
                   ; get_input_between = (fun i f t -> Database_str.(request db_str (Get_input_between (i,f,t))))
                   }
  in
  let qoe_api = () in
  { struct_api; qoe_api }
