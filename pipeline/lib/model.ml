open Containers
open Qoe_errors
open Common

open Lwt.Infix

type struct_api = { get_input : Common.Topology.topo_input -> (Structure.t list * Time.t) option Lwt.t
                  ; get_input_between : Common.Topology.topo_input -> Time.t -> Time.t -> (Structure.t list * Time.t) list Lwt.t
                  }

type qoe_api = ()
                    
type t = { struct_api : struct_api
         ; qoe_api    : qoe_api
         }
                    
let create db_conf s_struct e_video e_audio =
  let db = Result.get_exn @@ Db.Conn.create db_conf in
  Lwt_react.S.keep @@
    Lwt_react.S.map (fun x -> Lwt.catch (fun () -> Db.Structure.insert_structures db x)
                                (function Failure e -> Lwt_io.printf "str error: %s\n" e)) s_struct;
  Lwt_react.E.keep @@
    Lwt_react.E.map_p (fun x -> Lwt.catch (fun () -> Db.Errors.insert_video db x)
                                  (function Failure e -> Lwt_io.printf "vdata error: %s\n" e)) e_video;
  Lwt_react.E.keep @@
    Lwt_react.E.map_p (fun x -> Lwt.catch (fun () -> Db.Errors.insert_audio db x)
                                  (function Failure e -> Lwt_io.printf "adata error: %s\n" e)) e_audio;
  let struct_api = { get_input = (fun i -> Db.Structure.select_input db i)
                   ; get_input_between = (fun i f t -> Db.Structure.select_input_between db i f t)
                   }
  in
  let qoe_api = () in
  { struct_api; qoe_api }
