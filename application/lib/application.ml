open Containers
open Common.Topology

type t = { proc   : Proc.t option
         ; hw     : Hardware.t
         ; topo   : Common.Topology.t React.signal
         }

module Settings_topology = struct
  include Common.Topology
  let default   = `Boards []
  let domain    = "topology"
end

module Conf_topology = Storage.Config.Make(Settings_topology)

let proc_table = Proc.create_dispatcher [ (module Pipeline) ]
                                        
let create config db =
  let topology   = Option.get_exn @@ Conf_topology.get_opt config in
  let options    = Storage.Options.Conf.get config in
  let proc       = match topology with
    | `Boards bs -> None
    | `CPU c     -> Proc.create proc_table c.process config db
  in
  let hw, loop   = Hardware.create config db topology in
  { proc; hw; topo = hw.topo }, loop

let finalize app =
  Hardware.finalize app.hw;
  Option.iter (fun p -> p#finalize ()) app.proc
