open Containers
open Common.Topology

type t = { proc   : Proc.t option
         ; users  : User.entries
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
  let topology   = match Conf_topology.get_opt config with
    | None   -> failwith "bad topology config"
    | Some t -> t
  in
  let users      = User.create config in
  let options    = Storage.Options.Conf.get config in
  let proc       = match topology with
    | `Boards bs -> None
    | `CPU c     -> Proc.create proc_table c.process config db
  in
  let hw, loop   = Hardware.create config db topology in
  Option.iter (fun proc ->
      let filter = List.filter_map (function  (None,_) -> None
                                            | (Some uri, src) -> Some (uri, src))
      in
      Lwt_react.S.map (fun l ->
          List.fold_left (fun acc (_,ss) -> filter ss) [] l
          |> proc#reset) hw.streams
      |> Lwt_react.S.keep) proc;
  { users; proc; hw; topo = hw.topo }, loop

let redirect_filter app =
  Api.Redirect.redirect_auth (User.validate app.users)
  
let finalize app =
  Hardware.finalize app.hw;
  Option.iter (fun p -> p#finalize ()) app.proc
