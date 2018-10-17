module User' = User
open Containers
open Common

module Settings_topology = struct
  include Topology
  let default = `Boards []
  let domain = "topology"
end
module Conf_topology = Storage.Config.Make(Settings_topology)

type t =
  { proc : Data_processor.t option
  ; network : Pc_control.Network.t
  ; users : User'.entries
  ; hw : Hardware.t
  ; topo : Common.Topology.t React.signal
  }

let proc_table = Data_processor.create_dispatcher [(module Pipeline)]

let create config db =
  let topology   = match Conf_topology.get_opt config with
    | None   -> failwith "bad topology config"
    | Some t -> t
  in
  let users = User'.create config in
  let options = Storage.Options.Conf.get config in
  let network = match Pc_control.Network.create config with
    | Ok net -> net
    | Error e -> failwith ("bad network config: " ^ e) in
  let proc = match topology with
    | `Boards bs -> None
    | `CPU c -> Data_processor.create proc_table c.process config db in
  let hw, loop = Hardware.create config db topology in
  Option.iter (fun (proc : Data_processor.t) ->
      let filter =
        List.filter_map (function
            | (None, _) -> None
            | (Some uri, src) -> Some (uri, src)) in
      React.S.map ~eq:(fun _ _ -> false) (fun (l : Application_types.stream_table) ->
          List.fold_left (fun acc (_, _, ss) -> (filter ss) @ acc) [] l
          |> proc#reset)
      @@ React.S.limit ~eq:Application_types.equal_stream_table
           (fun () -> Lwt_unix.sleep 2.) hw.streams
      |> React.S.keep) proc;
  { users; proc; network; hw; topo = hw.topo }, loop

let redirect_filter app =
  Api.Redirect.redirect_auth (User'.validate app.users)
  
let finalize app =
  Hardware.finalize app.hw;
  Option.iter (fun p -> p#finalize ()) app.proc
