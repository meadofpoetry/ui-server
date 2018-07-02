open Containers
open Common.Topology

let limit ?eq f s =
  let limiter = ref Lwt.return_unit in
  let delayed = ref None in
  let event, push = React.E.create () in
  let iter =
    React.E.fmap
      (fun x ->
        if Lwt.is_sleeping !limiter then begin
            match !delayed with
            | Some cell ->
               cell := x;
               None
            | None ->
               let cell = ref x in
               delayed := Some cell;
               None
          end else begin
            limiter := f ();
            push x;
            None
          end)
      (React.S.changes s)
  in
  React.S.hold ?eq (React.S.value s) (React.E.select [iter; event])

type t = { proc       : Proc.t option
         ; network    : Pc_control.Network.t
         ; users      : User.entries
         ; hw         : Hardware.t
         ; topo       : Common.Topology.t React.signal
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
  let network    = match Pc_control.Network.create config with
    | Ok net  -> net
    | Error e -> failwith ("bad network config: " ^ e)
  in
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
          List.fold_left (fun acc (_,_,ss) -> (filter ss) @ acc) [] l
          |> proc#reset)
      @@ limit (fun () -> Lwt_unix.sleep 2.) hw.streams
      |> Lwt_react.S.keep) proc;
  { users; proc; network; hw; topo = hw.topo }, loop

let redirect_filter app =
  Api.Redirect.redirect_auth (User.validate app.users)
  
let finalize app =
  Hardware.finalize app.hw;
  Option.iter (fun p -> p#finalize ()) app.proc
