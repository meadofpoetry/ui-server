open Containers
open Boards
open Common
open Board_qos_types
open React

module Make(Logs : Logs.LOG) : sig

  val sync : Topology.topo_board -> events -> bool signal Board.Ports.t
  val active : Topology.topo_board -> events -> bool signal Board.Ports.t

end = struct

  let invalid_port board x =
    let prefix = Board.log_name board in
    let s = prefix ^ ": invalid port " ^ (string_of_int x) in
    raise (Board.Invalid_port s)

  let sync (board : Topology.topo_board) (events : events)
      : bool signal Board.Ports.t =
    let eq = Equal.bool in
    List.fold_left (fun acc (p : Topology.topo_port) ->
        begin match input_of_int p.port with
        | None -> invalid_port board p.port
        | Some i ->
           S.l2 ~eq (Fun.curry
                     @@ function
                       | x, _ :: _ when equal_input i x -> true
                       | _ -> false)
             events.device.input events.streams
        end
        |> fun x -> Board.Ports.add p.port x acc)
      Board.Ports.empty board.ports

  let active (board : Topology.topo_board) (events : events)
      : bool signal Board.Ports.t =
    let eq = Equal.bool in
    List.fold_left (fun acc (p : Topology.topo_port) ->
        begin match input_of_int p.port with
        | None -> invalid_port board p.port
        | Some i -> S.map ~eq (equal_input i) events.device.input
        end
        |> fun x -> Board.Ports.add p.port x acc)
      Board.Ports.empty board.ports

end
