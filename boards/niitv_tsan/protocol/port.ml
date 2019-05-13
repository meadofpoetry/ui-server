open Util_react
open Application_types
open Board_niitv_tsan_types

let invalid_port board x =
  let prefix = Boards.Board.log_name board in
  let s = prefix ^ ": invalid port " ^ (string_of_int x) in
  raise (Boards.Board.Invalid_port s)

let sync
    (board : Topology.topo_board)
    (input : input signal)
    (streams : Stream.t list signal) : bool signal Boards.Board.Ports.t =
  List.fold_left (fun acc (p : Topology.topo_port) ->
      match input_of_int p.port with
      | None -> invalid_port board p.port
      | Some i ->
        let f a b = match a, b with
          | x, _ :: _ when equal_input i x -> true
          | _ -> false in
        let s = S.l2 ~eq:(=) f input streams in
        Boards.Board.Ports.add p.port s acc)
    Boards.Board.Ports.empty board.ports

let active
    (board : Topology.topo_board)
    (input : input signal) : bool signal Boards.Board.Ports.t =
  List.fold_left (fun acc (p : Topology.topo_port) ->
      match input_of_int p.port with
      | None -> invalid_port board p.port
      | Some i ->
        let s = S.map ~eq:(=) (equal_input i) input in
        Boards.Board.Ports.add p.port s acc)
    Boards.Board.Ports.empty board.ports
