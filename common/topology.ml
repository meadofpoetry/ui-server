open Containers

type state = [ `Fine
             | `No_response
             | `Init
             ] [@@deriving yojson, show, eq]

let state_to_string = function
  | `Fine        -> "fine"
  | `No_response -> "no-response"
  | `Init        -> "init"

let state_of_string = function
  | "fine"        -> Some `Fine
  | "no-response" -> Some `No_response
  | "init"        -> Some `Init
  | _             -> None

type input = RF
           | TSOIP
           | ASI
[@@deriving show, eq]

type board_type = string [@@deriving yojson, show, eq]

type process_type = string [@@deriving yojson, show, eq]

let input_compare l r = match l, r with
  | RF, RF | TSOIP, TSOIP | ASI, ASI -> 0
  | RF, _  | _, ASI -> -1
  | ASI, _ | _, RF  -> 1
                         
let input_to_string = function
  | RF    -> "RF"
  | TSOIP -> "TSOIP"
  | ASI   -> "ASI"

let input_of_string = function
  | "RF"    -> Ok RF
  | "TSOIP" -> Ok TSOIP
  | "ASI"   -> Ok ASI
  | s       -> Error ("input_of_string: bad input string: " ^ s)

let input_to_yojson x = `String (input_to_string x)

let input_of_yojson = function
  | `String s -> input_of_string s
  | _ as e    -> Error ("input_of_yojson: unknown value: " ^ (Yojson.Safe.to_string e))

type boards = (int * board_type) list [@@deriving yojson, eq]

type version = int [@@deriving yojson, show, eq]

type id = int [@@deriving yojson, show, eq]

module Env = Map.Make(String)
type env = string Env.t
let env_to_yojson e : Yojson.Safe.json =
  `Assoc (Env.fold (fun k v a -> (k, `String v)::a) e [])
let env_of_yojson : Yojson.Safe.json -> (env, string) result = function
  | `Assoc ls -> begin
      try ls
          |> List.map (function (k, `String v) -> (k, v)
                              | _ -> raise_notrace (Failure "env_of_yojson :value should be string"))
          |> Env.add_list Env.empty
          |> Result.return
      with Failure e -> Error e
    end
  | _ -> Error "env_of_yojson"
let pp_env = Env.pp String.pp String.pp
let equal_env = Env.equal String.equal

type t = [`CPU of topo_cpu | `Boards of topo_board list] [@@deriving yojson, show, eq]

and topo_entry =
  | Input  : topo_input -> topo_entry
  | Board  : topo_board -> topo_entry

and topo_input = { input        : input
                 ; id           : int
                 }

and topo_board = { typ          : board_type
                 ; model        : string
                 ; manufacturer : string
                 ; version      : version
                 ; control      : int
                 ; connection   : state
                 ; env          : env
                 ; ports        : topo_port list
                 }

and topo_port = { port       : int
                ; listening  : bool
                ; switchable : bool
                ; child      : topo_entry
                }

and topo_cpu  = { process : process_type
                ; ifaces  : topo_interface list
                }

and topo_interface = { iface : string
                     ; conn  : topo_entry
                     }

type cpu_opt = process_type option [@@deriving yojson,eq]

let cpu_subbranches = function
  | `Boards _ -> `No_cpu
  | `CPU    c -> `Branches (List.map (fun i -> i.conn) c.ifaces)

let get_entries = function
  | `Boards l -> List.fold_left (fun acc b -> (List.map (fun p -> p.child) b.ports) @ acc) [] l
  | `CPU    c -> List.map (fun i -> i.conn) c.ifaces

let get_api_path = string_of_int

let get_input_name (i:topo_input) =
  let to_string s = Printf.sprintf "%s %d" s i.id in
  match i.input with
  | RF    -> to_string "RF"
  | TSOIP -> to_string "TSoIP"
  | ASI   -> to_string "ASI"

let inputs t =
  let rec get acc = function
    | Input x -> x :: acc
    | Board x -> List.fold_left (fun acc x -> get acc x.child) acc x.ports
  in
  let topo_inputs_cpu   c = List.fold_left (fun acc i -> get acc i.conn) [] c.ifaces in
  let topo_inputs_board b = List.fold_left (fun acc p -> get acc p.child) [] b.ports in
  match t with
  | `CPU c     -> topo_inputs_cpu c
  | `Boards bs -> List.fold_left (fun acc b -> (topo_inputs_board b) @ acc) [] bs

let boards t =
  let rec get acc = function
    | Input _ -> acc
    | Board x -> List.fold_left (fun acc x -> get acc x.child) (x :: acc) x.ports
  in
  let topo_boards_cpu   c = List.fold_left (fun acc i -> get acc i.conn) [] c.ifaces in
  let topo_boards_board b = List.fold_left (fun acc p -> get acc p.child) [b] b.ports in
  match t with
  | `CPU c     -> topo_boards_cpu c
  | `Boards bs -> List.fold_left (fun acc b -> (topo_boards_board b) @ acc) [] bs

let paths t =
  let topo_paths acc =
    let rec add_node acc paths = function
      | Input i -> (i,acc) :: paths
      | Board b -> (let ports = List.map (fun x -> x.child) b.ports in
                    match ports with
                    | [] -> paths
                    | l  -> List.fold_left (fun a x -> add_node (b :: acc) a x) paths l)
    in
    List.fold_left (fun a x -> add_node acc a x) []
  in
  let add_cpu cpu (i,bl) = (i,bl,cpu) in
  match t with
  | `Boards bs ->
     List.map (fun b -> List.map (add_cpu None) @@ topo_paths [b] @@ List.map (fun p -> p.child) b.ports) bs
     |> List.concat
  | `CPU c     ->
     List.map (add_cpu (Some c)) @@ topo_paths [] @@ List.map (fun p -> p.conn) c.ifaces
     
(*

let topo_boards =
  let rec f acc = (function
                   | Board b -> List.fold_left (fun a x -> f a x.child) (b :: acc) b.ports
                   | Input _ -> acc) in
  List.fold_left f []

let topo_paths =
  let rec add_node acc paths = function
    | Input i -> (i,acc) :: paths
    | Board b -> (let ports = List.map (fun x -> x.child) b.ports in
                  match ports with
                  | [] -> paths
                  | l  -> List.fold_left (fun a x -> add_node (b :: acc) a x) paths l)
  in
  List.fold_left (fun a x -> add_node [] a x) []

let rec sub topo id =
  let rec sub_port ports id =
    match ports with
    | []    -> None
    | x::tl ->
       let b = x.child in
       match got_it b id with
       | Some b -> Some b
       | None   -> sub_port tl id
  and got_it board id =
     match board with
     | Input _ -> None
     | Board b -> 
        if b.control = id
        then Some (Board b)
        else (sub_port b.ports id)
  in
  match topo with
  | []    -> None
  | x::tl ->
     match got_it x id with
     | Some b -> Some [b]
     | None   -> sub tl id
       
 *)
