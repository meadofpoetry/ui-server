type state = [ `Fine
             | `No_response
             | `Init
             ] [@@deriving yojson, show]

type typ = DVB
         | TS
         | IP2TS
         | TS2IP
         [@@deriving show]

type input = RF
           | TSOIP
           | ASI
[@@deriving show]

let typ_to_string = function
  | DVB   -> "DVB"
  | TS    -> "TS"
  | IP2TS -> "IP2TS"
  | TS2IP -> "TS2IP"

let typ_of_string = function
  | "DVB"   -> Ok DVB
  | "TS"    -> Ok TS
  | "IP2TS" -> Ok IP2TS
  | "TS2IP" -> Ok TS2IP
  | s       -> Error ("typ_of_string: bad typ string: " ^ s)

let typ_to_yojson x = `String (typ_to_string x)

let typ_of_yojson = function
  | `String s -> typ_of_string s
  | _ as e    -> Error ("typ_of_yojson: unknown value: " ^ (Yojson.Safe.to_string e))

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

type boards = (int * typ) list [@@deriving yojson]

type version = int [@@deriving yojson, show]

type id = int [@@deriving yojson, show]

type topology = topo_entry list [@@deriving yojson, show]

and topo_entry = Input  of topo_input
               | Board  of topo_board

and topo_input = { input        : input
                 ; id           : int
                 }

and topo_board = { typ          : typ
                 ; model        : string
                 ; manufacturer : string
                 ; version      : version
                 ; control      : int
                 ; connection   : state
                 ; ports        : topo_port list
                 }

and topo_port = { port      : int
                ; listening : bool
                ; child     : topo_entry
                }
              
let get_api_path = string_of_int

let topo_inputs =
  let rec f acc = (function
                   | Input x -> x :: acc
                   | Board x -> CCList.concat @@ (CCList.map (fun x -> f acc x.child) x.ports)) in
  CCList.fold_left f []

let topo_boards =
  let rec f acc = (function
                   | Board b -> CCList.fold_left (fun a x -> f a x.child) (b :: acc) b.ports
                   | Input _ -> acc) in
  CCList.fold_left f []

let topo_paths =
  let rec add_node acc paths = function
    | Input i -> (i,acc) :: paths
    | Board b -> (let ports = List.map (fun x -> x.child) b.ports in
                  match ports with
                  | [] -> paths
                  | l  -> List.fold_left (fun a x -> add_node (b :: acc) a x) paths l)
  in
  CCList.fold_left (fun a x -> add_node [] a x) []

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
       
