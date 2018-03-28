open Containers

type state = [ `Fine
             | `No_response
             | `Init
             ] [@@deriving yojson, show, eq]

type input = RF
           | TSOIP
           | ASI
[@@deriving show, eq]

type board_type = string [@@deriving yojson, show, eq]

type process_type = string [@@deriving yojson, show, eq]

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
   
type t = ([`active], [`high_level | `low_level]) topo_entry list (*[@@deriving yojson, show, eq]*)

and (_,_) topo_entry =
  | Input  : topo_input -> ([> `passive], [> `low_level]) topo_entry
  | Board  : topo_board -> ([> `active],  [> `low_level]) topo_entry
  | CPU    : topo_cpu   -> ([> `active],  [> `high_level]) topo_entry

and topo_input = { input        : input
                 ; id           : int
                 }
               
and topo_board = { typ          : board_type
                 ; model        : string
                 ; manufacturer : string
                 ; version      : version
                 ; control      : int
                 ; connection   : state
                 ; ports        : topo_port list
                 }

and topo_port = { port      : int
                ; listening : bool
                ; child     : ([`active | `passive], [`low_level]) topo_entry
                }

and topo_cpu  = { processes : process_type list
                ; ifaces    : topo_interface list
                }

and topo_interface = { iface : string
                     ; conn  : ([`active | `passive], [`low_level]) topo_entry
                     }
                   
let rec topology_to_yojson (t : t) : Yojson.Safe.json = `List (List.map topo_entry_to_yojson t)
and topo_entry_to_yojson : type a b. (a, b) topo_entry -> Yojson.Safe.json = function
  | Input i -> `Tuple [`String "Input"; topo_input_to_yojson i]
  | Board b -> `Tuple [`String "Board"; topo_board_to_yojson b]
  | CPU c   -> `Tuple [`String "CPU"; topo_cpu_to_yojson c]
and topo_input_to_yojson i = `Assoc [ "input", input_to_yojson i.input; "id", `Int i.id]
and topo_board_to_yojson b = `Assoc [ "typ", board_type_to_yojson b.typ
                                    ; "model", `String b.model
                                    ; "manufacturer", `String b.manufacturer
                                    ; "version", version_to_yojson b.version
                                    ; "control", `Int b.control
                                    ; "connection", state_to_yojson b.connection
                                    ; "ports", `List (List.map topo_port_to_yojson b.ports)
                               ]
and topo_cpu_to_yojson c = `Assoc [ "processes", `List (List.map process_type_to_yojson c.processes)
                                  ; "ifaces", `List (List.map topo_interface_to_yojson c.ifaces)
                             ]
and topo_port_to_yojson p = `Assoc [ "port", `Int p.port
                                   ; "listening", `Bool p.listening
                                   ; "child", topo_entry_to_yojson p.child
                              ]
and topo_interface_to_yojson i = `Assoc [ "iface", `String i.iface
                                        ; "conn", topo_entry_to_yojson i.conn
                                   ]

let llist_of_yojson f l = try
    Ok (List.map (fun js -> match f js with
                            | Ok v    -> v
                            | Error e -> raise_notrace (Failure e))
          l)
  with Failure e -> Error e
                               
let rec topology_of_yojson : Yojson.Safe.json -> (t, string) result = function
  | `List l -> llist_of_yojson topo_entry_active_of_yojson l
  | _       -> Error "topology_of_yojson: list expected"
and topo_entry_active_of_yojson : Yojson.Safe.json -> ((_,_) topo_entry, string) result = function
  | `Tuple [`String "Board"; b] -> Result.map (fun v -> Board v) (topo_board_of_yojson b)
  | `Tuple [`String "CPU";   c] -> Result.map (fun v -> CPU v) (topo_cpu_of_yojson c)
  | _ -> Error "topo_entry_active_of_yojson: list expected"               
and topo_entry_low_level_of_yojson : Yojson.Safe.json -> ((_,_) topo_entry, string) result = function
  | `Tuple [`String "Input"; i] -> Result.map (fun v -> Input v) (topo_input_of_yojson i)
  | `Tuple [`String "Board"; b] -> Result.map (fun v -> Board v) (topo_board_of_yojson b)
  | _ -> Error "topo_entry_of_yojson"
and topo_input_of_yojson : Yojson.Safe.json -> (topo_input, string) result = function
  | `Assoc [ "input", input; "id", `Int id] -> Result.map (fun input -> {input; id}) (input_of_yojson input)
  | _ -> Error "topo_input_of_yojson"
and topo_board_of_yojson : Yojson.Safe.json -> (topo_board, string) result = function
  | `Assoc [ "typ", typ
           ; "model", `String model
           ; "manufacturer", `String manufacturer
           ; "version", version
           ; "control", `Int control
           ; "connection", connection
           ; "ports", `List p
    ] -> let open Result in
         board_type_of_yojson typ >>= fun typ ->
         version_of_yojson version >>= fun version ->
         state_of_yojson connection >>= fun connection ->
         llist_of_yojson topo_port_of_yojson p >>= fun ports ->
         Ok { typ; model; manufacturer; version; control; connection; ports }
  | _ -> Error "topo_board_of_yojson"
and topo_cpu_of_yojson : Yojson.Safe.json -> (topo_cpu, string) result = function
  | `Assoc [ "processes", `List processes
           ; "ifaces", `List ifaces
    ] -> let open Result in
         llist_of_yojson process_type_of_yojson processes >>= fun processes ->
         llist_of_yojson topo_interface_of_yojson ifaces >>= fun ifaces ->
         Ok { processes; ifaces }
  | _ -> Error "topo_cpu_of_yojson"
and topo_port_of_yojson : Yojson.Safe.json -> (topo_port, string) result = function
  | `Assoc [ "port", `Int port
           ; "listening", `Bool listening
           ; "child", child
    ] -> let open Result in
         topo_entry_low_level_of_yojson child >>= fun child ->
         Ok { port; listening; child }
  | _ -> Error "topo_port_of_yojson"
and topo_interface_of_yojson : Yojson.Safe.json -> (topo_interface, string) result = function
  | `Assoc [ "iface", `String iface;
             "conn", conn
    ] -> let open Result in
         topo_entry_low_level_of_yojson conn >>= fun conn ->
         Ok { iface; conn }
  | _ -> Error "topo_interface_of_yojson"
                   
let get_api_path = string_of_int
(*
let cpu_subbranches : t -> [`No_cpu | `Branches of ([`active | `passive], [`low_level]) topo_entry list] = fun topo ->
  let rec
 *)               
let topo_inputs =
  let rec f acc = (function
                   | Input x -> x :: acc
                   | Board x -> List.concat @@ (List.map (fun x -> f acc x.child) x.ports)) in
  List.fold_left f []

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
       
