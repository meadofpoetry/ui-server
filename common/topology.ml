type state = [ `Fine
             | `No_response
             | `Init
             ] [@@deriving yojson]

type typ = DVB
         | TS
         | IP2TS
         | TS2IP

type input = RF
           | TSOIP
           | ASI

let typ_of_yojson = function
  | `String "DVB"    -> Ok DVB
  | `String "TS"     -> Ok TS
  | `String "IP2TS"  -> Ok IP2TS
  | `String "TS2IP"  -> Ok TS2IP
  | _ as e        -> Error ("typ_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let typ_to_yojson = function
  | DVB    -> `String "DVB"
  | TS     -> `String "TS"
  | IP2TS  -> `String "IP2TS"
  | TS2IP  -> `String "TS2IP"    
                   
let input_of_yojson = function
  | `String "RF"    -> Ok RF
  | `String "TSOIP" -> Ok TSOIP
  | `String "ASI"   -> Ok ASI
  | _ as e -> Error ("output_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let input_to_yojson = function
  | RF    -> `String "RF"
  | TSOIP -> `String "TSOIP"
  | ASI   -> `String "ASI"

type version = int [@@deriving yojson]

type id = int [@@deriving yojson]

type topology = topo_entry list [@@deriving yojson]

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
       
