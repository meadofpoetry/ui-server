open Topology
open Containers

type id = Single
        | T2mi_plp of int
        | Dvb of int * int
        | Unknown of int32 [@@deriving show, eq]

let id_of_int32 : int32 -> id = function
  | 0l -> Single
  | x when Int32.equal (Int32.logand x (Int32.of_int 0xFFFF0000)) Int32.zero
    -> let x'     = Int32.to_int x in
       let stream = (x' land 0x0000FF00) lsr 8 in
       let plp    = (x' land 0xFF) in
       (match stream with
        | 1             -> T2mi_plp plp
        | 2 | 3 | 4 | 5 -> Dvb (stream - 2, plp)
        | _             -> Unknown x)
  | _ as x -> Unknown x

let id_to_int32 : id -> int32 = function
  | Single           -> 0l
  | T2mi_plp plp     -> 1
                        |> (fun x -> x lsl 8)
                        |> Int32.of_int
                        |> Int32.logor (Int32.of_int plp)
  | Dvb (stream,plp) -> stream + 2
                        |> (fun x -> x lsl 8)
                        |> Int32.of_int
                        |> Int32.logor (Int32.of_int plp)
  | Unknown x        -> x

let id_to_yojson id : Yojson.Safe.json =
  let i32 = id_to_int32 id in `Intlit (Int32.to_string i32)
let id_of_yojson json : (id,string) result = match json with
  | `Intlit i -> Result.of_opt (Option.map id_of_int32 @@ Int32.of_string i)
  | _         -> Error "not an int32"

type stream_id = [`Ip of Url.t | `Ts of id] [@@deriving yojson, show, eq]

type stream =
  { source      : src
  ; id          : stream_id
  ; description : string option
  }
and src = Port   of int
        | Stream of id [@@deriving yojson, show, eq]

type t =
  { source      : source
  ; id          : stream_id 
  ; description : string option
  }
and source = Input  of Topology.topo_input
           | Parent of t [@@deriving yojson, show]

let to_short_name (t:t) =
  let src = match t.source with
    | Input i  -> Topology.get_input_name i
    | Parent _ -> "Поток"
  in
  let id  = match t.id with
    | `Ip uri -> Some (Url.to_string uri)
    | _       -> None
  in
  let s = Printf.sprintf "Источник: %s" src in
  match id with
  | Some id -> s ^ ", " ^ id
  | None    -> s

let rec equal l r =
  match l.id, r.id with
  | `Ip ul, `Ip ur ->
     if Url.equal ul ur
     then equal_source l.source r.source
     else false
  | `Ts il, `Ts ir ->
     if equal_id il ir
     then equal_source l.source r.source
     else false
  | _ -> false
and equal_source l r = match l, r with
  | Input l, Input r -> Topology.equal_topo_input l r
  | Parent l, Parent r -> equal l r
  | _ -> false

type t_opt  = t option [@@deriving yojson]

type t_list = t list [@@deriving yojson]

let t_to_topo_port (b:topo_board) (t:t) =
  let rec get_input = function
    | Parent x -> get_input x.source
    | Input x  -> x
  in
  let input = get_input t.source in
  let rec get_port = function
    | []    -> None
    | h::tl -> (match h.child with
                | Input x -> if equal_topo_input x input then Some h else get_port tl
                | Board x -> (match get_port x.ports with
                              | Some _ -> Some h
                              | None   -> get_port tl))
  in get_port b.ports

let header : t -> string = fun s ->
  let h = match s.id with
    | `Ip _  -> "IP stream"
    | `Ts id -> Printf.sprintf "TS stream %s" @@ show_id id
  in
  match s.description with
  | None -> h
  | Some d -> h ^ " (" ^ d ^ ")" 

let rec get_input s =
  match s.source with
  | Parent s -> get_input s
  | Input  i -> i
