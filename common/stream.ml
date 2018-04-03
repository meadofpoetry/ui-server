open Topology

type id = Single
        | T2mi_plp of int
        | Dvb of int * int
        | Unknown of int32 [@@deriving yojson, show, eq]

let id_of_int32 : int32 -> id = function
  | 0l -> Single
  | x when (Int32.logand x (Int32.of_int 0xFFFF0000)) = Int32.zero
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

type stream =
  { source      : src
  ; id          : [`Ip of Uri.t | `Ts of id]
  ; description : string option
  }
and src = Port   of int
        | Stream of id
        [@@deriving yojson, show, eq]

type t =
  { source      : source
  ; id          : [`Ip of Uri.t | `Ts of id]
  ; description : string option
  }
and source = Input  of Topology.topo_input
           | Parent of t
           [@@deriving yojson, show, eq]

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
                | Input x -> if x = input then Some h else get_port tl
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
