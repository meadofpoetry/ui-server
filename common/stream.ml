open Topology

type id = Single
        | T2mi_plp of int
        | Dvb of int * int
        | Unknown of int32 [@@deriving yojson, show]

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

type ip = Ipaddr.V4.t
let pp_ip = Ipaddr.V4.pp_hum
let ip_to_yojson ip =
  Ipaddr.V4.to_string ip
  |> fun s -> `String s
let ip_of_yojson = function
  | `String s -> Ipaddr.V4.of_string s
                 |> (function Some ip -> Ok ip | None -> Error ("ip_of_yojson: bad ip: " ^ s))
  | _ -> Error "ip_of_yojson: bad js"
type addr = { ip   : ip
            ; port : int
            } [@@deriving yojson, show]

type stream =
  { source      : src
  ; id          : [`Ip of addr | `Ts of id]
  ; description : string option
  }
and src = Port   of int
        | Stream of id
        [@@deriving yojson, show]

type t =
  { source      : source
  ; id          : [`Ip of addr | `Ts of id]
  ; description : string option
  }
and source = Input  of Topology.topo_input
           | Parent of t
           [@@deriving yojson, show]
