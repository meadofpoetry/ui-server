open Topology

type id = Single
        | T2mi_plp of int
        | Dvb of int * int
        | Unknown of int32 [@@deriving yojson]

type stream =
  { source      : src
  ; id          : id
  ; description : string option
  } 
and src = Port   of int
        | Stream of id
        [@@deriving yojson]
                      
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

type t =
  { source      : source
  ; id          : id
  ; description : string option
  }
and source = Board  of int
           | Input  of Topology.input
           | Parent of t
