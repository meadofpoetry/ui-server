type t = Single
       | Ip of int32
       | T2mi_plp of int
       | Dvb_plp of int * int [@@deriving yojson]

let of_int32 : int32 -> t = function
  | 0l -> Single
  | x when (Int32.logand x (Int32.of_int 0xFFFF0000)) = Int32.zero
    -> let x'     = Int32.to_int x in
       let stream = (x' land 0x0000FF00) lsr 8 in
       let plp    = (x' land 0xFF) in
       (match stream with
        | 1             -> T2mi_plp plp
        | 2 | 3 | 4 | 5 -> Dvb_plp (stream - 2, plp)
        | _             -> Ip x)
  | _ as x -> Ip x

let to_int32 : t -> int32 = function
  | Single                -> 0l
  | T2mi_plp plp          -> 1
                             |> (fun x -> x lsl 8)
                             |> Int32.of_int
                             |> Int32.logor (Int32.of_int plp)
  | Dvb_plp (stream,plp)  -> stream + 2
                             |> (fun x -> x lsl 8)
                             |> Int32.of_int
                             |> Int32.logor (Int32.of_int plp)
  | Ip x -> x
