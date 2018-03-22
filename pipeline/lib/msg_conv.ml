open Containers

let (%) = Fun.(%)

let rec msg_to_yojson : Msgpck.t -> Yojson.Safe.json = function
  | Nil       -> `Null
  | Bool x    -> `Bool x
  | Int  x    -> `Int x
  | Uint32  x -> `Int (Int32.to_int x)
  | Int32   x -> `Int (Int32.to_int x)
  | Uint64  x -> `Int (Int64.to_int x)
  | Int64   x -> `Int (Int64.to_int x)
  | Float32 x -> `Float (Int32.float_of_bits x)
  | Float   x -> `Float x
  | String  x -> `String x
  | Bytes   x -> `String x
  | Map   lst -> `Assoc (List.map
                           (Pair.map (fun k -> k
                                     |> msg_to_yojson
                                     |> function `String s -> s
                                               | _ -> raise (Failure "msg_to_yojson: key is not a string"))
                                     msg_to_yojson)
                           lst)
  | List  lst -> `List  (List.map msg_to_yojson lst)
  | Ext (i,s) -> `Tuple [(`Int i); (`String s)]

let rec msg_of_yojson : Yojson.Safe.json -> Msgpck.t = fun j ->
  let open Msgpck in
  match j with
  | `Null        -> Nil
  | `Bool x      -> Bool x
  | `Int x       -> Int x
  | `Intlit x    -> String x
  | `Float x     -> Float x
  | `String x    -> String x
  | `Assoc lst   -> Map (List.map
                           (Pair.map (fun k -> String k)
                                     msg_of_yojson)
                           lst)
  | `Tuple lst | `List lst -> List (List.map msg_of_yojson lst)
  | `Variant (k, Some v) ->   List [String k; (msg_of_yojson v)]
  | `Variant (k, None)   ->   List [String k]

let of_string (s : string) = Yojson.Safe.from_string s

let to_string js = Yojson.Safe.to_string js
                            
let of_msg_string msg =
  let (_,m) = Msgpck.String.read msg
  in m

let to_msg_string j = Bytes.to_string @@ Msgpck.String.to_string j

type 'a typ = Json    : Yojson.Safe.json typ
            | Msgpack : Msgpck.t typ
                    
type 'a converter = { of_string : string -> 'a
                    ; to_string : 'a -> string
                    }

let get_converter : type a. a typ -> a converter = function
  | Json    -> { of_string = of_string; to_string = to_string }
  | Msgpack -> { of_string = of_msg_string; to_string = to_msg_string }
