open Msgpck
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

let rec msg_of_yojson : Yojson.Safe.json -> Msgpck.t = function
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

module Qoe_types = struct
  module State = struct
    include Common.Qoe_types.State
          
    let to_msgpck state =
      to_yojson state
      |> msg_of_yojson
      |> Msgpck.String.to_string

    let of_msgpck msg =
      let (_,m) = Msgpck.String.read msg in
      m
      |> msg_to_yojson
      |> of_yojson
  end
end
