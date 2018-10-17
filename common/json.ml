open Containers
include Yojson.Safe

type 'a res = ('a,string) result

type t = Yojson.Safe.json

let equal x y = Equal.physical x y

let compare x y = Ord.compare x y

let pp ppf t =
  Format.fprintf ppf "%s" (Yojson.Safe.pretty_to_string t)

(** Dummy converters for ppx derivers *)
let to_yojson = fun x -> x
let of_yojson = fun x -> Ok x

module Bool = struct
  type t = bool
  let to_yojson (x:t) : json = `Bool x
  let of_yojson : json -> t res = function
    | `Bool x -> Ok x
    | _       -> Error "not a bool"
end

module Float = struct
  type t = float
  let to_yojson (x:t) : json = `Float x
  let of_yojson : json -> t res = function
    | `Float x -> Ok x
    | _        -> Error "not a float"
end

module Int = struct
  type t = int
  let to_yojson (x:t) : json = `Int x
  let of_yojson : json -> t res = function
    | `Int x -> Ok x
    | `Intlit x -> (try Ok (int_of_string x) with _ -> Error "Int.of_yojson: bad int")
    | _      -> Error "not an int"
end

module Int32 = struct
  type t = int32
  let to_yojson (x:t) : json = `Intlit (Int32.to_string x)
  let of_yojson : json -> t res = function
    | `Int x -> Ok (Int32.of_int x)
    | `Intlit x -> begin
        match Int32.of_string x with
        | Some x -> Ok x
        | None -> Error "int32_of_yojson: bad value"
      end
    | _ -> Error "int32_of_yojson: not an int32"
end

module Int64 = struct
  type t = int64
  let to_yojson (x:t) : json = `Intlit (Int64.to_string x)
  let of_yojson : json -> t res = function
    | `Int x -> Ok (Int64.of_int x)
    | `Intlit x -> (try Ok (Int64.of_string_exn x) with _ -> Error "Int64.of_yojson: bad int")
    | _      -> Error "not an int64"
end

module List = struct
  type 'a t = 'a list
  let to_yojson (f:'a -> json) (l:'a t) : json =
    `List (List.map f l)
  let of_yojson (f:json -> 'a res) = function
    | `List l -> List.map f l |> List.all_ok
    | _       -> Error "not a list"
end

module Option = struct
  type 'a t = 'a option
  let to_yojson (f:'a -> json) = function
    | None   -> `Null
    | Some v -> f v
  let of_yojson (f:json -> 'a res) = function
    | `Null -> Ok None
    | json  -> Result.map Option.return @@ f json
end

module String = struct
  type t = string
  let to_yojson (x:t) : json = `String x
  let of_yojson : json -> t res = function
    | `String x -> Ok x
    | _         -> Error "not a string"
end

module Pair = struct
  type ('a,'b) t = 'a * 'b
  let to_yojson (f1:'a -> json) (f2:'b -> json) (t:('a,'b) t) : json =
    let j1 = f1 @@ fst t in
    let j2 = f2 @@ snd t in
    `List [j1;j2]
  let of_yojson (f1:json -> 'a res) (f2:json -> 'b res) = function
    | `List [x;y] -> Result.(f1 x >>= fun v1 -> f2 y >|= fun v2 -> v1,v2)
    | _           -> Error "not a pair"
end
