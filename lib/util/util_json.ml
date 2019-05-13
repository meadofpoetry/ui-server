include Yojson.Safe

type 'a res = ('a, string) result

type t = Yojson.Safe.json

let all_ok l =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (Ok x)::tl -> loop (x::acc) tl
    | (Error _ as e)::_ -> e
  in loop [] l

(* TODO remove in 4.08 *)
let (>>=) m f = match m with
  | Ok v -> f v
  | Error _ as e -> e

let equal (x : t) y = x = y

let compare (x : t) y = compare x y

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
        match Int32.of_string_opt x with
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
    | `Intlit x -> (try Ok (Int64.of_string x) with _ -> Error "Int64.of_yojson: bad int")
    | _      -> Error "not an int64"
end

module List = struct
  type 'a t = 'a list
  let to_yojson (f:'a -> json) (l:'a t) : json =
    `List (List.map f l)
  let of_yojson (f:json -> 'a res) = function
    | `List l -> all_ok @@ List.map f l
    | _       -> Error "not a list"
end

module Option = struct
  type 'a t = 'a option
  let to_yojson (f:'a -> json) = function
    | None   -> `Null
    | Some v -> f v
  let of_yojson (f:json -> 'a res) = function
    | `Null -> Ok None
    | json  ->
       match f json with
       | Ok v -> Ok (Some v)
       | Error _ as e -> e
end

module String = struct
  type t = string
  let to_yojson (x:t) : json = `String x
  let of_yojson : json -> t res = function
    | `String x -> Ok x
    | _         -> Error "not a string"
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b
  let to_yojson (f1 : 'a -> json) (f2 : 'b -> json) (t : ('a, 'b) t) : json =
    let j1 = f1 @@ fst t in
    let j2 = f2 @@ snd t in
    `List [j1; j2]
  let of_yojson (f1 : json -> 'a res) (f2 : json -> 'b res) = function
    | `List [x; y] ->
       f1 x >>= fun x' ->
       f2 y >>= fun y' ->
       Ok (x', y')
    | _ -> Error "not a pair"
end

module Result = struct
  type ('a, 'b) t = ('a, 'b) result
  let to_yojson (f1 : 'a -> json) (f2 : 'b -> json) (t : ('a, 'b) t) : json =
    match t with
    | Ok x -> `List [`String "Ok"; f1 x]
    | Error e -> `List [`String "Error"; f2 e]
  let of_yojson (f1 : json -> 'a res)
        (f2 : json -> 'b res) : json -> ('a, 'b) t res = function
    | `List [`String "Ok"; x] ->
       begin match f1 x with
       | Ok v -> Ok (Ok v)
       | Error e -> Error e
       end
    | `List [`String "Error"; x] ->
       begin match f2 x with
       | Ok v -> Ok (Error v)
       | Error e -> Error e
       end
    | _ -> Error "not a result"

end
