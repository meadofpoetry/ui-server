type t = Yojson.Safe.t

let parse = function
  | `List (`Int code :: `Int id :: tl) -> (
    match code, tl with
    | 8, [`String s] -> Some (id, `Error s)
    | 32, [`String uri] -> Some (id, `Subscribe uri)
    | 33, [`Int subid] -> Some (id, `Subscribed subid)
    | 34, [`Int subid] -> Some (id, `Unsubscribe subid)
    | 35, [] -> Some (id, `Unsubscribed)
    | 36, [data] -> Some (id, `Event data)
    | _ -> None)
  | _ -> None

let compose id = function
  | `Error s -> `List [`Int 8; `Int id; `String s]
  | `Subscribe uri -> `List [`Int 32; `Int id; `String uri]
  | `Subscribed subid -> `List [`Int 33; `Int id; `Int subid]
  | `Unsubscribe subid -> `List [`Int 34; `Int id; `Int subid]
  | `Unsubscribed -> `List [`Int 35; `Int id]
  | `Event data -> `List [`Int 36; `Int id; data]
