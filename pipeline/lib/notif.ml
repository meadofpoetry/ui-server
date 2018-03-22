open Containers
open Msg_conv
   
module type NOTIF = sig
  type t
  val name : string
  val of_yojson : Yojson.Safe.json -> (t,string) result
end

module Make(N: NOTIF) = struct
  type t = N.t
  let create : type a. a typ -> ((t, string) result -> unit) -> (string * (a -> unit)) = function
    | Json    -> fun pus -> N.name, (fun v -> pus @@ N.of_yojson v)
    | Msgpack -> failwith "not implemented"
end

let dispatch_js
      (des : (string, (Yojson.Safe.json -> unit)) Hashtbl.t) = function
  | `Assoc [("name", `String name); ("data", data)] -> begin
      match Hashtbl.find_opt des name with
      | None   -> ()
      | Some f -> f data
    end
  | _ -> ()

let dispatch : type a. a typ -> (string, (a -> unit)) Hashtbl.t -> a -> unit = function
  | Json    -> dispatch_js
  | Msgpack -> failwith "not implemented"
