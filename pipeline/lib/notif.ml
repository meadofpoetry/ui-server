open Containers
   
module type NOTIF = sig
  type t
  val name : string
  val of_yojson : Yojson.Safe.json -> (t,string) result
end

module Make(N: NOTIF) = struct
  type t = N.t
  let create = function
    | `Json -> fun pus -> N.name, (fun v -> pus @@ N.of_yojson v)
end

let dispatch_js
      (des : (string, (Yojson.Safe.json -> unit)) Hashtbl.t) = function
  | `Assoc [("name", `String name); ("data", data)] -> begin
      match Hashtbl.find_opt des name with
      | None   -> ()
      | Some f -> f data
    end
  | _ -> ()
