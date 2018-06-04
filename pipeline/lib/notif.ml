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
  | m -> Lwt.ignore_result @@ Lwt_io.printf "UNKNOWN MSG %s\n" @@ Yojson.Safe.pretty_to_string m; ()

let dispatch : type a. a typ -> (string, (a -> unit)) Hashtbl.t -> a -> unit = function
  | Json    -> dispatch_js
  | Msgpack -> failwith "not implemented"

module Ready = Make(struct
                   type t = unit
                   let name = "backend"
                   let of_yojson = function
                     | `String "Ready" -> Ok ()
                     | _               -> Error "notification Ready: bad value"
                 end)

let _ready_ev = ref None

let next ev =
    let waiter, wakener = Lwt.task () in
    _ready_ev := Some (Lwt_react.E.map (fun x -> Lwt.wakeup_later wakener x) (Lwt_react.E.once ev));
    Lwt.on_cancel waiter (fun () -> match !_ready_ev with None -> () | Some ev -> Lwt_react.E.stop ev);
    waiter

let is_ready ev = next ev
    (*
let is_ready ev =
  let thread, wakeup = Lwt.wait () in
  _ready_ev := Some (Lwt_react.E.map (fun _ -> Lwt.wakeup_later wakeup ()) @@ Lwt_react.E.once ev);
  thread
     *)
