open Containers
open Msg_conv
open Interop_json
open Common

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

let dispatch : type a. a typ -> (string, (a -> unit)) Hashtbl.t -> a -> unit = function
  | Json    -> Interop_json.dispatch
  | Msgpack -> failwith "not implemented"

module Ready =
  Make(struct
      type t = unit
      let name = "backend"
      let of_yojson = function
        | `String "Ready" -> Ok ()
        | _               -> Error "notification Ready: bad value"
    end)

let _ready_ev = ref None

let next ev =
  let waiter, wakener = Lwt.task () in
  _ready_ev := Some (React.E.map (fun x -> Lwt.wakeup_later wakener x) (React.E.once ev));
  Lwt.on_cancel waiter (fun () -> match !_ready_ev with None -> () | Some ev -> React.E.stop ev);
  waiter

let is_ready ev = next ev
