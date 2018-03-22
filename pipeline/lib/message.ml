open Containers
open Lwt.Infix
open Msg_conv

type 'a msg = { name : string
              ; data : 'a
              } [@@deriving yojson]

type 'a cont_body = { _method : string [@key "method"]
                    ; body : 'a
                    } [@@deriving yojson]

type cont_met = { _method : string [@key "method"]
                } [@@deriving yojson]
            
let get_js (name : string) (send : Yojson.Safe.json -> Yojson.Safe.json Lwt.t) of_ () =
  let msg = msg_to_yojson cont_met_to_yojson { name; data = { _method = "Get" }} in
  send msg >>= function
  | `Assoc [("Fine",rep)] -> begin
      let rep' = cont_body_of_yojson of_ rep in
      match rep' with
      | Error e -> Lwt_io.printf "Received error: %s" (Yojson.Safe.pretty_to_string rep) >>= fun () ->
                   Lwt.return_error e
      | Ok v    -> Lwt.return_ok v.body
    end
  | `Assoc [("Error",`String e)] -> Lwt.return_error e
  | s -> Lwt.return_error ("bad response: " ^ (Yojson.Safe.pretty_to_string s))

let set_js (name : string) (send : Yojson.Safe.json -> Yojson.Safe.json Lwt.t) to_ v =
  let msg = msg_to_yojson (cont_body_to_yojson to_) { name; data = { _method = "Set"; body = v }} in
  send msg >>= fun rep ->
  match rep with
  | `Assoc [("Fine",_)] -> Lwt.return_ok ()
  | `Assoc [("Error", `String e)] -> Lwt.return_error e
  | s -> Lwt.return_error ("bad response: " ^ (Yojson.Safe.pretty_to_string s))

type 'a channel =
  { get : unit -> ('a, string) result Lwt.t
  ; set : 'a -> (unit, string) result Lwt.t
  }

module type VALUE = sig
  type t
  val name : string
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t,string) result
end
       
module Make(V: VALUE) = struct
  type t = V.t
  let create : type a. a typ -> (a -> a Lwt.t) -> t channel = function 
    | Json -> fun send ->
              { get = (fun () -> get_js V.name send V.of_yojson ())
              ; set = (fun x -> set_js V.name send V.to_yojson x)
              }
    | Msgpack -> failwith "not implemented"
end
