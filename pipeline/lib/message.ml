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
      | Error e -> Lwt_io.printf "Received error: %s in\n %s" e (Yojson.Safe.pretty_to_string rep) >>= fun () ->
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

  let mutex = Lwt_mutex.create ()
         
  let create : type a. a typ -> (a -> a Lwt.t) -> t channel = function 
    | Json -> fun send ->
              { get = (fun () ->
                  Lwt_mutex.with_lock mutex
                    (fun () ->
                      Lwt_unix.with_timeout 1.
                        (fun () ->
                          Logs.debug (fun m -> m "(Pipeline) %s <get> called" V.name);
                          get_js V.name send V.of_yojson ()
                          >>= function
                          | Ok v ->
                             Logs.debug (fun m -> m "(Pipeline) %s <get> succeded" V.name); Lwt.return_ok v
                          | Error e ->
                             Logs.err (fun m -> m "(Pipeline) %s <get> failed with %s" V.name e); Lwt.return_error e)))
              ; set = (fun x ->
                  Lwt_mutex.with_lock mutex
                    (fun () ->
                      Lwt_unix.with_timeout 1.
                        (fun () ->
                          Logs.debug (fun m -> m "(Pipeline) %s <set> called" V.name);
                          set_js V.name send V.to_yojson x
                          >>= function
                          | Ok () ->
                             Logs.debug (fun m -> m "(Pipeline) %s <set> succeded" V.name); Lwt.return_ok ()
                          | Error e ->
                             Logs.err (fun m -> m "(Pipeline) %s <set> failed with %s" V.name e); Lwt.return_error e)))
              }
    | Msgpack -> failwith "not implemented"
end
