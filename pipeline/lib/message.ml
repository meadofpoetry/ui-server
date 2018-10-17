open Containers
open Lwt.Infix
open Msg_conv
open Interop

module type VALUE = sig
  type t
  val name : string
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t,string) result
end
       
module Make (V: VALUE) = struct
  type t = V.t

  let mutex = Lwt_mutex.create ()
         
  let create : type a. a typ -> (a -> (a, exn) result Lwt.t) -> t channel = function 
    | Json -> fun send ->
              let open Interop_json in
              { get = (fun () ->
                  Logs.debug (fun m -> m "(Pipeline) %s <get> called" V.name);
                  get V.name send V.of_yojson ()
                  >>= function
                  | Ok v ->
                     Logs.debug (fun m -> m "(Pipeline) %s <get> succeded" V.name); Lwt.return_ok v
                  | Error e ->
                     Logs.err (fun m -> m "(Pipeline) %s <get> failed with %s" V.name e); Lwt.return_error e)
              ; set = (fun x ->
                  Logs.debug (fun m -> m "(Pipeline) %s <set> called" V.name);
                  set V.name send V.to_yojson x
                  >>= function
                  | Ok () ->
                     Logs.debug (fun m -> m "(Pipeline) %s <set> succeded" V.name); Lwt.return_ok ()
                  | Error e ->
                     Logs.err (fun m -> m "(Pipeline) %s <set> failed with %s" V.name e); Lwt.return_error e)
              }
    | Msgpack -> failwith "not implemented"
end

module Make_request (V : sig
             type t
             val name : string
             val of_yojson : Yojson.Safe.json -> (t,string) result
           end) = struct
  type t = V.t

  let mutex = Lwt_mutex.create ()

  let create : type a. a typ
                    -> (a -> (a, exn) result Lwt.t)
                    -> t request = function 
    | Json -> fun send ->
              let open Interop_json in
              (fun () ->
                Logs.debug (fun m -> m "(Pipeline) %s <get> called" V.name);
                get_request V.name send V.of_yojson ()
                >>= function
                | Ok v ->
                   Logs.debug (fun m -> m "(Pipeline) %s <get> succeded" V.name); Lwt.return_ok v
                | Error e ->
                   Logs.err (fun m -> m "(Pipeline) %s <get> failed with %s" V.name e); Lwt.return_error e)
    | Msgpack -> failwith "not implemented"
end
