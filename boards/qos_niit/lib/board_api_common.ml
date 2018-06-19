open Containers
open Common
open Websocket_cohttp_lwt
open Frame
open Board_types
open Board_protocol
open Api.Interaction.Json

module Api_handler = Api.Handler.Make(Common.User)

let ( % )   = Fun.( % )
let ( >|= ) = Lwt.Infix.(>|=)
let ( >>= ) = Api.Interaction.Json.( >>= )
let ( %> )  = Fun.( %> )

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let respond_bad_query e     = respond_result (Result.fail @@ api_err_to_yojson @@ Bad_query e)
let respond_unknown_query q = respond_result (Result.fail @@ api_err_to_yojson @@ Unknown_query q)
let respond_error_other s   = respond_result (Result.fail @@ api_err_to_yojson @@ Other s)

let socket_table = Hashtbl.create 1000

let sock_handler sock_data (event:'a React.event) (to_yojson:'a -> Yojson.Safe.json) body =
  let id = rand_int () in
  Cohttp_lwt.Body.drain_body body
  >>= fun () ->
  Websocket_cohttp_lwt.upgrade_connection
    (fst sock_data)
    (snd sock_data)
    (fun f -> match f.opcode with
              | Opcode.Close -> Hashtbl.remove socket_table id
              | _ -> ())
  >>= fun (resp, body, frames_out_fn) ->
  let send x =
    let msg = Yojson.Safe.to_string x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.E.map (send % to_yojson) event in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
