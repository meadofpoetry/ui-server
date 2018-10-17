open Containers
open Lwt.Infix

include Interop
            
let get (name : string) (send : Yojson.Safe.json -> (Yojson.Safe.json, exn) result Lwt.t) of_ () =
  let msg = msg_to_yojson cont_met_to_yojson { name; data = { _method = "Get" }} in
  send msg >>= function
  | Ok `Assoc [("Fine",rep)] -> begin
      let rep' = cont_body_of_yojson of_ rep in
      match rep' with
      | Error e -> Lwt_io.printf "Received error: %s in\n %s" e (Yojson.Safe.pretty_to_string rep) >>= fun () ->
                   Lwt.return_error e
      | Ok v    -> Lwt.return_ok v.body
    end
  | Ok `Assoc [("Error",`String e)] -> Lwt.return_error e
  | Ok s -> Lwt.return_error ("bad response: " ^ (Yojson.Safe.pretty_to_string s))
  | Error Lwt_unix.Timeout -> Lwt.return_error "Timeout"
  | Error _ -> Lwt.return_error "Unknown error"

let set (name : string) (send : Yojson.Safe.json -> (Yojson.Safe.json, exn) result Lwt.t) to_ v =
  let msg = msg_to_yojson (cont_body_to_yojson to_) { name; data = { _method = "Set"; body = v }} in
  send msg >>= fun rep ->
  match rep with
  | Ok `Assoc [("Fine",_)] -> Lwt.return_ok ()
  | Ok `Assoc [("Error", `String e)] -> Lwt.return_error e
  | Ok s -> Lwt.return_error ("bad response: " ^ (Yojson.Safe.pretty_to_string s))
  | Error Lwt_unix.Timeout -> Lwt.return_error "Timeout"
  | Error _ -> Lwt.return_error "Unknown error"

let get_request (name : string) (send : Yojson.Safe.json -> (Yojson.Safe.json, exn) result Lwt.t) of_ () =
  let msg = msg_to_yojson (fun () -> `Null) { name; data = () } in
  send msg >>= function
  | Ok `Assoc [("Fine",rep)] -> begin
      let rep' = of_ rep in
      match rep' with
      | Error e -> Lwt_io.printf "Received error: %s in\n %s" e (Yojson.Safe.pretty_to_string rep) >>= fun () ->
                   Lwt.return_error e
      | Ok v    -> Lwt.return_ok v
    end
  | Ok `Assoc [("Error",`String e)] -> Lwt.return_error e
  | Ok s -> Lwt.return_error ("bad response: " ^ (Yojson.Safe.pretty_to_string s))
  | Error Lwt_unix.Timeout -> Lwt.return_error "Timeout"
  | Error _ -> Lwt.return_error "Unknown error"

let dispatch (des : (string, (Yojson.Safe.json -> unit)) Hashtbl.t) = function
  | `Assoc [("name", `String name); ("data", data)] -> begin
      match Hashtbl.find_opt des name with
      | None   -> ()
      | Some f -> f data
    end
  | m -> Lwt.ignore_result @@ Lwt_io.printf "UNKNOWN MSG %s\n" @@ Yojson.Safe.pretty_to_string m; ()
