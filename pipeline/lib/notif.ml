open Containers
open Common
open React

let table = Hashtbl.create 10
   
let dispatch = function
  | `Assoc ["name", `String name; "data", data] -> begin
      match Hashtbl.find_opt table name with
      | None -> ()
      | Some f -> f data
    end
  | m -> Yojson.Safe.pretty_to_string m
         |> Lwt_io.printf "UNKNOWN MSG %s\n"
         |> Lwt.ignore_result;
         ()
                     
let wrap push of_yojson = fun x ->
  match of_yojson x with
  | Ok x -> push x
  | Error e -> Logs.err (fun m -> m "Notification error: %s" e)

let add_event ~name of_yojson =
  let event, push = E.create () in
  let f = wrap push of_yojson in
  Hashtbl.add table name f;
  event

let add_signal ~name ~eq ~init of_yojson =
  let signal, push = S.create ~eq init in
  let f = wrap push of_yojson in
  Hashtbl.add table name f;
  signal

