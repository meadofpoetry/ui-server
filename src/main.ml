(*
 *)
open Database
open Server_inst

let rec main () =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  let ss = { path = Filename.concat Filename.current_dir_name "resources"
           ; port = 7777
           } in
  let ds = { path = "./db" } in
  let db = Database.create ds in
  let pipe, pipeloop = Pipeline.create () in
  let server = Server_inst.create ~settings:ss ~database:db in
  let _ = Lwt_react.E.map (fun js -> Lwt_io.printf "Event: %s\n" (Yojson.Safe.to_string js)|> ignore) pipe.options_events in
  Lwt_main.run (Lwt.join [server; pipeloop ()]);
  main ()

let () = main ()
