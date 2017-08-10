
let rec main config =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  
  let db = Database.create config in
  let pipe, pipeloop = Pipeline.create config db in
  let server = Serv.create config db pipe in

  let _ = Lwt_react.E.map (fun js -> Lwt_io.printf "Event: %s\n" (Yojson.Safe.to_string js)|> ignore) pipe.options_events in
  Lwt_main.run (Lwt.join [server; pipeloop]);

  Database.finalize db;
  Pipeline.finalize pipe;
  main config

let () =
  let config = Config.create "./config.json" in
  main config
