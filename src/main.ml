
let rec main config =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  
  let module DB        = Database.Make(Config) in
  let module Pipe      = Pipeline.Make(Config)(DB) in
  let module Server    = Serv.Make(Config)(DB)(Pipe) in
  
  let db = DB.create config in
  let pipe, pipeloop = Pipe.create config db in
  let server = Server.create config db pipe in

  let _ = Lwt_react.E.map (fun js -> Lwt_io.printf "Event: %s\n" (Yojson.Safe.to_string js)|> ignore) pipe.options_events in
  Lwt_main.run (Lwt.join [server; pipeloop]);

  DB.finalize db;
  Pipe.finalize pipe;
  main config

let () =
  let config = Config.create "./config.json" in
  main config
