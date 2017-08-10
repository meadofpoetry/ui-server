
let main config =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  let rec mainloop () =
    
    let db = Database.create config in
    let pipe, pipeloop = Pipeline.create config db in
    let routes = Api_handler.create (Pipeline_api.handlers pipe) in
    let auth_filter = Redirect.redirect_auth db in
    let server = Serv.create config auth_filter routes in

    Lwt_main.run (Lwt.join [server; pipeloop]);

    Database.finalize db;
    Pipeline.finalize pipe;
    mainloop ()
  in mainloop ()

let () =
  let config = Config.create "./config.json" in
  main config
