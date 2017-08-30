let main config =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  let rec mainloop () =
    print_endline "Started.";
    (* State *)
    let db             = Database.create config in
    let user_api       = User_api.handlers db in
    (* Boards *)
    let hw, hwloop     = Hardware.create config db in
    let hw_api         = Hardware.handlers hw in
    (* QoE pipeline  *)
    let pipe, pipeloop =
      let open CCOpt in
      Hardware.streams hw
      >|= Pipeline.create config db
      |> function
        | None -> None, None
        | Some (p,loop) -> Some p, Some loop
    in 
    let pipe_api       =
      match pipe with
      | None -> Pipeline_api.handlers_not_implemented ()
      | Some pipe -> Pipeline_api.handlers pipe
    in
                       
    let routes = Api_handler.create (pipe_api @ user_api @ hw_api) in
    let auth_filter = Redirect.redirect_auth db in
        
    let server = Serv.create config auth_filter routes in

    let loops = match pipeloop with
      | None          -> [server; hwloop]
      | Some pipeloop -> [server; hwloop; pipeloop]
    in
    try
      Lwt_main.run @@ Lwt.pick loops;
    with
    | Failure s -> begin
       Printf.printf "Failed with msg: %s\nRestarting...\n" s;

       print_endline "done";

       Hardware.finalize hw;
       Database.finalize db;
       CCOpt.iter Pipeline.finalize pipe;

       mainloop ()
      end

    | _ -> print_endline "failed with unknown exception"

  in mainloop ()

let () =
  let config = Config.create "./config.json" in
  main config
