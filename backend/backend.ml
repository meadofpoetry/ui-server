module Api_handler = Api.Handler.Make(Common.User)
                   
let main config =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  let rec mainloop () =
    print_endline "Started.";
    (* State *)
    let db, dbloop     = Storage.Database.create config 10.0 in
    let users          = User.create config in
    let user_api       = User_api.handlers users in
    (* Boards *)
    let hw, hwloop       = Hardware.create config db in
    let hw_api           = Hardware_api.handlers hw in
    let hw_pages         = Hardware_template.create hw in
    (* QoE pipeline  *)
    let pipe, pipeloop   = Pipeline.create config db hw.input_sources in
    let pipe_api         =
      match pipe with
      | None -> Pipeline_api.handlers_not_implemented ()
      | Some pipe -> Pipeline_api.handlers pipe
    in
    let pipe_pages       =
      match pipe with
      | None -> []
      | Some pipe -> Pipeline_template.create ()
    in
                       
    let routes = Api_handler.create (pipe_api @ user_api @ hw_api) in
    let pages  = List.concat [Responses.home_template ();
                              User_template.create ();
                              hw_pages;
                              pipe_pages] in
    let auth_filter = Api.Redirect.redirect_auth (User.validate users) in
    
    let server = Serv.create config auth_filter routes pages in

    let loops = match pipeloop with
      | None          -> [dbloop; server; hwloop]
      | Some pipeloop -> [dbloop; server; hwloop; pipeloop]
    in
    try
      Lwt_main.run @@ Lwt.pick loops;
    with
    | Failure s -> begin
        Printf.printf "Failed with msg: %s\nRestarting...\n" s;
        
        print_endline "done";
        List.iteri (fun i t -> match Lwt.state t with
                               | Lwt.Sleep  -> Printf.printf "Thread %d is sleeping\n" i
                               | Lwt.Fail e -> Printf.printf "Thread %d is failed with exn %s\n" i (Printexc.to_string e)
                               | Lwt.Return _ -> Printf.printf "Thread %d is done\n" i) loops;
        
        Hardware.finalize hw;
        Storage.Database.finalize db;
        CCOpt.iter Pipeline.finalize pipe;

        (* mainloop () *)
      end

    | e -> print_endline (Printf.sprintf "failed with exn: %s" (Printexc.to_string e))

  in mainloop ()

let () =
  let config = Storage.Config.create "./config.json" in
  main config
