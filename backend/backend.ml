open Containers
   
module Api_handler = Api.Handler.Make(Common.User)
                   
let main config =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  let rec mainloop () =
    print_endline "Started.";
    (* State *)
    let db             = Storage.Database.create config 60000.0 in (* 60 seconds is for debug purpose only, need a bigger interval *)
    let app, app_loop  = Application.create config db in
    let routes         = Api_handler.create @@ Application_api.handlers app in
    let templates      = Application_template.create app in
    let auth_filter    = Application.redirect_filter app in
    
    let server = Serv.create config auth_filter routes templates in

    let loops = [server; app_loop] in
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
        
        Application.finalize app;
        Storage.Database.finalize db;
        (* mainloop () *)
      end

    | e -> print_endline (Printf.sprintf "failed with exn: %s" (Printexc.to_string e))

  in mainloop ()

let () =
  Lwt_engine.set ~transfer:true ~destroy:true (new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.epoll ());
  let config = Storage.Config.create "./config.json" in
  main config
