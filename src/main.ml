
let main config =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  let rec mainloop () =
    print_endline "Started.";
    let db = Database.create config in
    let hw, hwloop = Hardware.create config db in
    let pipe, pipeloop = Pipeline.create config db in
    let routes = Api_handler.create @@ ((Pipeline_api.handlers pipe)
                                        @ (User_api.handlers db)
                                        @ (Hardware.handlers hw)) in
    let auth_filter = Redirect.redirect_auth db in
    let server = Serv.create config auth_filter routes in

    try
      Lwt_main.run (Lwt.pick [pipeloop; hwloop; server]);
    with
    | Failure s -> begin
       Printf.printf "Failed with msg: %s\nRestarting...\n" s;

       print_endline "done";

       Hardware.finalize hw;
       Database.finalize db;
       Pipeline.finalize pipe;
       Usb_device.finalize ();
       
       mainloop ()
      end

    | _ -> print_endline "failed with unknown exception"

  in mainloop ()

let () =
  let config = Config.create "./config.json" in
  main config
