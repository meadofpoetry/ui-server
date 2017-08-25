
let main config =
  Nocrypto_entropy_lwt.initialize () |> ignore;
  let rec mainloop () =
    print_endline "Started.";
    (*
    let usb = Cyusb.create () in
    Lwt_io.printf "got: %s\n" (Cbuffer.fold (fun s c -> s ^ (Printf.sprintf " %02x " (int_of_char c))) "" (Cyusb.recv usb)) |> ignore;
     *)
    let _, loop = Usb_device.create () in
    let db = Database.create config in
    let pipe, pipeloop = Pipeline.create config db in
    let routes = Api_handler.create @@ (Pipeline_api.handlers pipe) @ (User_api.handlers db) in
    let auth_filter = Redirect.redirect_auth db in
    let server = Serv.create config auth_filter routes in

    try 
      Lwt_main.run (Lwt.pick [pipeloop; server; loop ()]);
    with
    | Failure s -> begin
       Printf.printf "Failed with msg: %s\nRestarting...\n" s;

       print_endline "done";

       Database.finalize db;
       Pipeline.finalize pipe;
       
       mainloop ()
      end

    | _ -> print_endline "failed with unknown exception"

  in mainloop ()

let () =
  let config = Config.create "./config.json" in
  main config
