(*
 *)
open Server

let rec main () =
  let server = Server.create () in
  Lwt_main.run server;
  main ()

let () = main ()
