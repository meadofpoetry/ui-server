(*
 *)
open Database
open Server_inst

let rec main () =
  let ss = { path = Filename.concat Filename.current_dir_name "resources"
           ; port = 7777
           } in
  let ds = { path = "./db" } in
  let db = Database.create ds in
  let server = Server_inst.create ss in
  Lwt_main.run server;
  main ()

let () = main ()
