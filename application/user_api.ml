type t = User.passwd

let set_password (users : t) user body _env _state =
  let open User in
  let open Lwt.Infix in
  match pass_change_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok pass ->
    Lwt.catch (* TODO remove catch *)
      (fun () ->
         check_pass users pass.user pass.old_pass
         >>= function
         | true ->
           set_pass users { user = pass.user; password = pass.new_pass }
           >>= fun () ->
           print_endline @@ Printf.sprintf "req: %s, body: %s"
             (to_string user) (to_string pass.user);
           if equal user pass.user
           then Lwt.return `Need_auth
           else Lwt.return `Unit
         | false -> Lwt.return (`Error "Wrong password"))
      (fun _ -> Lwt.return (`Error "internal password db error, please report"))

let logout _user _body _env _state =
  Lwt.return `Need_auth

