let off _user _body _env _state =
  let ( let* ) = Lwt.bind in
  let* () = Power.off () in
  Lwt.return `Unit

let reboot _user _body _env _state =
  let ( let* ) = Lwt.bind in
  let* () = Power.reboot () in
  Lwt.return `Unit
