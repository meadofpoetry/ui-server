open Components

let section () =
  let snackbar =
    Snackbar.make
      ~label:"Can't send photo. Retry in 5 seconds."
      ~action:(Label "retry")
      ~dismiss:True
      () in
  let show =
    Button.make
      ~label:"show snackbar"
      ~on_click:(fun _ _ _ -> snackbar#open_ ())
      () in
  Lwt.ignore_result
  @@ Events.listen_lwt snackbar#root Snackbar.Event.closing (fun _ _ ->
         print_endline "closing";
         Lwt.return_unit);
  Lwt.ignore_result
  @@ Events.listen_lwt snackbar#root Snackbar.Event.closed (fun _ _ ->
         print_endline "closed";
         Lwt.return_unit);
  Widget.create_div ~widgets:[snackbar#widget; show#widget] ()
