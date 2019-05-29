open Components

let section () =
  let checkbox = Checkbox.make () in
  (* let t = checkbox#connect#change (fun _ _ ->
   *     print_endline "changed!";
   *     Lwt.return_unit) in
   * checkbox#set_on_destroy (fun () -> Lwt.cancel t); *)
  let button =
    Button.make ~appearance:Raised
      ~label:"toggle indeterminate"
      ~on_click:(fun _ _ _ ->
        checkbox#set_indeterminate (not checkbox#indeterminate);
        Lwt.return_unit)
      () in
  Widget.create_div
    ~widgets:[ checkbox#widget
             ; button#widget ]
    ()
