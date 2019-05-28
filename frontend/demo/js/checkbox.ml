open Components

let section () =
  let checkbox = Checkbox.make () in
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
