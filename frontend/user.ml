open Components
open Common.User
open Api_js.Requests.Json_request

let make_card user =
  let username = match user with
    | `Root -> "администратора"
    | `Operator -> "оператора"
    | `Guest -> "гостя" in
  let verify_pass pass =
    if String.length pass < 4
    then Error "password is too short"
    else Ok () in
  let eq_pass old npass =
    match old with
    | None -> Error "pass empty"
    | Some old ->
       if old = npass then Ok ()
       else Error "pass mismatch" in
  let title     =
    new Card.Primary.title ("Пароль " ^ username) () in
  let primary   =
    new Card.Primary.t ~widgets:[title] () in
  let old_form  =
    new Textfield.t
      ~outlined:true
      ~required:true
      ~label:"Пароль"
      ~input_type:(Password (fun pass -> Ok ())) () in
  let new_form  =
    new Textfield.t
      ~outlined:true
      ~required:true
      ~label:"Новый пароль"
      ~input_type:(Password (fun pass -> verify_pass pass)) () in
  let acc_form  =
    new Textfield.t
      ~outlined:true
      ~required:true
      ~label:"Повторите пароль"
      ~input_type:(Password (fun pass ->
                       eq_pass (React.S.value new_form#s_input) pass)) () in
  let settings =
    new Vbox.t ~widgets:[ old_form#widget
                        ; new_form#widget
                        ; acc_form#widget ]
      () in
  let apply = new Button.t ~label:"Применить" () in
  let media = new Card.Media.t ~widgets:[settings] () in
  let actions = new Card.Actions.t ~widgets:[ apply ] () in
  let card =
    new Card.t ~widgets:[ primary#widget
                        ; (new Divider.t ())#widget
                        ; media#widget
                        ; actions#widget] () in
  apply#listen_click_lwt (fun _ _ ->
      let open Lwt.Infix in
      begin match React.S.value old_form#s_input,
                  React.S.value acc_form#s_input with
      | Some old_pass, Some new_pass ->
         let open Common in
         let pass = { user; old_pass; new_pass } in
         post_result
           ?scheme:None ?host:None ?port:None ?from_err:None
           ~from:(fun _ -> Ok ())
           ~path:Uri.Path.Format.("/api/user/password" @/ empty)
           ~query:Uri.Query.empty
           ~contents:(User.pass_change_to_yojson pass)
         >|= (function
              | Ok _ -> if user = `Root
                        then Dom_html.window##.location##.href := Js.string "/";
                        Ok ()
              | Error e -> Error (Api_js.Requests.err_to_string e))
      | _, _ -> Lwt_result.fail "Incorrect or empty ip address"
      end
      >|= ignore)
  |> Lwt.ignore_result;
  card

let () =
  let user      = Js.to_string @@ Js.Unsafe.global##.username in

  let root_card     = make_card `Root in
  let operator_card = make_card `Operator in
  let guest_card    = make_card `Guest in

  let box = new Layout_grid.t
                ~cells:[ new Layout_grid.Cell.t ~widgets:[root_card] ()
                       ; new Layout_grid.Cell.t ~widgets:[operator_card] ()
                       ; new Layout_grid.Cell.t ~widgets:[guest_card] () ]
                ()
  in
  let _ = new Ui_templates.Page.t (`Static [box#widget]) () in
  ()
