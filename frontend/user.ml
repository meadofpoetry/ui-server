open Components
open Common.User
open Api_js.Requests

let make_card user =
  let username =
    match user with
    | `Root     -> "администратора"
    | `Operator -> "оператора"
    | `Guest    -> "гостя"
  in 
  let verify_pass pass =
    if String.length pass < 4
    then Error "password is too short"
    else Ok ()
  in
  let eq_pass old npass =
    match old with
    | None     -> Error "pass empty"
    | Some old -> if old = npass
                  then Ok ()
                  else Error "pass mismatch"
  in
  (* let title     = new Card.Title.t ~title:("Пароль " ^ username) () in
   * let primary   = new Card.Primary.t ~widgets:[title] () in *)
  (* title#add_class "color--primary-on-primary";
   * primary#add_class "background--primary"; *)
  let old_form  = new Textfield.t
                    ~label:"Пароль пользователя"
                    ~input_type:(Widget.Password (fun pass -> Ok ())) () in
  let new_form  = new Textfield.t
                    ~label:"Новый пароль"
                    ~input_type:(Widget.Password (fun pass -> verify_pass pass)) () in
  let acc_form  = new Textfield.t
                    ~label:"Повторите пароль"
                    ~input_type:(Widget.Password (fun pass -> eq_pass (React.S.value new_form#s_input) pass)) () in
  let settings  = new Box.t
                      ~vertical:true
                      ~widgets:[ old_form#widget
                               ; new_form#widget
                               ; acc_form#widget ]
                      () in
  let media     = new Card.Media.t ~widgets:[settings] () in
  old_form#set_required true; new_form#set_required true; acc_form#set_required true;

  let apply       = new Button.t ~label:"Применить" () in
  let actions     = new Card.Actions.t ~widgets:[ apply ] () in
  let card        = new Card.t ~widgets:[media#widget; actions#widget] () in
  let _ = React.E.map (fun _ ->
              let open Lwt_result.Infix in
              match (React.S.value old_form#s_input, React.S.value acc_form#s_input) with
              | Some old_pass, Some new_pass ->
                 let pass = { user; old_pass; new_pass } in
                 post_js_ok "/api/user/password" (Common.User.pass_change_to_yojson pass)
                 >|= fun () ->
                 if user = `Root
                 then Dom_html.window##.location##.href := Js.string "/";
              | _, _ -> Lwt_result.fail "Incorrect or empty ip address")
            apply#e_click
  in
  card

let () =
  let user      = Js.to_string @@ Js.Unsafe.variable "username" in
  let doc       = Dom_html.document in
  let container = Dom_html.getElementById "arbitrary-content" in
  let text      = Dom_html.createP doc in

  let root_card     = make_card `Root in
  let operator_card = make_card `Operator in
  let guest_card    = make_card `Guest in

  let box = new Layout_grid.t
              ~cells:[ new Layout_grid.Cell.t ~widgets:[root_card] ()
                     ; new Layout_grid.Cell.t ~widgets:[operator_card] ()
                     ; new Layout_grid.Cell.t ~widgets:[guest_card] () ]
              ()
  in
  Dom.appendChild container box#root;
