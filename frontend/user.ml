open Components
open Common.User
open Api_js.Requests.Json_request
open Containers

let make_card user =
  let username = match user with
    | `Root -> "администратора"
    | `Operator -> "оператора"
    | `Guest -> "гостя" in
  let verify_pass pass =
    if String.length pass < 4
    then Error "Слишком короткий пароль"
    else Ok () in
  let eq_pass old npass =
    match old with
    | None -> Ok ()
    | Some old ->
       if String.equal old npass then Ok ()
       else Error "Пароли не совпадают" in
  let title     =
    new Card.Primary.title ("Пароль " ^ username) () in
  let primary   =
    new Card.Primary.t ~widgets:[title] () in
  let old_helper_text =
    new Textfield.Helper_text.t
      ~validation:true
      ~auto_validation_message:true
      () in
  let old_textfield  =
    new Textfield.t
      ~helper_text:old_helper_text
      ~outlined:true
      ~required:true
      ~label:"Пароль"
      ~input_type:(Password (fun _ -> Ok ())) () in
  let new_helper_text =
    new Textfield.Helper_text.t
      ~validation:true
      ~auto_validation_message:true
      () in
  let new_textfield =
    new Textfield.t
      ~helper_text:new_helper_text
      ~outlined:true
      ~required:true
      ~label:"Новый пароль"
      ~input_type:(Password verify_pass)
      () in
  let acc_helper_text =
    new Textfield.Helper_text.t
      ~validation:true
      ~auto_validation_message:true
      () in
  let acc_textfield =
    new Textfield.t
      ~helper_text:acc_helper_text
      ~outlined:true
      ~required:true
      ~label:"Повторите пароль"
      ~input_type:(Password (fun pass ->
                       eq_pass (React.S.value new_textfield#s_input) pass)) () in
  let s_new =
    React.S.map (fun x ->
        begin match x with
        | None -> ()
        | Some _ ->
           if not acc_textfield#empty
           then acc_textfield#update ()
        end;
        x)
      new_textfield#s_input in
  let s =
    React.S.l3 (fun n o r ->
        match n, o, r with
        | Some new_pass, Some old_pass, Some _ ->
           Some { user; old_pass; new_pass }
        | _ -> None) s_new old_textfield#s_input acc_textfield#s_input in
  let settings =
    new Vbox.t ~widgets:[ old_textfield#widget
                        ; old_helper_text#widget
                        ; new_textfield#widget
                        ; new_helper_text#widget
                        ; acc_textfield#widget
                        ; acc_helper_text#widget ]
      () in
  let set_error (s : string) : unit =
    old_textfield#set_valid false;
    old_helper_text#set_content s in
  let set (pass : pass_change) =
    let open Common in
    let open Lwt.Infix in
    let f () =
      post_result
        ?scheme:None ?host:None ?port:None ?from_err:None
        ~from:(fun _ -> Ok ())
        ~path:Uri.Path.Format.("/api/user/password" @/ empty)
        ~query:Uri.Query.empty
        ~contents:(pass_change_to_yojson pass) in
    Lwt.try_bind
      (fun () -> f ())
      (function
       | Ok _ ->
          if eq user `Root
          then Dom_html.window##.location##.href := Js.string "/";
          Lwt.return (Ok ())
       | Error e ->
          let s = Api_js.Requests.err_to_string e in
          set_error s;
          Lwt.return (Error s))
      (fun e ->
        let s = Printexc.to_string e in
        set_error s;
        Lwt.return (Error s)) in
  let apply = new Ui_templates.Buttons.Set.t s set () in
  let media = new Card.Media.t ~widgets:[settings] () in
  let actions = new Card.Actions.t ~widgets:[apply] () in
  new Card.t
    ~widgets:[ primary#widget
             ; (new Divider.t ())#widget
             ; media#widget
             ; actions#widget]
    ()

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
