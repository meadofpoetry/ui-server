open Components
open Application_types
open Containers
open Netlib.Uri

module Api_http = Api_js.Http.Make(Body)

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
    React.S.map ~eq:(Equal.option String.equal)
      (fun x ->
        begin match x with
        | None -> ()
        | Some _ ->
           if not acc_textfield#empty
           then acc_textfield#update ()
        end;
        x)
      new_textfield#s_input in
  let s =
    React.S.l3 ~eq:(Equal.option User.equal_pass_change)
      (fun n o r ->
        match n, o, r with
        | Some new_pass, Some old_pass, Some _ ->
           Some User.{ user; old_pass; new_pass }
        | _ -> None)
      s_new old_textfield#s_input acc_textfield#s_input in
  let settings =
    new Vbox.t
      ~widgets:[ old_textfield#widget
               ; old_helper_text#widget
               ; new_textfield#widget
               ; new_helper_text#widget
               ; acc_textfield#widget
               ; acc_helper_text#widget ]
      () in
  let set_error (s : string) : unit =
    old_textfield#set_valid false;
    old_helper_text#set_content s in
  let set (pass : User.pass_change) =
    let f () =
      Api_http.perform_unit
        ~path:Path.Format.("/api/user/password" @/ empty)
        ~query:Query.empty
        ~body:(User.pass_change_to_yojson pass)
        (fun _env x -> Lwt.return x) in
    Lwt.try_bind
      (fun () -> f ())
      (function
       | Ok _ ->
          if User.equal user `Root
          then Js_of_ocaml.(Dom_html.window##.location##.href := Js.string "/");
          Lwt.return (Ok ())
       | Error e ->
          let s = Api_js.Http.error_to_string e in
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
  let root_card = make_card `Root in
  let operator_card = make_card `Operator in
  let guest_card = make_card `Guest in

  let box =
    new Layout_grid.t
      ~cells:[ new Layout_grid.Cell.t ~widgets:[root_card] ()
             ; new Layout_grid.Cell.t ~widgets:[operator_card] ()
             ; new Layout_grid.Cell.t ~widgets:[guest_card] () ]
      () in
ignore @@ new Ui_templates.Page.t (`Static [box#widget]) ()
