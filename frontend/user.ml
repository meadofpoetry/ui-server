open Components
open Common.User
open Api_js.Requests

let () =
  let user      = Js.to_string @@ Js.Unsafe.variable "username" in
  let doc       = Dom_html.document in
  let container = Dom_html.getElementById "arbitrary-content" in
  let text      = Dom_html.createP doc in
  text##.textContent := Js.some @@ Js.string user;

  let verify_pass user password =
    if String.length password < 4
    then Error "Password is too short"
    else Ok {user; password}
  in
  let root_form  = new Textfield.t
                     ~label:"Пароль администратора"
                     ~input_type:(Widget.Custom ((fun pass -> verify_pass `Root pass), (fun s -> s.password))) () in
  let but    = new Components.Button.t ~label:"Применить" () in
  let place  = new Components.Card.t
                 ~sections:[`Primary (new Card.Primary.t ~widgets:[root_form] ());
                            `Actions (new Card.Actions.t ~widgets:[but] ())]
                 () in
  let post = function
    | Some pass -> post_js_ok "/api/user/password" (Common.User.pass_to_yojson pass) |> ignore
    | None      -> ()
  in
  but#button_element##.onclick := Dom.handler (fun _ -> post @@ React.S.value root_form#s_input; Js._false);
  
  Dom.appendChild container text;
  Dom.appendChild container place#root;
