open Js_of_ocaml
open Components

let ( >>= ) = Lwt_result.bind

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Lwt.return
    @@ Application_types.User.of_string
    @@ Js.to_string
    @@ Js.Unsafe.global##.username
    >>= fun user ->
    let account = Account.make user in
    let content = match user with
      | `Root ->
        let password = Password.make ~set_snackbar:scaffold#show_snackbar user in
        [password#markup]
      | `Guest | `Operator -> [] in
    let page =
      Widget.create
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ Markup.make (account#markup :: content) in
    Lwt.return_ok page in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  scaffold#set_body loader#root
