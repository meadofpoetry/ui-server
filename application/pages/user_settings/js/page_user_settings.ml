open Js_of_ocaml_tyxml

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

include Page_user_settings_tyxml.Page
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let get_username () =
  Js_of_ocaml.Js.Unsafe.global##.username
  |> Js_of_ocaml.Js.to_string
  |> Application_types.User.of_string
  |> function
  | Ok _ as x -> Lwt.return x
  | Error s -> Lwt.return_error (`Msg s)

let () =
  let (scaffold : Components.Scaffold.t) = Js_of_ocaml.Js.Unsafe.global##.scaffold in
  let thread =
    get_username ()
    >>=? fun user ->
    scaffold#loaded
    >>= fun () ->
    let account = Account.make user in
    let content =
      match user with
      | `Root ->
          let password = Password.make ~set_snackbar:scaffold#show_snackbar_await user in
          [password#markup]
      | `Guest | `Operator -> []
    in
    let page =
      Components.Widget.create
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ D.create ~children:(account#markup :: content) ()
    in
    Lwt.return_ok page
  in
  let loader = Components_lab.Loader.make_widget_loader thread in
  scaffold#set_body loader
