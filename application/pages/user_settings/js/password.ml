open Js_of_ocaml
open Components

let name = "Password config"

module Selector = struct
  let old_password = Printf.sprintf "#%s" Markup.Password.old_password_id
  let new_password = Printf.sprintf "#%s" Markup.Password.new_password_id
  let confirm_password = Printf.sprintf "#%s" Markup.Password.confirm_password_id
  let user_tabs = Printf.sprintf ".%s" Tab_bar.CSS.root
end

module Validation = struct
  open Textfield

  let old_password = Password (fun _ -> Ok ())

  let new_password = Password (fun pass ->
      if String.length pass < 4
      then Error "Слишком короткий пароль"
      else Ok ())

  let confirm (new_password : string Textfield.t) = Password (fun pass ->
      match new_password#value with
      | None -> Ok ()
      | Some new_ ->
        if String.equal new_ pass then Ok ()
        else Error "Пароли не совпадают")
end

class t (elt : Dom_html.element Js.t) =
  let old_password : string Textfield.t =
    match Element.query_selector elt Selector.old_password with
    | None -> failwith @@ name ^ ": not old password input found"
    | Some x -> Textfield.attach ~validation:Validation.new_password x in

  let new_password : string Textfield.t =
    match Element.query_selector elt Selector.new_password with
    | None -> failwith @@ name ^ ": not new password input found"
    | Some x -> Textfield.attach ~validation:Validation.new_password x in

  let confirm_password : string Textfield.t =
    match Element.query_selector elt Selector.confirm_password with
    | None -> failwith @@ name ^ ": not new confirm password input found"
    | Some x -> Textfield.attach ~validation:Validation.new_password x in
  object
    val user_tabs : Tab_bar.t =
      match Element.query_selector elt Selector.user_tabs with
      | None -> failwith @@ name ^ ": no tab bar element found"
      | Some x -> Tab_bar.attach ~on_change:(fun _ bar ->
          match bar#active_tab with
          | None -> Lwt.return_unit
          | Some _tab -> Lwt.return_unit) x

    inherit Widget.t elt () as super

    method! destroy () : unit =
      old_password#destroy ();
      new_password#destroy ();
      confirm_password#destroy ();
      super#destroy ()

  end

let make () : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Markup.Password.make () in
  new t elt
