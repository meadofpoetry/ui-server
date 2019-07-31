open Js_of_ocaml
open Js_of_ocaml_lwt
open Components
open Application_types

let name = "Password config"

module Attr = struct
  let username = "data-username"
end

module Selector = struct
  let username = Printf.sprintf "input[name=\"username\"]"
  let old_password = Printf.sprintf "#%s" Markup.Password.old_password_id
  let new_password = Printf.sprintf "#%s" Markup.Password.new_password_id
  let confirm_password = Printf.sprintf "#%s" Markup.Password.confirm_password_id
  let user_tabs = Printf.sprintf ".%s" Tab_bar.CSS.root
  let form = Printf.sprintf "form"
  let submit = Printf.sprintf ".%s.%s" Card.CSS.action Button.CSS.root
end

module Validation = struct
  open Textfield

  let old_password = Password (fun x ->
      match x with
      | "" -> Error "Введите пароль"
      | _ -> Ok ())

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
  let username : Dom_html.inputElement Js.t =
    match Element.query_selector elt Selector.username with
    | None -> failwith @@ name ^ ": username input not found"
    | Some x -> Js.Unsafe.coerce x in

  let old_password : string Textfield.t =
    match Element.query_selector elt Selector.old_password with
    | None -> failwith @@ name ^ ": old password input not found"
    | Some x -> Textfield.attach ~validation:Validation.old_password x in

  let new_password : string Textfield.t =
    match Element.query_selector elt Selector.new_password with
    | None -> failwith @@ name ^ ": new password input not found"
    | Some x -> Textfield.attach ~validation:Validation.new_password x in

  let confirm_password : string Textfield.t =
    match Element.query_selector elt Selector.confirm_password with
    | None -> failwith @@ name ^ ": new confirm password input not found"
    | Some x -> Textfield.attach ~validation:(Validation.confirm new_password) x in
  object(self)

    val submit_button : Button.t =
      match Element.query_selector elt Selector.submit with
      | None -> failwith @@ name ^ ": submit button not found"
      | Some x -> Button.attach x

    val form : Dom_html.formElement Js.t =
      match Element.query_selector elt Selector.form with
      | None -> failwith @@ name ^ ": form element not found"
      | Some x -> Js.Unsafe.coerce x

    val user_tabs : Tab_bar.t =
      match Element.query_selector elt Selector.user_tabs with
      | None -> failwith @@ name ^ ": tab bar element not found"
      | Some x -> Tab_bar.attach ~on_change:(fun _ bar ->
          match bar#active_tab with
          | None -> Lwt.return_unit
          | Some tab ->
            Js.Opt.iter (tab#root##getAttribute (Js.string Attr.username))
              (fun attr -> username##.value := attr);
            Lwt.return_unit) x

    val mutable listeners_ = []

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      let invalid ?use_capture x =
        Lwt_js_events.make_event ?use_capture (Dom_html.Event.make "invalid") x in
      listeners_ <- Lwt_js_events.(
          [ seq_loop invalid old_password#input_element (self#handle_invalid `O)
          ; seq_loop invalid new_password#input_element (self#handle_invalid `N)
          ; seq_loop invalid confirm_password#input_element (self#handle_invalid `C)
          ; inputs new_password#input_element self#handle_new_password_input
          ]);
      form##.onsubmit := Dom.handler self#handle_submit;
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      old_password#destroy ();
      new_password#destroy ();
      confirm_password#destroy ();
      user_tabs#destroy ();
      submit_button#destroy ();
      List.iter Lwt.cancel listeners_;
      listeners_ <- [];
      super#destroy ()

    method private handle_submit e : bool Js.t =
      Dom.preventDefault e;
      let thread = match User.of_string @@ Js.to_string username##.value,
                         old_password#value,
                         new_password#value with
      | Ok user, Some old_pass, Some new_pass ->
        Application_http_js.set_user_password { User. user; old_pass; new_pass }
      | _ -> Lwt.return_ok () in
      submit_button#set_loading_lwt thread;
      Js._false

    method private handle_invalid typ e _ : unit Lwt.t =
      Dom.preventDefault e;
      let input, update_message = match typ with
        | `O -> old_password, true
        | `N -> new_password, false
        | `C -> confirm_password, true in
      input#set_valid input#valid;
      if update_message
      then (
        let message = input#validation_message in
        input#set_helper_text_content message);
      Lwt.return_unit

    method private handle_new_password_input _ _ : unit Lwt.t =
      confirm_password#update ();
      Lwt.return_unit

  end

let make () : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Markup.Password.make () in
  new t elt
