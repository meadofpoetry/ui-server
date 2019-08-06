open Js_of_ocaml
open Js_of_ocaml_lwt
open Components
open Application_types

let name = "password-change-form"

let ( >>= ) = Lwt.bind

module Attr = struct
  let username = "data-username"
  let hidden = "aria-hidden"
end

module Selector = struct
  let username = "input[name=\"username\"]"
  let old_password = "input[name=\"current_password\"]"
  let new_password = "input[name=\"new_password\"]"
  let confirm_password = "input[name=\"confirm_password\"]"
  let submit_input = "input[type=\"submit\"]"
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

class t ~set_snackbar (user : User.t)
    (submit_button : Button.t)
    (form : Dom_html.formElement Js.t) =

  let new_password : string Textfield.t =
    match Element.query_selector form Selector.new_password with
    | None -> failwith @@ name ^ ": new password input not found"
    | Some x ->
      Js.Opt.case (Element.get_parent x)
        (fun () -> failwith @@ name ^ ": new password input parent not found")
        (Textfield.attach ~validation:Validation.new_password) in

  let confirm_password : string Textfield.t =
    match Element.query_selector form Selector.confirm_password with
    | None -> failwith @@ name ^ ": new confirm password input not found"
    | Some x ->
      Js.Opt.case (Element.get_parent x)
        (fun () -> failwith @@ name ^ ": confirm password input parent not found")
        (Textfield.attach ~validation:(Validation.confirm new_password)) in
  object(self)

    val username : Dom_html.inputElement Js.t =
      match Element.query_selector form Selector.username with
      | None -> failwith @@ name ^ ": username input not found"
      | Some x -> Js.Unsafe.coerce x

    val old_password : string Textfield.t =
      match Element.query_selector form Selector.old_password with
      | None -> failwith @@ name ^ ": old password input not found"
      | Some x ->
        Js.Opt.case (Element.get_parent x)
          (fun () -> failwith @@ name ^ ": old password input parent not found")
          (Textfield.attach ~validation:Validation.old_password)

    val submit_input : Dom_html.inputElement Js.t =
      match Element.query_selector form Selector.submit_input with
      | None -> failwith @@ name ^ ": submit input element not found"
      | Some x -> Js.Unsafe.coerce x

    val mutable listeners_ = []

    inherit Widget.t form () as super

    method! initial_sync_with_dom () : unit =
      let invalid ?use_capture x =
        Lwt_js_events.make_event ?use_capture (Dom_html.Event.make "invalid") x in
      listeners_ <- Lwt_js_events.(
          [ seq_loop invalid old_password#input_element (self#handle_invalid `O)
          ; seq_loop invalid new_password#input_element (self#handle_invalid `N)
          ; seq_loop invalid confirm_password#input_element (self#handle_invalid `C)
          ; inputs new_password#input_element self#handle_new_password_input
          ; Textfield.Event.icons old_password#root (self#handle_icon `O)
          ; Textfield.Event.icons new_password#root (self#handle_icon `N)
          ; Textfield.Event.icons confirm_password#root (self#handle_icon `C)
          ]);
      form##.onsubmit := Dom.handler self#handle_submit;
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      old_password#destroy ();
      new_password#destroy ();
      confirm_password#destroy ();
      List.iter Lwt.cancel listeners_;
      listeners_ <- [];
      super#destroy ()

    method set_disabled (x : bool) : unit =
      List.iter (fun input ->
          if x then input#set_attribute Attr.hidden "true"
          else input#remove_attribute Attr.hidden;
          input#set_disabled x)
        [old_password; new_password; confirm_password]

    method form : Dom_html.formElement Js.t = form

    method submit_input : Dom_html.inputElement Js.t = submit_input

    (* Private methods *)

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
      confirm_password#force_custom_validation ();
      Lwt.return_unit

    method private handle_icon typ e _ : unit Lwt.t =
      let target = Dom_html.eventTarget e in
      let input = match typ with
        | `O -> old_password
        | `N -> new_password
        | `C -> confirm_password in
      let typ, icon = match Js.to_string input#input_element##._type with
        | "password" -> "text", Some Icon.SVG.Path.eye
        | "text" -> "password", Some Icon.SVG.Path.eye_off
        | s -> s, None in
      (match icon with
       | None -> ()
       | Some icon ->
         match Element.query_selector target "path" with
         | None -> ()
         | Some path ->
           let (path : Dom_svg.pathElement Js.t) = Js.Unsafe.coerce path in
           path##setAttribute (Js.string "d") (Js.string icon));
      Element.set_attribute input#input_element "type" typ;
      Lwt.return_unit

    method private handle_submit e : bool Js.t =
      Dom.preventDefault e;
      let username = User.of_string @@ Js.to_string username##.value in
      let old_pass = old_password#value in
      let new_pass = new_password#value in
      let thread = match username, old_pass, new_pass with
        | Ok usr, Some old_pass, Some new_pass ->
          let pass = { User. user = usr; old_pass; new_pass } in
          (Application_http_js.set_user_password pass
           >>= function
           | Ok () ->
             if User.equal user usr
             then Dom_html.window##.location##reload;
             Lwt.return_ok ()
           | Error e ->
             let msg = Api_js.Http.error_to_string e in
             old_password#set_helper_text_content msg;
             old_password#set_use_native_validation false;
             old_password#set_valid false;
             old_password#set_use_native_validation true;
             Lwt.return_error e)
        | _ -> Lwt.return_ok () in
      submit_button#set_loading_lwt thread;
      Lwt.async (fun () ->
          thread
          >>= fun x ->
          let label = match x with
            | Ok () -> "Пароль успешно изменен"
            | Error e ->
              Printf.sprintf "Не удалось изменить пароль. %s"
              @@ Api_js.Http.error_to_string e in
          let snackbar = Snackbar.make ~label () in
          set_snackbar snackbar
          >>= fun _ -> snackbar#destroy (); Lwt.return_unit);
      Js._false

  end
