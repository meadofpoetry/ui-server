open Js_of_ocaml
open Js_of_ocaml_lwt
open Components
open Application_types

let name = "Password config"

let ( >>= ) = Lwt.bind

module Attr = struct
  let username = "data-username"
end

module Selector = struct
  open Printf
  let username = sprintf "input[name=\"username\"]"
  let old_password user = sprintf "#%s" @@ Markup.Password.old_pass_id user
  let new_password user = sprintf "#%s" @@ Markup.Password.new_pass_id user
  let confirm_password user = sprintf "#%s" @@ Markup.Password.confirm_pass_id user
  let user_tabs = sprintf ".%s" Tab_bar.CSS.root
  let form user = sprintf "#%s" @@ Markup.Password.form_id user
  let submit = sprintf ".%s.%s" Card.CSS.action Button.CSS.root
  let slider = sprintf ".%s" Markup.CSS.Password.slider
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

module Form = struct

  let name = "password-change-form"

  class t (submit_button : Button.t)
      (form : Dom_html.formElement Js.t) =
    let _for : User.t =
      match Element.get_attribute form Attr.username with
      | None -> failwith @@ name ^ ": usename attribute not found"
      | Some x -> match User.of_string x with
        | Error e -> failwith @@ name ^ ": " ^ e
        | Ok x -> x in

    let username : Dom_html.inputElement Js.t =
      match Element.query_selector form Selector.username with
      | None -> failwith @@ name ^ ": username input not found"
      | Some x -> Js.Unsafe.coerce x in

    let old_password : string Textfield.t =
      match Element.query_selector form (Selector.old_password _for) with
      | None -> failwith @@ name ^ ": old password input not found"
      | Some x -> Textfield.attach ~validation:Validation.old_password x in

    let new_password : string Textfield.t =
      match Element.query_selector form (Selector.new_password _for) with
      | None -> failwith @@ name ^ ": new password input not found"
      | Some x -> Textfield.attach ~validation:Validation.new_password x in

    let confirm_password : string Textfield.t =
      match Element.query_selector form (Selector.confirm_password _for) with
      | None -> failwith @@ name ^ ": new confirm password input not found"
      | Some x -> Textfield.attach ~validation:(Validation.confirm new_password) x in
    object(self)

      val user : User.t =
        match User.of_string @@ Js.to_string Js.Unsafe.global##.username with
        | Error _ -> failwith @@ name ^ ": invalid user"
        | Ok x -> x

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

      method form = form

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
        print_endline "submit";
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
        Js._false

    end

end

class t (elt : Dom_html.element Js.t) =
  let slider : Dom_html.element Js.t =
    match Element.query_selector elt Selector.slider with
    | None -> failwith @@ name ^ ": slider element not found"
    | Some x -> x in
  object(self)

    val submit_button : Button.t =
      match Element.query_selector elt Selector.submit with
      | None -> failwith @@ name ^ ": submit button not found"
      | Some x -> Button.attach x

    val user_tabs : Tab_bar.t =
      match Element.query_selector elt Selector.user_tabs with
      | None -> failwith @@ name ^ ": tab bar element not found"
      | Some x -> Tab_bar.attach ~on_change:(fun _ bar ->
          match bar#active_tab with
          | None -> Lwt.return_unit
          | Some tab ->
            let id = tab#index in
            let translate = Printf.sprintf "translate3d(%d%%, 0, 0)" (-100 * id) in
            slider##.style##.transform := Js.string translate;
            Lwt.return_unit) x

    val mutable forms = []
    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      forms <- (
        List.map (fun form -> new Form.t submit_button (Js.Unsafe.coerce form))
        @@ Element.query_selector_all elt "form");
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <- Lwt_js_events.(
          [ clicks submit_button#root self#handle_submit_click
          ]);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel listeners;
      listeners <- [];
      List.iter Widget.destroy forms;
      forms <- [];
      user_tabs#destroy ();
      submit_button#destroy ();
      super#destroy ()

    method private handle_submit_click _ _ : unit Lwt.t =
      match user_tabs#active_tab_index with
      | None -> Lwt.return_unit
      | Some i -> (List.nth forms i)#form##submit; Lwt.return_unit

  end

let make () : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Markup.Password.make () in
  new t elt
