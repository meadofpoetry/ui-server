open Js_of_ocaml
open Js_of_ocaml_tyxml
open Js_of_ocaml_lwt
open Components
include Page_user_settings_tyxml.Password
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

module Selector = struct
  open Printf

  let form user = sprintf "#%s" @@ D.form_id user

  let user_tabs = sprintf ".%s" Tab_bar.CSS.root

  let submit = sprintf ".%s.%s" Card.CSS.action Button.CSS.root

  let slider = sprintf ".%s" CSS.slider
end

class t ~set_snackbar (user : Application_types.User.t) (elt : Dom_html.element Js.t) =
  object (self)
    val slider : Dom_html.element Js.t =
      match Element.query_selector elt Selector.slider with
      | None -> failwith @@ CSS.root ^ ": slider element not found"
      | Some x -> x

    val submit_button : Button.t =
      match Element.query_selector elt Selector.submit with
      | None -> failwith @@ CSS.root ^ ": submit button not found"
      | Some x -> Button.attach x

    val user_tabs : Tab_bar.t =
      match Element.query_selector elt Selector.user_tabs with
      | None -> failwith @@ CSS.root ^ ": tab bar element not found"
      | Some x -> Tab_bar.attach x

    val mutable forms = []

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      forms <-
        List.mapi (fun i form ->
            let form =
              new Password_form.t
                ~set_snackbar
                user
                submit_button
                (Js.Unsafe.coerce form)
            in
            form#set_disabled (i <> 0);
            form)
        @@ Element.query_selector_all elt "form";
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        Lwt_js_events.
          [ clicks submit_button#root self#handle_submit_click
          ; Tab_bar.Lwt_js_events.changes user_tabs#root self#handle_user_change ];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Widget.destroy forms;
      forms <- [];
      user_tabs#destroy ();
      super#destroy ()

    method private handle_user_change e _ =
      let id = (Widget.event_detail e)##.index in
      let translate = Printf.sprintf "translate3d(%d%%, 0, 0)" (-100 * id) in
      List.iteri (fun i form -> form#set_disabled (i <> id)) forms;
      slider##.style##.transform := Js.string translate;
      Lwt.return_unit

    method private handle_submit_click _ _ : unit Lwt.t =
      match user_tabs#active_tab_index with
      | None -> Lwt.return_unit
      | Some i ->
          (List.nth forms i)#submit_input##click;
          Lwt.return_unit
  end

let make ~set_snackbar user : t =
  let (elt : Dom_html.element Js.t) = Tyxml_js.To_dom.of_element @@ D.create () in
  new t ~set_snackbar user elt
