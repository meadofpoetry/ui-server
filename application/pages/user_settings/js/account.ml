open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Page_user_settings_tyxml.Account
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let name = "account"

let ( >>= ) = Lwt.bind

module Selector = struct
  let action = Printf.sprintf ".%s" Card.CSS.action

  let accounts_info = Printf.sprintf ".%s" CSS.accounts_info_link
end

let logout ?href () : unit = Js.Unsafe.global##logout (Js.Optdef.option href)

let make_accounts_info_dialog () =
  let section (user : Application_types.User.t) =
    let title' =
      Format.asprintf "%a" Page_user_settings_tyxml.Util.pp_user_human user
    in
    let text = permissions ~pesonal_appeal:false user in
    let icon =
      Icon.D.SVG.icon ~d:(Page_user_settings_tyxml.Util.user_icon_path user) ()
    in
    Js_of_ocaml_tyxml.Tyxml_js.Html.(
      div
        ~a:[ a_class [ CSS.account_info ] ]
        [
          div ~a:[ a_class [ CSS.account_info_title ] ] [ icon; txt title' ];
          div ~a:[ a_class [ CSS.account_info_text ] ] [ txt text ];
        ])
  in
  let title = "Типы учётных записей" in
  let children = [ section `Guest; section `Operator; section `Root ] in
  let title = Dialog.D.dialog_title ~title () in
  let content = Dialog.D.dialog_content ~children () in
  let actions = Dialog.D.[ dialog_action ~label:"Ok" ~action:Close () ] in
  Dialog.make ~title ~content ~actions ()

class t (elt : Dom_html.element Js.t) =
  object
    val info_dialog : Dialog.t = make_accounts_info_dialog ()

    val accounts_info_link : Dom_html.element Js.t =
      match Element.query_selector elt Selector.accounts_info with
      | None -> failwith @@ name ^ ": accounts info link not found"
      | Some x -> x

    val exit_button : Button.t =
      match Element.query_selector elt Selector.action with
      | None -> failwith @@ name ^ ": exit button not found"
      | Some x -> Button.attach x

    val mutable _listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      _listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [
            clicks accounts_info_link (fun _ _ ->
                info_dialog#open_await () >>= fun _ -> Lwt.return_unit);
          ];
      info_dialog#append_to_body ();
      super#init ()

    method! destroy () : unit =
      info_dialog#remove_from_dom ();
      List.iter Lwt.cancel _listeners;
      _listeners <- [];
      info_dialog#destroy ();
      exit_button#destroy ();
      super#destroy ()
  end

let make user : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ D.create user
  in
  new t elt
