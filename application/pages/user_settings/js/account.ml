open Js_of_ocaml
open Components

let name = "account"

let ( >>= ) = Lwt.bind

module Selector = struct
  let logout = "a[href=\"/logout\"]"
  let change_role = Printf.sprintf ".%s" Card.CSS.action
  let accounts_info = Printf.sprintf ".%s" Markup.CSS.Account.accounts_info_link
end

let logout ?(reload = true) () : unit =
  Js.Unsafe.global##logout (Js.bool reload)

let make_accounts_info_dialog () =
  let section (user : Application_types.User.t) =
    let title' = Format.asprintf "%a" Markup.pp_user_human user in
    let text = Markup.Account.permissions ~pesonal_appeal:false user in
    Js_of_ocaml_tyxml.Tyxml_js.Html.(
      div ~a:[a_class [Markup.CSS.Account.account_info]]
        [ div ~a:[a_class [Markup.CSS.Account.account_info_title]] [txt title']
        ; div ~a:[a_class [Markup.CSS.Account.account_info_text]] [txt text]
        ]) in
  let title = "Типы учётных записей" in
  let content =
    [ section `Guest
    ; section `Operator
    ; section `Root
    ] in
  let title =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_title_simple ~title () in
  let content =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_content ~content () in
  let actions =
    [ Dialog.make_action ~label:"Ok" ~action:Close ()
    ] in
  Dialog.make ~title ~content ~actions ()

class t (elt : Dom_html.element Js.t) = object

  val info_dialog : Dialog.t = make_accounts_info_dialog ()

  val accounts_info_link : Dom_html.element Js.t =
    match Element.query_selector elt Selector.accounts_info with
    | None -> failwith @@ name ^ ": accounts info link not found"
    | Some x -> x

  val logout_button : Button.t option =
    match Element.query_selector elt Selector.logout with
    | None -> None
    | Some x ->
      let on_click = fun _ e _ ->
        Dom.preventDefault e;
        logout ~reload:false ();
        Dom_html.window##.location##replace (Js.string "/");
        Lwt.return_unit in
      Some (Button.attach ~on_click x)

  val change_button : Button.t =
    match Element.query_selector elt Selector.change_role with
    | None -> failwith @@ name ^ ": change role button not found"
    | Some x ->
      let on_click = fun _ _ _ -> Lwt.return @@ logout ~reload:true () in
      Button.attach ~on_click x

  val mutable _listeners = []

  inherit Widget.t elt () as super

  method! init () : unit =
    _listeners <- Js_of_ocaml_lwt.Lwt_js_events.(
        [ clicks accounts_info_link (fun _ _ ->
              info_dialog#open_await ()
              >>= fun _ -> Lwt.return_unit)
        ]);
    info_dialog#append_to_body ();
    super#init ()

  method! destroy () : unit =
    info_dialog#remove_from_dom ();
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    info_dialog#destroy ();
    Utils.Option.iter Widget.destroy logout_button;
    change_button#destroy ();
    super#destroy ()
end

let make user : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Markup.Account.make user in
  new t elt
