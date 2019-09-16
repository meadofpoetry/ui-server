open Js_of_ocaml
open Components
include Page_server_settings_tyxml.Https
module Markup_js =
  Page_server_settings_tyxml.Https.Make
    (Js_of_ocaml_tyxml.Tyxml_js.Xml)
    (Js_of_ocaml_tyxml.Tyxml_js.Svg)
    (Js_of_ocaml_tyxml.Tyxml_js.Html)

let ( >>= ) = Lwt.bind

let name = "HTTPS"

module Selector = struct
  let submit = Printf.sprintf ".%s" Card.CSS.action

  let enable = Printf.sprintf "#%s" enable_id
end

class t ~set_snackbar (elt : Dom_html.element Js.t) =
  object (self)
    val enable : Switch.t Form_field.t =
      match Element.query_selector elt Selector.enable with
      | None -> failwith @@ name ^ ": enable input field not found"
      | Some x -> Form_field.attach Switch.attach x

    val submit_button : Button.t =
      match Element.query_selector elt Selector.submit with
      | None -> failwith @@ name ^ ": submit button not found"
      | Some x -> Button.attach x

    val mutable listeners_ = []

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      listeners_ <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [clicks submit_button#root self#handle_submit_click];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel listeners_;
      listeners_ <- [];
      enable#destroy ();
      submit_button#destroy ();
      super#destroy ()

    method set_value (x : bool) : unit = (enable#input)#toggle ~force:x ()

    method private handle_submit_click _ _ : unit Lwt.t =
      Server_http_js.set_https_enabled (enable#input)#checked
      >>= function
      | Ok () ->
          let label =
            Printf.sprintf
              "HTTPS %s. Настройки вступят в силу после \
               перезагрузки прибора."
              (if (enable#input)#checked then "включен" else "выключен")
          in
          let snackbar = Snackbar.make ~label:(`Text label) () in
          set_snackbar snackbar >>= fun () -> Lwt.return @@ snackbar#destroy ()
      | Error (`Msg e) ->
          let snackbar = Snackbar.make ~label:(`Text e) () in
          set_snackbar snackbar >>= fun () -> Lwt.return @@ snackbar#destroy ()
  end

let make ~set_snackbar (init : Server_types.settings) : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element @@ Markup_js.create init
  in
  new t ~set_snackbar elt
