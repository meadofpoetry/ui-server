open Js_of_ocaml
open Components
include Page_server_settings_tyxml.Certificate
module Markup_js =
  Page_server_settings_tyxml.Certificate.Make
    (Js_of_ocaml_tyxml.Tyxml_js.Xml)
    (Js_of_ocaml_tyxml.Tyxml_js.Svg)
    (Js_of_ocaml_tyxml.Tyxml_js.Html)

let ( >>= ) = Lwt.bind

let name = "Certificate"

module Selector = struct
  let certificate = Printf.sprintf ".%s" CSS.certificate

  let private_key = Printf.sprintf ".%s" CSS.private_key
end

class t
  ?(init : Server_types.settings option)
  ~set_snackbar
  (elt : Dom_html.element Js.t) =
  object
    val certificate : _ X509_file_config.t =
      match Element.query_selector elt Selector.certificate with
      | None -> failwith @@ name ^ ": no certificate settings block found"
      | Some x ->
          let value =
            match init with
            | None -> None
            | Some x -> Some x.tls_cert
          in
          new X509_file_config.t ?value ~set_snackbar ~typ:Crt x

    val private_key : _ X509_file_config.t =
      match Element.query_selector elt Selector.private_key with
      | None -> failwith @@ name ^ ": no private key settings block found"
      | Some x ->
          let value =
            match init with
            | None -> None
            | Some x -> Some x.tls_key
          in
          new X509_file_config.t ?value ~set_snackbar ~typ:Key x

    inherit Widget.t elt () as super

    method! destroy () : unit =
      certificate#destroy ();
      private_key#destroy ();
      super#destroy ()

    method set_value (x : Server_types.settings) : unit =
      certificate#set_value x.tls_cert;
      private_key#set_value x.tls_key
  end

let make ~set_snackbar (init : Server_types.settings) : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element @@ Markup_js.create init
  in
  new t ~set_snackbar ~init elt
