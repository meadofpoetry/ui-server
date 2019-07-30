open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

let ( >>= ) = Lwt.bind

let name = "Certificate"

module Selector = struct
  let certificate = Printf.sprintf ".%s" Markup.CSS.Certificate.certificate
  let private_key = Printf.sprintf ".%s" Markup.CSS.Certificate.private_key
end

class t
    ?(init : Server_types.settings option)
    ~set_snackbar
    (elt : Dom_html.element Js.t) = object
  val certificate : _ X509_file_config.t =
    match Element.query_selector elt Selector.certificate with
    | None -> failwith @@ name ^ ": no certificate settings block found"
    | Some x ->
      let value = match init with
        | None -> None
        | Some x -> Some x.tls_cert in
      new X509_file_config.t ?value ~set_snackbar ~typ:Crt x

  val private_key : _ X509_file_config.t =
    match Element.query_selector elt Selector.private_key with
    | None -> failwith @@ name ^ ": no private key settings block found"
    | Some x ->
      let value = match init with
        | None -> None
        | Some x -> Some x.tls_key in
      new X509_file_config.t ?value ~set_snackbar ~typ:Key x

  inherit Widget.t elt () as super

  method! destroy () : unit =
    super#destroy ()
end

let make
    ~set_snackbar
    (init : Server_types.settings) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.Certificate.make init in
  new t ~set_snackbar ~init elt
