open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Netlib.Uri
open Page_server_settings_tyxml

(* let ( >>= ) = Lwt.bind *)

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Api_http = Api_js.Http.Make(Body)

let read_file filename =
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := !lines ^ input_line chan ^ "\n";
    done; !lines
  with End_of_file ->
    close_in chan;
    !lines

let make_card user =
  let _username = match user with
    | `Root -> "администратора"
    | `Operator -> "оператора"
    | `Guest -> "гостя" in

  (* TODO add https flag and permission check *)

  let key_field =
    Dom_html.createInput ~_type:(Js.string "file") Dom_html.document
  in
  let key_but = Button.make
      ~label:"Сохранить ключ"
      ~on_click:(fun _ _ _ ->
          match Js.Optdef.to_option @@ key_field##.files with
          | None -> Lwt.return @@ print_endline "no file 1"
          | Some files ->
            match Js.Opt.to_option @@ files##item 0 with
            | None -> Lwt.return @@ print_endline "no file 2"
            | Some file ->
              let freader = new%js File.fileReader in
              freader##readAsBinaryString file;
              freader##.onload :=
                Dom.handler (fun _ ->
                    let file_blob = Obj.magic freader##.result in
                    Js.Unsafe.global##.console##log file_blob |> ignore;
                    Api_js.Http.perform_file
                      ~file:file_blob
                      ~path:Path.Format.("api/server/config/key" @/ String ^/empty)
                      ~query:Query.empty
                      (Js.to_string file##.name)
                      (fun _ _ -> Lwt.return @@ print_endline "Done")
                    |> ignore;
                    Js._true);
              Lwt.return_unit)
      ()
  in

  let cert_field =
    Dom_html.createInput ~_type:(Js.string "file") Dom_html.document
  in
  let cert_but = Button.make
      ~label:"Сохранить сертификат"
      ~on_click:(fun _ _ _ ->
          match Js.Optdef.to_option @@ cert_field##.files with
          | None -> Lwt.return @@ print_endline "no file"
          | Some files ->
            match Js.Opt.to_option @@ files##item 0 with
            | None -> Lwt.return @@ print_endline "no file"
            | Some file ->
              let freader = new%js File.fileReader in
              freader##readAsBinaryString file;
              freader##.onload :=
                Dom.handler (fun _ ->
                    let file_blob = Obj.magic freader##.result in
                    Js.Unsafe.global##.console##log file_blob |> ignore;
                    Api_js.Http.perform_file
                      ~file:file_blob
                      ~path:Path.Format.("api/server/config/cert" @/ String ^/empty)
                      ~query:Query.empty
                      (Js.to_string file##.name)
                      (fun _ _ -> Lwt.return @@ print_endline "Done")
                    |> ignore;
                    Js._true);
              Lwt.return_unit)
      () in

  let reset = Button.make
      ~label:"Перезапустить сервер"
      ~on_click:(fun _ _ _ ->
          Api_http.perform_unit
            ~meth:`POST
            ~path:Path.Format.("api/server/config/server_restart" @/ empty)
            ~query:Query.empty
            (fun _ _ -> Lwt.return_unit))
      () in

  let cont =
    Box.make
      ~dir:`Column
      [ Widget.create key_field
      ; key_but#widget
      ; Widget.create cert_field
      ; cert_but#widget
      ; reset#widget
      ] in
  cont

module Markup = Cert_viewer.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let user = Js_of_ocaml.(Js.to_string @@ Js.Unsafe.variable "username") in
  let user = match Application_types.User.of_string user with
    | Error e -> failwith e
    | Ok user -> user
  in
  let thread =
    Server_http_js.get_config ()
    >>=? fun _config ->
    let tz_offset_s = Ptime_clock.current_tz_offset_s () in
    let cert =
      Tyxml_js.To_dom.of_element
      @@ Markup.of_certificate ?tz_offset_s (match _config.tls_cert with
          | None -> assert false
          | Some x -> snd x) in
    let card = make_card user in
    Dom.appendChild card#root cert;
    Lwt.return_ok card in
  let body = Ui_templates.Loader.create_widget_loader thread in
  scaffold#set_body body
