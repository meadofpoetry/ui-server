open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components

let ( >>= ) = Lwt_result.bind

type _ typ =
  | Key : string option typ
  | Crt : (string * Server_types.certificate) option typ

let remove_file (type a) : a typ -> (unit, Api_js.Http.error) Lwt_result.t =
  function
  | Key -> Server_http_js.delete_tls_key ()
  | Crt -> Server_http_js.delete_tls_crt ()

let send_file (type a) :
  string
  -> File.blob Js.t
  -> a typ
  -> (unit, Api_js.Http.error) Lwt_result.t =
  fun name blob -> function
    | Key -> Server_http_js.set_tls_key ~name blob
    | Crt -> Server_http_js.set_tls_crt ~name blob

let fetch_value (type a) typ =
  let value : Server_types.settings -> a typ -> a = fun s ->
    function Key -> s.tls_key | Crt -> s.tls_cert in
  Server_http_js.get_config ()
  >>= fun x -> Lwt.return_ok (value x typ)

let make_info (type a) : a typ -> a -> Dom_html.element Js.t =
  fun typ v ->
  match typ with
  | Key -> Dom_html.(createDiv document) (* TODO *)
  | Crt ->
    match v with
    | None -> Dom_html.(createDiv document) (* TODO *)
    | Some (_, v) ->
      Tyxml_js.To_dom.of_element
      @@ Markup.Certificate_viewer.of_certificate v

let update_row_state (type a) : a -> Dom_html.element Js.t -> a typ -> unit =
  fun v row typ ->
  let force = match typ with
    | Key -> Utils.Option.is_none v
    | Crt -> Utils.Option.is_none v in
  ignore @@ Element.toggle_class ~force row Markup.CSS.Certificate.empty

let filename_of_value (type a) : a -> a typ -> string option =
  fun v -> function
    | Key -> v
    | Crt -> match v with None -> None | Some (n, _) -> Some n

let make_warning_dialog (type a) (typ : a typ) =
  let title, file = match typ with
    | Key -> "Удалить приватный ключ?", "приватного ключа"
    | Crt -> "Удалить сертификат?", "сертификата" in
  let content =
    Printf.sprintf
      "Удаление %s приведет к невозможности использования \
       защищенного протокола передачи данных HTTPS для доступа к \
       анализатору. Изменения вступят в силу при перезапуске прибора. \
       Хотите продолжить?" file in
  let title =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_title_simple ~title () in
  let content =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_content_simple content () in
  let actions =
    [ Dialog.make_action ~action:Close ~label:"Отмена" ()
    ; Dialog.make_action ~action:Accept ~label:"Удалить" ()
    ] in
  Dialog.make ~title ~content ~actions ()

let make_certificate_dialog () =
  let title =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_title_simple ~title:"Сертификат" () in
  let content =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_content ~content:[] () in
  let actions = [Dialog.make_action ~action:Close ~label:"ОК" ()] in
  Dialog.make ~title ~content ~actions ()

module Selector = struct
  let filename = Printf.sprintf ".%s" Markup.CSS.Certificate.filename
  let action_info = Printf.sprintf ".%s" Markup.CSS.Certificate.action_info
  let action_remove = Printf.sprintf ".%s" Markup.CSS.Certificate.action_remove
  let action_import = Printf.sprintf ".%s" Markup.CSS.Certificate.action_import
end

class ['a] t ?value ~(typ : 'a typ) (elt : Dom_html.element Js.t) = object(self)

  val filename : Dom_html.element Js.t =
    match Element.query_selector elt Selector.filename with
    | None -> failwith @@ "settings row: no filename element found"
    | Some x -> x

  val warning_dialog : Dialog.t =
    make_warning_dialog typ

  val info_dialog : Dialog.t option =
    let f (type a) : a typ -> Dialog.t option = function
      | Key -> None
      | Crt -> Some (make_certificate_dialog ()) in
    f typ

  val info : Icon_button.t option =
    match Element.query_selector elt Selector.action_info with
    | None -> None
    | Some x -> Some (Icon_button.attach x)

  val remove : Icon_button.t =
    match Element.query_selector elt Selector.action_remove with
    | None -> failwith @@ "settings row: no remove action found"
    | Some x -> Icon_button.attach x

  val import : File_button.t =
    match Element.query_selector elt Selector.action_import with
    | None -> failwith @@ "settings row: no import action found"
    | Some x -> new File_button.t x

  val mutable value_ : 'a option = value
  val mutable listeners_ = []

  inherit Widget.t elt () as super

  method! init () : unit =
    (match info_dialog with
     | None -> ()
     | Some dialog -> Dom.appendChild Dom_html.document##.body dialog#root);
    Dom.appendChild Dom_html.document##.body warning_dialog#root;
    super#init ()

  method! destroy () : unit =
    (match info_dialog with
     | None -> ()
     | Some dialog ->
       dialog#destroy ();
       Element.remove_child_safe Dom_html.document##.body dialog#root);
    Element.remove_child_safe Dom_html.document##.body warning_dialog#root;
    warning_dialog#destroy ();
    (match info with
     | None -> ()
     | Some x -> x#destroy ());
    remove#destroy ();
    import#destroy ();
    List.iter Lwt.cancel listeners_;
    listeners_ <- [];
    super#destroy ()

  method! initial_sync_with_dom () : unit =
    listeners_ <- Lwt_js_events.(
        [ clicks remove#root self#handle_remove
        ; changes import#file_input self#handle_files
        ]);
    (match info with
     | None -> ()
     | Some x ->
       listeners_ <- Lwt_js_events.(clicks x#root self#handle_info)
                     :: listeners_);
    super#initial_sync_with_dom ()

  method set_value (v : 'a) : unit =
    value_ <- Some v;
    update_row_state v super#root typ;
    let name = match filename_of_value v typ with
      | None -> Markup.Certificate.empty
      | Some name -> name in
    filename##.textContent := Js.some @@ Js.string name

  method value : 'a option = value_

  (* Private methods *)

  method private handle_files _ _ : unit Lwt.t =
    match Js.Optdef.to_option @@ import#file_input##.files with
    | None -> Lwt.return_unit
    | Some files ->
      Js.Opt.case (files##item 0)
        (fun () -> Lwt.return_unit)
        (fun file ->
           let freader = new%js File.fileReader in
           let handler = fun e ->
             let target = Dom.eventTarget e in
             let blob = target##.result in
             let thread =
               Js.Opt.case (File.CoerceTo.string blob)
                 (fun () -> Lwt.return_error (`Error "Не удалось открыть файл"))
                 (fun data ->
                    let name = Js.to_string file##.name in
                    let blob = File.blob_from_any [`js_string data] in
                    send_file name blob typ
                    >>= fun () -> fetch_value typ
                    >>= fun v -> (* TODO handle error at each stage *)
                    self#set_value v;
                    Lwt.return_ok ()) in
             import#button#set_loading_lwt thread;
             Js._true in
           freader##.onload := Dom.handler handler;
           freader##readAsBinaryString file;
           Lwt.return_unit)

  method private handle_info _ _ : unit Lwt.t =
    match info_dialog with
    | None -> Lwt.return_unit
    | Some dialog ->
      let value = match self#value with
        | Some x -> Lwt.return_ok x
        | None -> fetch_value typ in
      let info =
        Ui_templates.Loader.create_widget_loader
          (value
           |> Lwt_result.map_err Api_js.Http.error_to_string
           >>= fun x ->
           self#set_value x;
           Lwt.return_ok
           @@ Widget.create
           @@ make_info typ x) in
      (match dialog#content with
       | None -> ()
       | Some content ->
         Element.remove_children content;
         Dom.appendChild content info#root);
      Lwt.(dialog#open_await () >>= fun _ -> Lwt.return_unit)

  method private handle_remove _ _ : unit Lwt.t =
    let ( >>=? ) = Lwt_result.bind in
    let ( >>= ) = Lwt.bind in
    warning_dialog#open_await ()
    >>= function
    | Close | Destroy | Custom _ -> Lwt.return_unit
    | Accept ->
      remove_file typ
      >>=? (fun () -> fetch_value typ)
      >>= function
      | Ok v -> self#set_value v; Lwt.return_unit
      | Error _ -> Lwt.return_unit (* TODO handle error *)
end
