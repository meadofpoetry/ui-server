open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

type _ typ =
  | Key : string option typ
  | Crt : (string * Server_types.certificate) option typ

let max_file_size = 500_000 (* bytes *)

let file_size_to_string (x : int) =
  let threshold = 1000. in
  let units = ["kB"; "MB"; "GB"; "TB"] in
  let rec aux acc = function
    | [] -> acc
    | unit :: tl ->
      if (fst acc) > threshold
      then aux ((fst acc) /. threshold, unit) tl
      else acc in
  let size, unit = aux (float_of_int x, "B") units in
  Printf.sprintf "%g %s" size unit

let remove_file (type a) : a typ -> (unit, Api_js.Http.error) Lwt_result.t =
  function
  | Key -> Server_http_js.delete_tls_key ()
  | Crt -> Server_http_js.delete_tls_crt ()

let upload_file (type a) :
  ?upload_progress:(int -> int -> unit)
  -> string
  -> File.blob Js.t
  -> a typ
  -> (unit, Api_js.Http.error) Lwt_result.t =
  fun ?upload_progress name blob -> function
    | Key -> Server_http_js.set_tls_key ?upload_progress ~name blob
    | Crt -> Server_http_js.set_tls_crt ?upload_progress ~name blob

let fetch_value (type a) typ =
  let value : a typ -> Server_types.settings -> a =
    fun typ s -> match typ with
      | Key -> s.tls_key
      | Crt -> s.tls_cert in
  Server_http_js.get_config ()
  >>=? Lwt.return_ok % value typ

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

class type dialog = object
  inherit Dialog.t
  method do_not_show : bool
  method reset : unit -> unit
end

let make_disclaimer_dialog () =
  let title = "Сохранить приватный ключ?" in
  let message =
    "Сохранение приватного ключа требует отправки \
     содержимого файла по сети. В случае, если передача \
     приватного ключа осуществляется через публичную сеть \
     или с использованием незащищенного канала связи, \
     доступ к содержимому файла может быть получен третьми лицами." in
  let do_not_show =
    Form_field.make
      ~label:"Больше не показывать это сообщение"
      (Checkbox.make ()) in
  let title = Dialog.Markup.create_title_simple ~title () in
  let content =
    Dialog.Markup.create_content
      ~content:[ Tyxml_js.Html.txt message
               ; do_not_show#markup
               ]
      () in
  let actions =
    [ Dialog.Markup.create_action ~action:Close ~label:"Отмена" ()
    ; Dialog.Markup.create_action ~action:Accept ~label:"Сохранить" ()
    ] in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_simple ~title ~content ~actions () in
  object
    inherit Dialog.t elt ()
    method reset () : unit = do_not_show#input#toggle ~force:false ()
    method do_not_show : bool = do_not_show#input#checked
  end

let make_warning_dialog (type a) (typ : a typ) =
  let title, file = match typ with
    | Key -> "Удалить приватный ключ?", "приватного ключа"
    | Crt -> "Удалить сертификат?", "сертификата" in
  let content =
    Printf.sprintf
      "Удаление %s приведет к невозможности использования \
       защищенного протокола передачи данных HTTPS для доступа к \
       анализатору. Изменения вступят в силу при перезапуске прибора."
      file in
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

class ['a] t ?value
    ~(typ : 'a typ)
    ~(set_snackbar : Snackbar.t -> Snackbar.dismiss_reason Lwt.t)
    (elt : Dom_html.element Js.t) = object(self)

  val filename : Dom_html.element Js.t =
    match Element.query_selector elt Selector.filename with
    | None -> failwith @@ "settings row: no filename element found"
    | Some x -> x

  val warning_dialog : Dialog.t = make_warning_dialog typ

  val info_dialog : Dialog.t option =
    let f (type a) : a typ -> Dialog.t option = function
      | Key -> None
      | Crt -> Some (make_certificate_dialog ()) in
    f typ

  val disclaimer_dialog : dialog option =
    let f (type a) : a typ -> dialog option = function
      | Key -> Some (make_disclaimer_dialog ())
      | Crt -> None in
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
    (match disclaimer_dialog with
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
    (match disclaimer_dialog with
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

  method private handle_file_loaded ~name ~size e =
    let upload_progress = fun cur tot ->
      let v = (float_of_int cur) /. (float_of_int tot) in
      import#loader#set_value v in
    let upload_file blob =
      Lwt_result.map_err Api_js.Http.error_to_string
        (upload_file ~upload_progress name blob typ
         >>=? fun () -> fetch_value typ
         >>=? Lwt.return_ok % self#set_value) in
    let thread =
      Js.Opt.case (File.CoerceTo.string @@ (Dom.eventTarget e)##.result)
        (fun () -> Lwt.return_error "Не удалось открыть файл")
        (fun data ->
           if size > max_file_size
           then
             Lwt.return_error
             @@ Printf.sprintf "Превышен допустимый размер файла (%s)"
             @@ file_size_to_string max_file_size
           else
             let blob = File.blob_from_any [`js_string data] in
             match Storage.get_show_private_key_disclaimer (),
                   disclaimer_dialog with
             | false, _ | _, None -> upload_file blob
             | true, Some dialog ->
               dialog#reset ();
               dialog#open_await ()
               >>= function
               | Close | Destroy | Custom _ -> Lwt.return_ok ()
               | Accept ->
                 Storage.set_show_private_key_disclaimer @@ not dialog#do_not_show;
                 upload_file blob) in
    import#button#set_loading_lwt thread;
    Lwt.on_termination thread (fun () -> import#loader#set_value 0.);
    Lwt.async (fun () ->
        thread >>= function
        | Ok () -> Lwt.return_unit
        | Error e ->
          let snackbar = Snackbar.make ~label:e () in
          set_snackbar snackbar
          >>= fun _ -> Lwt.return @@ snackbar#destroy ());
    Js._true

  method private handle_files _ _ : unit Lwt.t =
    match Js.Optdef.to_option @@ import#file_input##.files with
    | None -> Lwt.return_unit
    | Some files ->
      Js.Opt.case (files##item 0)
        (fun () -> Lwt.return_unit)
        (fun file ->
           let freader = new%js File.fileReader in
           freader##.onload := Dom.handler (
               self#handle_file_loaded
                 ~name:(Js.to_string file##.name)
                 ~size:file##.size);
           freader##readAsBinaryString file;
           import#file_input##.value := Js.string "";
           Lwt.return_unit)

  method private handle_info _ _ : unit Lwt.t =
    match info_dialog with
    | None -> Lwt.return_unit
    | Some dialog ->
      match dialog#content with
      | None -> Lwt.return_unit
      | Some content ->
        let value = match self#value with
          | Some x -> Lwt.return_ok x
          | None -> fetch_value typ in
        let info =
          Ui_templates.Loader.create_widget_loader
            (Lwt_result.map_err Api_js.Http.error_to_string value
             >>=? fun x ->
             self#set_value x;
             Lwt.return_ok @@ Widget.create @@ make_info typ x) in
        Element.remove_children content;
        Dom.appendChild content info#root;
        dialog#open_await () >>= fun _ -> Lwt.return_unit

  method private handle_remove _ _ : unit Lwt.t =
    warning_dialog#open_await ()
    >>= function
    | Close | Destroy | Custom _ -> Lwt.return_unit
    | Accept ->
      remove_file typ
      >>=? (fun () -> fetch_value typ)
      >>= function
      | Ok v -> Lwt.return @@ self#set_value v
      | Error e ->
        let label = Api_js.Http.error_to_string e in
        let snackbar = Snackbar.make ~label () in
        set_snackbar snackbar
        >>= fun _ -> Lwt.return @@ snackbar#destroy ()
end
