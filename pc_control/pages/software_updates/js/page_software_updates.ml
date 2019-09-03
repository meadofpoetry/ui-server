open Js_of_ocaml
open Components
open Netlib.Uri
open Pc_control_types.Software_updates

type event = [`State of state]

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let status_to_string = function
  | Unknown -> ""
  | Wait -> ""
  | Setup -> ""
  | Running -> ""
  | Query -> ""
  | Info -> ""
  | Remove -> ""
  | Refresh_cache -> ""
  | Download -> ""
  | Install -> ""
  | Update -> ""
  | Cleanup -> ""
  | Obsolete -> ""
  | Dep_resolve -> ""
  | Sig_check -> ""
  | Test_commit -> ""
  | Commit -> ""
  | Request -> ""
  | Finished -> ""
  | Cancel -> ""
  | Download_repository -> ""
  | Download_packagelist -> ""
  | Download_filelist -> ""
  | Download_changelog -> ""
  | Download_group -> ""
  | Download_updateinfo -> ""
  | Repackaging -> ""
  | Loading_cache -> ""
  | Scan_applications -> ""
  | Generate_package_list -> ""
  | Waiting_for_lock -> ""
  | Waiting_for_auth -> ""
  | Scan_process_list -> ""
  | Check_executable_files -> ""
  | Check_libraries -> ""
  | Copy_files -> ""
  | Run_hook -> ""

module Selector = struct
  let action = Printf.sprintf ".%s" Button.CSS.root

  let progress = Printf.sprintf ".%s" Linear_progress.CSS.root

  let placeholder = Printf.sprintf ".%s" Components_lab.Placeholder.CSS.root

  let action_label = Printf.sprintf ".%s" Button.CSS.label
end

class t (elt : Dom_html.element Js.t) = object(self)
  inherit Widget.t elt () as super

  val placeholder =
    Components_lab.Placeholder.attach
    @@ Element.query_selector_exn elt Selector.placeholder

  val action =
    Button.attach
    @@ Element.query_selector_exn elt Selector.action

  val progress =
    Linear_progress.attach
    @@ Element.query_selector_exn elt Selector.progress

  val mutable listeners = []

  method! initial_sync_with_dom () : unit =
    listeners <- Js_of_ocaml_lwt.Lwt_js_events.(
        [ clicks action#root self#handle_action
        ] @ listeners);

  method! destroy () : unit =
    action#destroy ();
    progress#destroy ();
    placeholder#destroy ();
    super#destroy ()

  method notify : event -> unit Lwt.t = function
    | `State state -> self#handle_state_change state

  method private handle_state_change (state : state) : unit Lwt.t =
    match state with
    | `Checking (_status, v) | `Upgrading (_status, v) ->
      let v = min 100l v in
      progress#open_ ();
      progress#set_progress (Int32.to_float v /. 100.);
      Lwt.return_unit
    | `Updates_avail ->
      placeholder#set_error false;
      placeholder#set_text Markup.available_hint;
      self#set_action_label Markup.action_label_update;
      self#close_progress ()
    | `Need_reboot ->
      placeholder#set_error false;
      placeholder#set_text Markup.reboot_hint;
      self#set_action_label Markup.action_label_reboot;
      self#close_progress ()
    | `Unchecked ->
      placeholder#set_error false;
      placeholder#set_text Markup.unchecked_hint;
      self#set_action_label Markup.action_label_check_updates;
      self#close_progress ()
    | `Updates_not_avail ->
      placeholder#set_error false;
      placeholder#set_text Markup.not_available_hint;
      self#close_progress ()

  method private handle_action _ _ : unit Lwt.t =
    let thread =
      Pc_control_http_js.Updates.check_updates ()
      >>= function
      | Ok _ -> Lwt.return_unit
      | Error err ->
        placeholder#set_text
        @@ Printf.sprintf
          "Не удалось проверить наличие обновлений. \n%s"
          (Api_js.Http.error_to_string err);
        placeholder#set_error true;
        Lwt.return_unit in
    action#set_loading_lwt thread;
    thread

  method private close_progress () =
    if progress#has_class Linear_progress.CSS.closed
    then Lwt.return_unit
    else (
      progress#close ();
      Js_of_ocaml_lwt.Lwt_js_events.transitionend progress#root
      >>= fun () ->
      progress#set_progress 0.;
      Lwt.return_unit)

  method private set_action_label label =
    match Element.query_selector action#root Selector.action_label with
    | None -> ()
    | Some x -> x##.textContent := Js.some @@ Js.string label
end

let attach (elt : #Dom_html.element Js.t) =
  new t (elt :> Dom_html.element Js.t)

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    let open Lwt_react in
    Pc_control_http_js.Updates.get_state ()
    >>=? fun _state ->
    Api_js.Websocket.JSON.open_socket ~path:(Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Pc_control_http_js.Updates.Event.get_state socket
    >>=? fun (_, state_ev) ->
    let section =
      attach
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ Markup.make_update_section () in
    let notify =
      E.merge (fun _ _ -> ()) ()
        [ E.map_s (fun x -> section#notify (`State x)) state_ev
        ] in
    let page =
      Widget.create
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ Markup.make [section#markup] in
    page#set_on_destroy (fun () ->
        section#destroy ();
        E.stop ~strong:true notify;
        E.stop ~strong:true state_ev;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok page in
  let (_ : Dom_html.element Js.t) =
    Components_lab.Loader.make_widget_loader
      ~elt:scaffold#app_content_inner
      thread
  in
  ()
