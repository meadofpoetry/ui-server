open Js_of_ocaml
open Components
open Pc_control_types.Software_updates

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let unchecked_hint =
  "Проверьте наличие обновлений"

let not_available_hint =
  "Нет доступных обновлений"

let available_hint = ""

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

let state_to_string = function
  | `Unchecked -> "unchecked"
  | `Checking _ -> "checking"
  | `Updates_avail -> "updates available"
  | `Upgrading _ -> "upgrading"
  | `Need_reboot -> "need reboot"

module Selector = struct
  let button = Printf.sprintf ".%s" Button.CSS.root

  let progress = Printf.sprintf ".%s" Linear_progress.CSS.root
end

class t (elt : Dom_html.element Js.t) = object
  inherit Widget.t elt () as super

  val button =
    Button.attach
    @@ Element.query_selector_exn elt Selector.button

  val progress =
    Linear_progress.attach
    @@ Element.query_selector_exn elt Selector.progress

  method! destroy () : unit =
    button#destroy ();
    progress#destroy ();
    super#destroy ()
end

let attach (elt : #Dom_html.element Js.t) =
  new t (elt :> Dom_html.element Js.t)

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Pc_control_http_js.Updates.get_state ()
    >>=? fun state ->
    print_endline @@ state_to_string state;
    let section =
      attach
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ Markup.make_update_section () in
    let page =
      Widget.create
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ Markup.make [section#markup] in
    Lwt.return_ok page in
  let (_ : Dom_html.element Js.t) =
    Ui_templates.Loader.make_widget_loader
      ~elt:scaffold#app_content_inner
      thread
  in
  ()
