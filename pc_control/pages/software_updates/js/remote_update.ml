open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pc_control_http_js.Updates
open Pc_control_types.Software_updates
include Page_software_updates_tyxml.Remote_update
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event = [ `State of state ]

let ( >>= ) = Lwt.bind

module Selector = struct
  let action = Printf.sprintf ".%s" Button.CSS.root

  let progress = Printf.sprintf ".%s" Linear_progress.CSS.root

  let placeholder = Printf.sprintf ".%s" Components_lab.Placeholder.CSS.root

  let action_label = Printf.sprintf ".%s" Button.CSS.label

  let path = "path"
end

let make_warning_dialog () =
  let open Dialog.D in
  let title = dialog_title ~title:"Внимание!" () in
  let content =
    dialog_content
      ~children:
        [
          Js_of_ocaml_tyxml.Tyxml_js.Html.txt
            "После завершения обновления прибор \
             будет автоматически перезагружен.\n\
             Во время перезагрузки доступ к \
             прибору будет недоступен.\n";
        ]
      ()
  in
  let actions =
    [
      dialog_action ~action:Close ~label:"Отмена" ();
      dialog_action ~action:Accept
        ~label:"Продолжить обновление" ();
    ]
  in
  Dialog.make ~title ~content ~actions ()

class t (elt : Dom_html.element Js.t) =
  object (self)
    inherit Widget.t elt () as super

    val placeholder =
      Components_lab.Placeholder.attach
      @@ Element.query_selector_exn elt Selector.placeholder

    val action = Button.attach @@ Element.query_selector_exn elt Selector.action

    val progress =
      Linear_progress.attach @@ Element.query_selector_exn elt Selector.progress

    val warning_dialog : Dialog.t = make_warning_dialog ()

    val mutable listeners = []

    val mutable _state = default_state

    method! init () : unit =
      warning_dialog#append_to_body ();
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ clicks action#root self#handle_action ] @ listeners)

    method! destroy () : unit =
      warning_dialog#remove_from_dom ();
      warning_dialog#destroy ();
      action#destroy ();
      progress#destroy ();
      placeholder#destroy ();
      super#destroy ()

    method notify : event -> unit Lwt.t =
      function `State state -> self#handle_state_change state

    method private handle_state_change (state : state) : unit Lwt.t =
      let loading = is_loading state in
      let hint = state_to_hint ~auto_reboot state in
      let icon = state_to_svg_path state in
      let action_label = state_to_action_label ~auto_reboot state in
      let action_disabled = state_to_action_disabled state in
      self#set_placeholder_icon icon;
      self#set_action_label action_label;
      action#set_disabled action_disabled;
      if not @@ equal_state _state state then placeholder#set_text ~loading hint;
      _state <- state;
      (* Update progress bar *)
      match state with
      | `Checking (_, pct) | `Upgrading (_, pct) ->
          let v = if pct > 100l then 0l else pct in
          let v = Int32.to_float v /. 100. in
          progress#set_progress v;
          progress#set_buffer v;
          progress#open_ ()
      | _ -> self#close_progress ()

    method private handle_action _ _ : unit Lwt.t =
      let auto_reboot = auto_reboot in
      let rec aux () =
        action#set_disabled true;
        let thread =
          match _state with
          | `Updates_avail -> (
              (warning_dialog#open_await () >>= function
               | Close | Destroy | Custom _ -> Lwt.return_ok ()
               | Accept -> upgrade ~reboot:auto_reboot ())
              >>= function
              | Ok _ -> Lwt.return_ok ()
              | Error (`Msg err) ->
                  let msg =
                    Printf.sprintf
                      "Не удалось выполнить \
                       обновление. \n\
                       %s"
                      err
                  in
                  Lwt.return_error msg )
          | `Unchecked | `Updates_not_avail -> (
              check_updates () >>= function
              | Ok _ -> Lwt.return_ok ()
              | Error (`Msg err) ->
                  let msg =
                    Printf.sprintf
                      "Не удалось проверить наличие \
                       обновлений. \n\
                       %s"
                      err
                  in
                  Lwt.return_error msg )
          | `Need_reboot -> (
              if auto_reboot then (
                Dom_html.window##.location##reload;
                Lwt.return_ok () )
              else
                Pc_control_http_js.Power.reboot () >>= function
                | Ok _ as ok -> Lwt.return ok
                | Error (`Msg err) ->
                    let msg =
                      Printf.sprintf
                        "Не удалось перезагрузить \
                         прибор. \n\
                         %s"
                        err
                    in
                    Lwt.return_error msg )
          | `Checking _ | `Upgrading _ -> Lwt.return_ok ()
        in
        Lwt.on_termination thread (fun () -> action#set_disabled false);
        thread >>= function
        | Ok () -> Lwt.return_unit
        | Error err ->
            let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
            let snackbar =
              Snackbar.make ~dismiss:`True ~stacked:true
                ~action:(`Text "Повторить") ~label:(`Text err) ()
            in
            scaffold#show_snackbar
              ~on_close:(fun x ->
                snackbar#destroy ();
                match x with Action -> Lwt.async aux | _ -> ())
              snackbar
      in
      aux ()

    method private close_progress () =
      progress#close () >>= fun () ->
      progress#set_progress 0.;
      progress#set_buffer 0.;
      Lwt.return_unit

    method private set_placeholder_icon path =
      match placeholder#icon with
      | None -> ()
      | Some x -> (
          match Element.query_selector x Selector.path with
          | None -> ()
          | Some x -> Element.set_attribute x "d" path )

    method private set_action_label label =
      match Element.query_selector action#root Selector.action_label with
      | None -> ()
      | Some x -> x##.textContent := Js.some @@ Js.string label
  end

let make ?classes ?a () =
  let elt = Tyxml_js.To_dom.of_element @@ D.create ?classes ?a () in
  new t elt

let attach (elt : #Dom_html.element Js.t) = new t (elt :> Dom_html.element Js.t)
