open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
module D =
  Page_power_management_tyxml.Reboot.Make (Tyxml_js.Xml) (Tyxml_js.Svg)
    (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

module Selector = struct
  let action = Printf.sprintf ".%s" Button.CSS.root
end

let make_warning_dialog () =
  let open Dialog.D in
  let title = dialog_title ~title:"Выключить прибор?" () in
  let actions =
    [
      dialog_action ~action:Close ~label:"Отмена" ();
      dialog_action ~action:Accept ~label:"Выключить" ();
    ]
  in
  Dialog.make ~title ~actions ()

class t (elt : Dom_html.element Js.t) =
  object (self)
    val action = Button.attach @@ Element.query_selector_exn elt Selector.action

    val warning_dialog : Dialog.t = make_warning_dialog ()

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      warning_dialog#append_to_body ();
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.[ clicks action#root self#handle_action ];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel listeners;
      listeners <- [];
      warning_dialog#remove_from_dom ();
      warning_dialog#destroy ();
      action#destroy ();
      super#destroy ()

    method private handle_action _ _ : unit Lwt.t =
      let rec aux () =
        warning_dialog#open_await () >>= function
        | Close | Destroy | Custom _ -> Lwt.return_unit
        | Accept -> (
            Pc_control_http_js.Power.off () >>= function
            | Ok () -> Lwt.return_unit
            | Error (`Msg err) ->
                let msg =
                  Printf.sprintf
                    "Не удалось выключить прибор. %s"
                    err
                in
                let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
                let snackbar =
                  Snackbar.make ~label:(`Text msg) ~dismiss:`True ~stacked:true
                    ~action:(`Text "Повторить") ()
                in
                scaffold#show_snackbar
                  ~on_close:(fun x ->
                    snackbar#destroy ();
                    match x with Action -> Lwt.async aux | _ -> ())
                  snackbar )
      in
      aux ()
  end

let attach (elt : #Dom_html.element Js.t) : t =
  new t (elt :> Dom_html.element Js.t)

let make ?classes ?a () =
  D.create ?classes ?a () |> Tyxml_js.To_dom.of_element |> attach
