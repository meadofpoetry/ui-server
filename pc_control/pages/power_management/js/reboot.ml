open Js_of_ocaml
open Components

let ( >>= ) = Lwt.bind

module Selector = struct
  let action = Printf.sprintf ".%s" Button.CSS.root
end

let make_warning_dialog () =
  let title =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_title_simple
         ~title:"Перезагрузить прибор?"
         ()
  in
  let actions =
    List.map
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      [ Dialog.Markup.create_action ~action:Close ~label:"Отмена" ()
      ; Dialog.Markup.create_action ~action:Accept ~label:"Перезагрузить" ()
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
      listeners <- Js_of_ocaml_lwt.Lwt_js_events.[clicks action#root self#handle_action];
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
        warning_dialog#open_await ()
        >>= function
        | Close | Destroy | Custom _ -> Lwt.return_unit
        | Accept -> (
            Pc_control_http_js.Power.reboot ()
            >>= function
            | Ok () -> Lwt.return_unit
            | Error err ->
                let msg =
                  Printf.sprintf
                    "Не удалось перезагрузить прибор. %s"
                  @@ Api_js.Http.error_to_string err
                in
                let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
                let snackbar =
                  Snackbar.make
                    ~label:msg
                    ~dismiss:True
                    ~stacked:true
                    ~action:(Label "Повторить")
                    ()
                in
                scaffold#show_snackbar
                  ~on_close:(fun x ->
                    snackbar#destroy ();
                    match x with
                    | Action -> Lwt.async aux
                    | _ -> ())
                  snackbar)
      in
      aux ()
  end

let make ?classes ?attrs () =
  let elt =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element @@ Markup.make ?classes ?attrs ()
  in
  new t elt

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t)
