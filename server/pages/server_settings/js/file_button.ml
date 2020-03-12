open Js_of_ocaml
open Components

module Selector = struct
  let button = Printf.sprintf ".%s" Button.CSS.root

  let file_input = Printf.sprintf "input[type=\"file\"]"
end

class t (elt : Dom_html.element Js.t) =
  object
    val loader : Circular_progress.t =
      Circular_progress.make ~size:25 ~indeterminate:false ()

    val button : Button.t =
      match Element.query_selector elt Selector.button with
      | None -> failwith "file-button: no `button` element found"
      | Some x -> Button.attach x

    val file_input : Dom_html.inputElement Js.t =
      match Element.query_selector elt Selector.file_input with
      | None -> failwith "file-button: no `input` element found"
      | Some x -> Js.Unsafe.coerce x

    val mutable listeners_ = []

    inherit Widget.t elt () as super

    method! init () : unit =
      button#set_loader loader#root;
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners_ <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [
            clicks super#root (fun _ _ ->
                file_input##click;
                Lwt.return_unit);
          ];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      loader#destroy ();
      button#destroy ();
      List.iter Lwt.cancel listeners_;
      listeners_ <- [];
      super#destroy ()

    method loader : Circular_progress.t = loader

    method file_input : Dom_html.inputElement Js.t = file_input

    method button : Button.t = button
  end
