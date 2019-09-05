open Js_of_ocaml
open Components

let ( >>= ) = Lwt.bind

let on_loaded (scaffold : Scaffold.t) () =
  let reboot = Reboot.attach (Dom_html.getElementById "reboot") in
  let shutdown = Shutdown.attach (Dom_html.getElementById "shutdown") in
  scaffold#set_on_destroy (fun () ->
      reboot#destroy ();
      shutdown#destroy ());
  Lwt.return_unit

let () = Lwt.async (fun () ->
    let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
    scaffold#loaded >>= on_loaded scaffold)
