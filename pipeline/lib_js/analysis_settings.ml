open Containers
open Components
open Requests
open Lwt_result.Infix

class t () = object(self)
  val mutable sock : WebSockets.webSocket Js.t option = None
  inherit Widget.widget (Dom_html.createDiv Dom_html.document) () as super

  method on_load =
    Requests.get_settings ()
    >>= (fun settings ->
      let e_settings,settings_sock = Requests.get_settings_socket () in
      let open Lwt.Infix in
      let el = Ui.Settings.create ~init:settings ~events:e_settings
                                  ~post:(fun s ->
                                    Requests.post_settings s
                                    >|= (function
                                         | Ok () -> ()
                                         | Error _ -> print_endline @@ "error post settings")
                                    |> Lwt.ignore_result)
      in
      sock <- Some settings_sock;
      Dom.appendChild self#root el;
      Lwt_result.return ())
    |> ignore

  method on_unload =
    Option.iter (fun x -> x##close; sock <- None) sock

end

let page () = new t ()
