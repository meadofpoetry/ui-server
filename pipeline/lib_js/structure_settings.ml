open Containers
open Components
open Requests
open Lwt_result.Infix

class t () =
  object(self)
    val mutable sock : WebSockets.webSocket Js.t option = None
    inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()

    method on_load =
      Requests.get_structure ()
      >>= (fun structure ->
        let e_structure,structure_sock = Requests.get_structure_socket () in
        let open Lwt.Infix in
        let el = Ui.Structure.create ~init:structure ~events:e_structure
                                     ~post:(fun s ->
                                       Requests.post_structure s
                                       >|= (function
                                            | Ok () -> ()
                                            | Error _ -> print_endline @@ "error post settings")
                                       |> Lwt.ignore_result)
        in
        sock <- Some structure_sock;
        Dom.appendChild self#root el;
        Lwt_result.return ())
      |> ignore

    method on_unload =
      Option.iter (fun x -> x##close; sock <- None) sock
  end

let page () = new t ()
