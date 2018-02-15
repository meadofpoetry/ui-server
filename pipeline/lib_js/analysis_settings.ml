open Containers
open Components
open Requests
open Lwt_result.Infix

class t () =

  let div  = Dom_html.createDiv Dom_html.document in
  let cell = new Layout_grid.Cell.t ~widgets:[Widget.create div] () in

  object(self)

    val mutable in_dom = false
    val mutable observer = None
    val mutable sock : WebSockets.webSocket Js.t option = None

    inherit Layout_grid.t ~cells:[cell] ()

    method private observe =
      MutationObserver.observe
        ~node:Dom_html.document
        ~f:(fun _ _ ->
          let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
          if in_dom && (not in_dom_new)
          then Option.iter (fun x -> x##close; sock <- None) sock
          else if (not in_dom) && in_dom_new
          then (Requests.get_settings ()
                >>= (fun settings ->
                  let e_settings,settings_sock = Requests.get_settings_socket () in
                  let open Lwt.Infix in
                  let el = Ui.Settings.create ~init:settings ~events:e_settings
                             ~post:(fun s ->
                               Requests.post_settings s
                               >|= (function
                                    | Ok () -> ()
                                    | Error e -> print_endline @@ "error post settings" ^ e)
                               |> Lwt.ignore_result)
                  in
                  sock <- Some settings_sock;
                  Dom.appendChild self#root el;
                  Lwt_result.return ())
                |> ignore);
          in_dom <- in_dom_new)
        ~child_list:true
        ~subtree:true
        ()
      |> (fun o -> observer <- Some o)

    initializer
      self#observe

  end

let page () = new t ()
