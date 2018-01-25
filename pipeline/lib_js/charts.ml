open Components
open Requests
open Lwt_result.Infix

class t () =

  let div  = Dom_html.createDiv Dom_html.document in
  let cell = new Layout_grid.Cell.t ~widgets:[Widget.create div] () in

  object(self)

    val mutable in_dom = false
    val mutable observer = None
    val mutable sock : (WebSockets.webSocket Js.t * WebSockets.webSocket Js.t) option = None

    inherit Layout_grid.t ~cells:[cell] ()

    method private observe =
      MutationObserver.observe
        ~node:Dom_html.document
        ~f:(fun _ _ ->
          let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
          if in_dom && (not in_dom_new)
          then CCOpt.iter (fun (x,y) -> x##close;y##close; sock <- None) sock
          else if (not in_dom) && in_dom_new
          then (Requests.get_structure ()
                >>= (fun structure ->
                  let e_vdata,vdata_sock = Requests.get_vdata_socket () in
                  let e_structure,structure_sock = Requests.get_structure_socket () in
                  let open Lwt.Infix in
                  let el = Ui.Plots.create ~init:structure ~events:e_structure ~data:e_vdata in
                  sock <- Some (vdata_sock, structure_sock);
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
