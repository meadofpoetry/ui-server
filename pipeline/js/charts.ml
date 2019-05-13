open Js_of_ocaml
open Containers
open Components

class t () =
object
  val mutable sock : (WebSockets.webSocket Js.t * WebSockets.webSocket Js.t) option = None
  inherit Widget.t (Dom_html.createDiv Dom_html.document) ()

  method on_unload =
    Option.iter (fun (x,y) -> x##close;y##close; sock <- None) sock
  method on_load = ()
    (* Requests.get_structure ()
     * >>= (fun structure ->
     *   let e_vdata,vdata_sock = Requests.get_vdata_socket () in
     *   let e_structure,structure_sock = Requests.get_structure_socket () in
     *   let open Lwt.Infix in
     *   let el = Ui.Plots.create ~init:structure ~events:e_structure ~data:e_vdata in
     *   sock <- Some (vdata_sock, structure_sock);
     *   Dom.appendChild self#root el;
     *   Lwt_result.return ())
     * |> ignore *)

end

let page () = new t ()
