open Containers
open Lwt_result.Infix
open Application_js
open Common.Topology
open Components

class t () = object(self)
  val mutable _sock  : WebSockets.webSocket Js.t option = None
  val mutable _nodes : [ `CPU of Topo_cpu.t | `Board of Topo_board.t | `Input of Topo_input.t ] list = []
  inherit Widget.widget (Dom_html.createDiv Dom_html.document) () as super

  (* FIXME hack, need to handle resize of total element *)
  method layout =
    Dom_html.setTimeout (fun () -> List.iter (function
                                              | `Board b -> b#layout
                                              | `Input i -> i#layout
                                              | `CPU c   -> c#layout) _nodes)
                        10. |> ignore

  method private on_load =
    let open Lwt_result.Infix in
    Requests.get_topology ()
    >>= (fun init ->
      let event,sock = Requests.get_topology_socket () in
      let nodes      = Topology.create ~parent:self ~init ~event () in
      _nodes <- nodes;
      _sock <- Some sock;
      self#layout;
      Lwt_result.return ())
    |> ignore

  initializer
    self#add_class Topology._class;
    Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ -> self#layout; true) |> ignore;
    super#set_on_unload @@ Some (fun () -> Option.iter (fun x -> x##close; _sock <- None) _sock);
    super#set_on_load   @@ Some (fun () -> self#on_load);
end

let () =
  let elt = new t () in
  let _ = new Page.t (`Static [elt#widget]) () in
  ()
