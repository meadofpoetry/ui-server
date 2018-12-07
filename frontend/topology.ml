open Js_of_ocaml
open Containers
open Lwt_result.Infix
open Application_js
open Components

class t () = object(self)
  val mutable _sock : WebSockets.webSocket Js.t option = None
  val mutable _nodes : [ `CPU of Topo_cpu.t
                       | `Board of Topo_board.t
                       | `Input of Topo_input.t ] list = []
  inherit Widget.t Dom_html.(createDiv document) () as super

  method init () : unit =
    super#init ();
    self#add_class Page_topology._class;
    Dom_events.listen
      Dom_html.window
      Dom_events.Typ.resize (fun _ _ ->
        self#layout (); true) |> ignore;
    super#set_on_unload
    @@ Some (fun () ->
           Option.iter (fun x -> x##close; _sock <- None) _sock);
    super#set_on_load
    @@ Some (fun () -> self#on_load);

  (* FIXME hack, need to handle resize of total element *)
  method layout () : unit =
    super#layout ();
    List.iter (function
        | `Board b -> b#layout ()
        | `Input i -> i#layout ()
        | `CPU c -> c#layout ()) _nodes

  method private on_load =
    let open Lwt_result.Infix in
    Requests.HTTP.get_topology ()
    >>= (fun init ->
      let event,sock = Requests.WS.get_topology () in
      let nodes = Page_topology.create ~parent:self ~init ~event () in
      _nodes <- nodes;
      _sock <- Some sock;
      self#layout ();
      Lwt_result.return ())
    |> ignore

end

let () =
  let elt = new t () in
  ignore @@ new Ui_templates.Page.t (`Static [elt#widget]) ()
