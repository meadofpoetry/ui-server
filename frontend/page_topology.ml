open Js_of_ocaml
open Containers
open Application_js
open Components

class ['a] t ~init () = object(self)
  val mutable _sock : WebSockets.webSocket Js.t option = None
  val mutable _nodes : [ `CPU of Topo_cpu.t
                       | `Board of Topo_board.t
                       | `Input of Topo_input.t ] list = []
  val mutable _resize_observer = None

  inherit ['a] Ui_templates.Loader.widget_loader init () as super

  method! init () : unit =
    super#init ();
    super#add_class Page_topology._class;
    let obs =
      Ui_templates.Resize_observer.observe ~node:self#root
        ~f:(fun _ -> self#layout ()) () in
    _resize_observer <- Some obs;
    let open Lwt_result.Infix in
    Requests.HTTP.get_topology ()
    >>= (fun init ->
      let event, sock = Requests.WS.get_topology () in
      let nodes = Page_topology.create ~parent:self ~init ~event () in
      _nodes <- nodes;
      _sock <- Some sock;
      self#layout ();
      Lwt_result.return ())
    |> Lwt.ignore_result

  method! destroy () : unit =
    super#destroy ();
    Option.iter (fun x -> x##close) _sock;
    _sock <- None;
    Option.iter Ui_templates.Resize_observer.disconnect _resize_observer;
    _resize_observer <- None

  (* FIXME hack, need to handle resize of total element *)
  method! layout () : unit =
    super#layout ();
    List.iter (function
        | `Board b -> b#layout ()
        | `Input i -> i#layout ()
        | `CPU c -> c#layout ()) _nodes

end

let () =
  let init =
    Requests.HTTP.get_topology ()
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let elt = new t ~init () in
  ignore @@ new Ui_templates.Page.t (`Static [elt#widget]) ()
