open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pipeline_types

module Selector = struct
  let table_ghost = Printf.sprintf ".%s" Markup.CSS.grid_ghost (* FIXME *)
end

module Grid = struct

  type event = Touch of Dom_html.touchEvent Js.t
             | Mouse of Dom_html.mouseEvent Js.t

  (* TODO implement *)
  class t (elt : Dom_html.element Js.t) = object(self)
    inherit Widget.t elt () as super

    method! destroy () : unit =
      super#destroy ()
  end

end

class t ?(containers = []) ~resolution elt () = object(self)
  val ghost = match Element.query_selector elt Selector.table_ghost with
    | None -> failwith "container-editor: table ghost element not found"
    | Some x -> x

  val mutable _containers : (string * Wm.container) list = containers
  val mutable _listeners = []
  val mutable _resize_observer = None
  val mutable _focused_item = None

  inherit Drop_target.t elt () as super

  method! init () : unit =
    super#init ()

  method! initial_sync_with_dom () : unit =
    _resize_observer <- Some (
        Ui_templates.Resize_observer.observe
          ~f:(fun _ -> self#layout ())
          ~node:super#root
          ());
    _listeners <- Events.(
        [ keydowns super#root self#handle_keydown
        ]);
    super#initial_sync_with_dom ()

  method! destroy () : unit =
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  (* Private methods *)

  method private handle_keydown _ _ : unit Lwt.t =
    (* TODO implement *)
    Lwt.return_unit

  method private handle_dropped_json (json : Yojson.Safe.json) : unit Lwt.t =
    Lwt.return_unit

  method private move_ghost ?aspect event : unit =
    ()
end

let make (wm : Wm.t) =
  let content = Markup.create_grid_ghost () :: [] in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Tyxml_js.Html.(div content) in
  new t
    ~resolution:wm.resolution
    ~containers:wm.layout
    elt ()
