open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pipeline_types

(* TODO
   [ ] undo/redo
   [ ] selection
   [ ] editing mode: table, containers
*)

let ( >>= ) = Lwt.bind

module Selector = struct
  let table_ghost = Printf.sprintf ".%s" Markup.CSS.grid_ghost (* FIXME *)
  let table_overlay = Printf.sprintf ".%s" Markup.CSS.grid_overlay (* FIXME *)
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
  val grid =
    let make_cell ?id ~col ~row ?classes ?content () =
      Resizable_grid.Markup.create_grid_cell
        ?attrs:(match id with None -> None
                            | Some id -> Some [Tyxml_js.Html.a_id id])
        ?content ?classes ~row ~col () in
    let nested_grid = Tyxml_js.Html.(
        Resizable_grid.Markup.create_grid
          ~rows:2 ~cols:2
          ~content:[ make_cell ~col:1 ~row:1 ()
                   ; make_cell ~col:1 ~row:2 ()
                   ; make_cell ~col:2 ~row:1 ()
                   ; make_cell ~col:2 ~row:2 ()
                   ]
          ()) in
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Tyxml_js.Html.(
          Resizable_grid.Markup.create_grid
            ~rows:2 ~cols:2
            ~content:[ make_cell ~id:"cell" ~col:1 ~row:1 ~content:[nested_grid] ()
                     ; make_cell ~col:1 ~row:2 ()
                     ; make_cell ~col:2 ~row:1 ()
                     ; make_cell ~col:2 ~row:2 ()
                     ]
            ()) in
    Resizable_grid.attach ~drag_interval:(`Fr 0.1) elt

  val mutable _containers : (string * Wm.container) list = containers
  val mutable _listeners = []
  val mutable _focused_item = None
  val mutable _toolbar = Widget.create_div ()

  inherit Drop_target.t elt () as super

  method! init () : unit =
    Element.append_child super#root grid#root;
    grid#reset ~rows:5 ~cols:5 ();
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ keydowns super#root self#handle_keydown
        ; listen_lwt grid#root Resizable_grid.Event.selected self#handle_selected
        ]);
    super#initial_sync_with_dom ()

  method! layout () : unit =
    let scale_factor = self#scale_factor in
    let width' = int_of_float @@ float_of_int (fst resolution) *. scale_factor in
    let height' = int_of_float @@ float_of_int (snd resolution) *. scale_factor in
    super#root##.style##.width := Utils.px_js width';
    super#root##.style##.height := Utils.px_js height';
    super#layout ()

  method! destroy () : unit =
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method toolbar = _toolbar

  (* Private methods *)

  method private handle_selected e _ : unit Lwt.t =
    Js.Unsafe.global##.console##log e##.detail |> ignore;
    Lwt.return_unit

  method private handle_keydown _ _ : unit Lwt.t =
    (* TODO implement *)
    Lwt.return_unit

  method private parent_rect : float * float * float =
    Js.Opt.case (Element.get_parent super#root)
      (fun () -> 0., 0., 1.)
      (fun x ->
         let width = float_of_int x##.offsetWidth in
         let height = float_of_int x##.offsetHeight in
         width, height, width /. height)

  method private scale_factor : float =
    let cur_width, cur_height, cur_aspect = self#parent_rect in
    if cur_aspect > (float_of_int (fst resolution) /. float_of_int (snd resolution))
    then cur_height /. float_of_int (snd resolution)
    else cur_width /. float_of_int (fst resolution)

  method private handle_dropped_json (json : Yojson.Safe.json) : unit Lwt.t =
    Lwt.return_unit

  method private move_ghost ?aspect event : unit =
    ()
end

let make (wm : Wm.t) =
  let content = Markup.create_grid_ghost () :: [] in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Resizable_grid.Markup.create ~content () in
  new t
    ~resolution:wm.resolution
    ~containers:wm.layout
    elt ()
