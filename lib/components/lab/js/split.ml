open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Components_lab_tyxml.Split
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

(* TODO
   - test touch support *)

type event =
  | Mouse of Dom_html.mouseEvent Js.t
  | Touch of Dom_html.touchEvent Js.t

module Selector = struct
  let splitter = Printf.sprintf ".%s" CSS.splitter
end

let sum_scroll_offsets (e : Dom_html.element Js.t) =
  let rec aux cur acc_left acc_top =
    match Js.Opt.to_option cur with
    | None -> (acc_left, acc_top)
    | Some cur -> (
        match Js.to_string cur##.nodeName##toLowerCase with
        | "body" -> (acc_left, acc_top)
        | _ ->
            aux cur##.parentNode
              (acc_left + (Js.Unsafe.coerce cur)##.scrollLeft)
              (acc_top + (Js.Unsafe.coerce cur)##.scrollTop) )
  in
  aux e##.parentNode 0 0

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val splitter : Dom_html.element Js.t =
      Element.query_selector_exn elt Selector.splitter

    val panels : Dom_html.element Js.t * Dom_html.element Js.t =
      match Element.query_selector_all elt CSS.panel with
      | [ x; y ] -> (x, y)
      | _ -> failwith "split: two panels must be provided"

    val mutable _mousedown : unit Lwt.t option = None

    val mutable _mouseup : unit Lwt.t option = None

    val mutable _mousemove : unit Lwt.t option = None

    val mutable _mouseout : unit Lwt.t option = None

    val mutable _touchstart : unit Lwt.t option = None

    val mutable _touchmove : unit Lwt.t option = None

    val mutable _touchend : unit Lwt.t option = None

    val mutable _touchcancel : unit Lwt.t option = None

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      (* Attach event listeners. *)
      let mousedown =
        Js_of_ocaml_lwt.Lwt_js_events.mousedowns super#root
          self#handle_mousedown
      in
      _mousedown <- Some mousedown;
      let touchstart =
        Js_of_ocaml_lwt.Lwt_js_events.touchstarts super#root
          self#handle_touchstart
      in
      _touchstart <- Some touchstart

    method! destroy () : unit =
      super#destroy ();
      (* Detach event listeners. *)
      Option.iter Lwt.cancel _mousedown;
      _mousedown <- None;
      self#stop_drag ()

    method vertical : bool = super#has_class CSS.vertical

    method set_vertical (x : bool) : unit =
      super#toggle_class ~force:x CSS.vertical

    (* Private methods *)
    method private handle_touchstart (_ : Dom_html.touchEvent Js.t) _
        : unit Lwt.t =
      let touchmove =
        Js_of_ocaml_lwt.Lwt_js_events.touchmoves super#root (fun e _ ->
            let percent = self#calc_percent (Touch e) in
            self#apply_panel_styles percent;
            Lwt.return_unit)
      in
      let touchend =
        Js_of_ocaml_lwt.Lwt_js_events.touchends super#root (fun _ _ ->
            self#stop_drag ();
            Lwt.return_unit)
      in
      let touchcancel =
        Js_of_ocaml_lwt.Lwt_js_events.touchcancels super#root (fun _ _ ->
            self#stop_drag ();
            Lwt.return_unit)
      in
      _touchmove <- Some touchmove;
      _touchend <- Some touchend;
      _touchcancel <- Some touchcancel;
      Lwt.return_unit

    method private handle_mousedown (e : Dom_html.mouseEvent Js.t) _
        : unit Lwt.t =
      match e##.button with
      | 0 ->
          let mouseup =
            Js_of_ocaml_lwt.Lwt_js_events.mouseups Dom_html.document##.body
              (fun _ _ ->
                self#stop_drag ();
                Lwt.return_unit)
          in
          let mousemove =
            Js_of_ocaml_lwt.Lwt_js_events.mousemoves super#root (fun e _ ->
                let percent = self#calc_percent (Mouse e) in
                self#apply_panel_styles percent;
                Lwt.return_unit)
          in
          let mouseout =
            Js_of_ocaml_lwt.Lwt_js_events.mouseouts super#root (fun e _ ->
                let related_target =
                  match Js.Optdef.to_option e##.relatedTarget with
                  | None -> None
                  | Some x -> Js.Opt.to_option x
                in
                ( match related_target with
                | None -> ()
                | Some target ->
                    if Element.equal Dom_html.document##.documentElement target
                    then self#stop_drag () );
                Lwt.return_unit)
          in
          _mouseup <- Some mouseup;
          _mousemove <- Some mousemove;
          _mouseout <- Some mouseout;
          Lwt.return_unit
      | _ -> Lwt.return_unit

    method private apply_panel_styles (percent : float) : unit =
      let panel1, panel2 = panels in
      (Js.Unsafe.coerce panel1##.style)##.flexGrow := percent;
      (Js.Unsafe.coerce panel2##.style)##.flexGrow := 100. -. percent

    method private stop_drag () =
      Option.iter Lwt.cancel _mouseup;
      _mouseup <- None;
      Option.iter Lwt.cancel _mousemove;
      _mousemove <- None;
      Option.iter Lwt.cancel _mouseout;
      _mouseout <- None;
      Option.iter Lwt.cancel _touchmove;
      _touchmove <- None;
      Option.iter Lwt.cancel _touchend;
      _touchend <- None;
      Option.iter Lwt.cancel _touchcancel;
      _touchcancel <- None

    method private get_cursor_position (event : event) : int * int =
      match event with
      | Mouse e -> (e##.clientX, e##.clientY)
      | Touch e -> (
          let touch = Js.Optdef.to_option (e##.touches##item 0) in
          match touch with
          | None -> (0, 0)
          | Some (touch : Dom_html.touch Js.t) ->
              (touch##.clientX, touch##.clientY) )

    method private calc_percent (event : event) : float =
      let rect = super#root##getBoundingClientRect in
      let client_x, client_y = self#get_cursor_position event in
      if self#vertical then
        let height = super#root##.clientHeight in
        let offsets = sum_scroll_offsets super#root in
        let rel_y = client_y - int_of_float rect##.top + snd offsets in
        100. *. (float_of_int rel_y /. float_of_int height)
      else
        let width = super#root##.clientWidth in
        let offsets = sum_scroll_offsets super#root in
        let rel_x = client_x - int_of_float rect##.left + fst offsets in
        100. *. (float_of_int rel_x /. float_of_int width)
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?vertical ?splitter ~panel_a ~panel_b () =
  D.split ?classes ?a ?vertical ?splitter ~panel_a ~panel_b ()
  |> Tyxml_js.To_dom.of_div
  |> attach
