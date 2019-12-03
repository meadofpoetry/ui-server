open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Tab
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

type dimensions =
  { root_left : int
  ; root_right : int
  ; content_left : int
  ; content_right : int }

module Event = struct
  let interact : Dom_html.element Js.t Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (CSS.root ^ ":interact")
end

module Lwt_js_events = struct
  open Js_of_ocaml_lwt.Lwt_js_events

  let interact ?use_capture ?passive x =
    make_event ?use_capture ?passive Event.interact x

  let interacts ?cancel_handler ?use_capture ?passive x =
    seq_loop ?cancel_handler ?use_capture ?passive interact x
end

module Selector = struct
  let ripple = Printf.sprintf ".%s" CSS.ripple

  let content = Printf.sprintf ".%s" CSS.content

  let tab_indicator = Printf.sprintf ".%s" Tab_indicator.CSS.root
end

class t (elt : Dom_html.buttonElement Js.t) () =
  object (self)
    val ripple_elt : Dom_html.element Js.t =
      Element.query_selector_exn elt Selector.ripple

    val content_elt : Dom_html.element Js.t =
      Element.query_selector_exn elt Selector.content

    val indicator : Tab_indicator.t =
      Tab_indicator.attach @@ Element.query_selector_exn elt Selector.tab_indicator

    inherit Widget.t elt () as super

    val mutable ripple_ : Ripple.t option = None

    val mutable listeners = []

    method! init () : unit =
      ripple_ <- Some (self#create_ripple ());
      super#init ()

    method! initial_sync_with_dom () : unit =
      (* Attach event handlers *)
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ clicks super#root (fun _ _ ->
                super#emit ~should_bubble:true ~detail:super#root Event.interact;
                Lwt.return_unit) ]
          @ listeners);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      Option.iter Ripple.layout ripple_;
      super#layout ()

    method! destroy () : unit =
      (* Destroy internal components *)
      Option.iter Ripple.destroy ripple_;
      ripple_ <- None;
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

    method indicator : Tab_indicator.t = indicator

    method disabled : bool = Js.to_bool elt##.disabled

    method set_disabled (x : bool) : unit = elt##.disabled := Js.bool x

    method active : bool = super#has_class CSS.active

    method set_active ?(previous : t option) (x : bool) : unit =
      super#toggle_class ~force:x CSS.active;
      super#set_attribute "aria-selected" @@ string_of_bool x;
      super#set_attribute "tabindex" (if x then "0" else "-1");
      let previous = Option.map (fun x -> x#indicator) previous in
      indicator#set_active ?previous x;
      if x then super#root##focus

    method compute_dimensions () : dimensions =
      let root_width = super#root##.offsetWidth in
      let root_left = super#root##.offsetWidth in
      let content_width = content_elt##.offsetWidth in
      let content_left = content_elt##.offsetLeft in
      { root_left
      ; root_right = root_left + root_width
      ; content_left = root_left + content_left
      ; content_right = root_left + content_left + content_width }

    method index : int =
      let rec aux i node =
        match Js.Opt.to_option node##.previousSibling with
        | None -> i
        | Some x -> aux (succ i) x
      in
      aux 0 super#node

    method width : int = super#root##.offsetWidth

    method left : int = super#root##.offsetLeft

    method ripple : Ripple.t option = ripple_

    method private create_ripple () : Ripple.t =
      let adapter = Ripple.make_default_adapter super#root in
      let adapter = {adapter with style_target = ripple_elt} in
      new Ripple.t adapter ()
  end

let attach (elt : #Dom_html.element Js.t) : t =
  Js.Opt.case
    (Dom_html.CoerceTo.button elt)
    (fun () -> failwith (CSS.root ^ ": root element must have a `button` tag"))
    (fun btn -> new t btn ())

let make
    ?classes
    ?a
    ?active
    ?stacked
    ?disabled
    ?min_width
    ?indicator_span_content
    ?indicator_icon
    ?icon
    ?text_label
    ?ripple
    ?indicator
    ?content
    ?children
    () =
  D.tab
    ?classes
    ?a
    ?active
    ?stacked
    ?disabled
    ?min_width
    ?indicator_span_content
    ?indicator_icon
    ?icon
    ?text_label
    ?ripple
    ?indicator
    ?content
    ?children
    ()
  |> Tyxml_js.To_dom.of_button
  |> attach
