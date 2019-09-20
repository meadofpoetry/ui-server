open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Top_app_bar
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

(* TODO
   - add 'attach' function for all subcomponents
   - add 'sections', 'rows', etc methods for component class
   - do we really need these subcomponent classes? *)

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

let prevent_scroll = ref false

type align =
  [ `Start
  | `End ]

let equal_align (a : align) (b : align) =
  match a, b with
  | `Start, `Start | `End, `End -> true
  | _, _ -> false

let align_to_class : align -> string = function
  | `Start -> CSS.section_align_start
  | `End -> CSS.section_align_end

module Selector = struct
  let title = "." ^ CSS.title

  let navigation_icon = "." ^ CSS.navigation_icon

  let section_align_start = "." ^ CSS.section_align_start

  let actions_section =
    Printf.sprintf ".%s:first-child .%s:last-child:not(:only-child)" CSS.row CSS.section
end

type tolerance =
  { up : int
  ; down : int }

type dir =
  [ `Up
  | `Down ]

type scroll_target =
  | Window of Dom_html.window Js.t
  | Element of Dom_html.element Js.t

class t
  ?(scroll_target : #Dom_html.eventTarget Js.t option)
  ?(offset = 0)
  ?(tolerance = {up = 0; down = 0})
  ?(imply_leading = true)
  elt
  () =
  object (self)
    val scroll_target =
      match scroll_target with
      | None -> Window Dom_html.window
      | Some x ->
          if Js.Unsafe.coerce Dom_html.window == x
          then Window (Js.Unsafe.coerce x)
          else Element (Js.Unsafe.coerce x)

    val mutable ticking : bool = false

    val mutable offset : int = offset

    val mutable last_scroll_y = 0

    val mutable tolerance : tolerance = tolerance

    val mutable listeners = []

    val mutable imply_leading : bool = imply_leading

    inherit Widget.t elt () as super

    method! init () : unit =
      last_scroll_y <- self#get_scroll_y ();
      super#init ()

    method! initial_sync_with_dom () : unit =
      (* Attach event listeners *)
      let target =
        match scroll_target with
        | Window w -> (w :> Dom_html.eventTarget Js.t)
        | Element e -> (e :> Dom_html.eventTarget Js.t)
      in
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.([scrolls target self#handle_scroll] @ listeners);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

    method title : string =
      match Element.query_selector super#root Selector.title with
      | None -> ""
      | Some x -> Js.Opt.case x##.textContent (fun () -> "") Js.to_string

    method set_title (s : string) : unit =
      match Element.query_selector super#root Selector.title with
      | None -> ()
      | Some x -> x##.textContent := Js.some (Js.string s)

    method leading : Dom_html.element Js.t option =
      Element.query_selector super#root Selector.navigation_icon
    (** Returns leading widget, if any *)

    method hide_leading () : unit =
      match self#leading with
      | None -> ()
      | Some x -> x##.style##.display := Js.string "none"

    method show_leading () : unit =
      match self#leading with
      | None -> ()
      | Some x -> x##.style##.display := Js.string ""

    method remove_leading () : unit =
      match self#leading with
      | None -> ()
      | Some x -> Js.Opt.iter x##.parentNode (fun p -> Dom.removeChild p x)

    method set_leading : 'a. (#Dom_html.element as 'a) Js.t -> unit =
      fun elt ->
        match Element.query_selector super#root Selector.section_align_start with
        | None -> failwith "top-app-bar: no section found"
        | Some section ->
            (* Remove previous leading *)
            self#remove_leading ();
            (* Insert the new one *)
            Element.add_class elt CSS.navigation_icon;
            Element.insert_child_at_index section 0 elt;
            self#show_leading ()

    method actions : Dom_html.element Js.t list =
      match Element.query_selector super#root Selector.actions_section with
      | None -> []
      | Some x -> Element.children x
    (** Returns trailing actions widgets, if any *)

    method set_actions (actions : Dom_html.element Js.t list) =
      match Element.query_selector super#root Selector.actions_section with
      | None -> () (* TODO maybe create section? *)
      | Some x ->
          Element.remove_children x;
          List.iter (Dom.appendChild x) actions

    method imply_leading : bool = imply_leading
    (** Controls whether leading widget should be inserted when
        the `top app bar` is used inside the `scaffold` widget *)

    method set_imply_leading (x : bool) : unit = imply_leading <- x

    (* Private methods *)
    method private handle_scroll _ _ : unit Lwt.t =
      if (not !prevent_scroll) && not ticking
      then (
        ticking <- true;
        Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
        >>= fun () ->
        self#update ();
        ticking <- false;
        Lwt.return_unit)
      else if !prevent_scroll
      then (
        Js_of_ocaml_lwt.Lwt_js.yield ()
        >>= fun () ->
        prevent_scroll := false;
        Lwt.return_unit)
      else Lwt.return_unit

    method private pin () : unit =
      if super#has_class CSS.fixed_scrolled then super#remove_class CSS.fixed_scrolled

    method private unpin () : unit =
      if not (super#has_class CSS.fixed_scrolled) then super#add_class CSS.fixed_scrolled

    method private get_scroll_y () : int =
      match scroll_target with
      | Window w -> (
          Js.Optdef.to_option (Js.Unsafe.coerce w)##.pageYOffset
          |> function
          | Some x -> x
          | None ->
              let doc = Dom_html.document in
              doc##.documentElement##.scrollTop)
      | Element e -> e##.scrollTop
    (** Returns the Y scroll position *)

    method private get_viewport_height () : int =
      let wnd = Dom_html.window in
      Js.Optdef.get wnd##.innerHeight (fun () ->
          Dom_html.document##.documentElement##.clientHeight)
    (** Returns the height of the viewport *)

    method private get_element_physical_height (elt : Dom_html.element Js.t) : int =
      max elt##.offsetHeight elt##.clientHeight

    method private get_scroller_physical_height () : int =
      match scroll_target with
      | Window _ -> self#get_viewport_height ()
      | Element e -> self#get_element_physical_height e

    method private get_document_height () : int =
      let body = Dom_html.document##.body in
      let doc_elt = Dom_html.document##.documentElement in
      List.fold_left
        max
        0
        [ body##.scrollHeight
        ; doc_elt##.scrollHeight
        ; body##.offsetHeight
        ; doc_elt##.offsetHeight
        ; body##.clientHeight
        ; doc_elt##.clientHeight ]

    method private get_element_height (elt : Dom_html.element Js.t) : int =
      List.fold_left max 0 [elt##.scrollHeight; elt##.offsetHeight; elt##.clientHeight]

    method private get_scroller_height () : int =
      match scroll_target with
      | Window _ -> self#get_document_height ()
      | Element e -> self#get_element_height e

    method private is_out_of_bounds (cur_scroll_y : int) : bool =
      let past_top = cur_scroll_y < 0 in
      let past_bot =
        cur_scroll_y + self#get_scroller_physical_height () > self#get_scroller_height ()
      in
      past_top || past_bot

    method private tolerance_exceeded (cur_scroll_y : int) (dir : dir) : bool =
      let tol =
        match dir with
        | `Up -> tolerance.up
        | `Down -> tolerance.down
      in
      abs (cur_scroll_y - last_scroll_y) >= tol

    method private should_unpin (cur_scroll_y : int) (dir : dir) (exceeded : bool) : bool
        =
      super#has_class CSS.fixed
      &&
      match dir with
      | `Up -> false
      | `Down -> exceeded && cur_scroll_y > offset

    method private should_pin (cur_scroll_y : int) (dir : dir) (exceeded : bool) : bool =
      super#has_class CSS.fixed
      &&
      match dir with
      | `Down -> false
      | `Up -> exceeded || cur_scroll_y <= offset

    method private update () : unit =
      let cur_scroll_y = self#get_scroll_y () in
      let dir = if cur_scroll_y > last_scroll_y then `Down else `Up in
      let exceeded = self#tolerance_exceeded cur_scroll_y dir in
      if not (self#is_out_of_bounds cur_scroll_y)
      then (
        if self#should_unpin cur_scroll_y dir exceeded
        then self#unpin ()
        else if self#should_pin cur_scroll_y dir exceeded
        then self#pin ();
        last_scroll_y <- cur_scroll_y)
  end

(** Attach top app bar widget to existing element *)
let attach ?scroll_target ?offset ?tolerance ?imply_leading elt : t =
  new t
    ?scroll_target
    ?offset
    ?tolerance
    ?imply_leading
    (elt :> Dom_html.element Js.t)
    ()

let make
    ?classes
    ?a
    ?leading
    ?title
    ?actions
    ?children
    ?scroll_target
    ?offset
    ?tolerance
    ?imply_leading
    () =
  D.top_app_bar ?classes ?a ?leading ?title ?actions ?children ()
  |> Tyxml_js.To_dom.of_header
  |> attach ?scroll_target ?offset ?tolerance ?imply_leading
