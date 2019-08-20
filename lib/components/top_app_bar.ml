open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml

(* TODO
   - add 'attach' function for all subcomponents
   - add 'sections', 'rows', etc methods for component class
   - do we really need these subcomponent classes? *)

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

include Components_tyxml.Top_app_bar
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type align = [`Start | `End]

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
    Printf.sprintf ".%s:first-child .%s:last-child:not(:only-child)"
      CSS.row CSS.section
end

module Title = struct

  type 'a content =
    [ `Text of string
    | `Widgets of (#Widget.t as 'a) list
    ]

  class t (content : 'a content) () =
    let content' = match content with
      | `Text x -> [Tyxml_js.Html.txt x]
      | `Widgets x -> List.map Widget.to_markup x in
    let elt =
      Markup.create_title ~content:content' ()
      |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

  let make (content : 'a content) : t =
    new t content ()

end

module Section = struct

  class t ?(align : align option) ~(widgets : #Widget.t list) () =
    let content = List.map Widget.to_markup widgets in
    let (elt : Dom_html.element Js.t) =
      Markup.create_section ?align ~content ()
      |> Tyxml_js.To_dom.of_element in
    object
      val mutable align : align option = align
      inherit Widget.t elt () as super

      method align : align option = align
      method set_align (x : align option) : unit =
        if not @@ (Option.equal equal_align) align x then
          (Option.iter (super#remove_class % align_to_class) align;
           Option.iter (super#add_class % align_to_class) x;
           align <- x)

    end

  let make ?align ~widgets () : t =
    new t ?align ~widgets ()

end

module Row = struct

  class t ~(sections : Section.t list) () =
    let (elt : Dom_html.element Js.t) =
      Markup.create_row ~sections:(List.map Widget.to_markup sections) ()
      |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.t elt () as super
      method! init () : unit = super#init ()
    end

  let make ~sections () : t =
    new t ~sections ()

end

type tolerance =
  { up : int
  ; down : int
  }

type dir = [`Up | `Down]

type scroll_target =
  | Window of Dom_html.window Js.t
  | Element of Dom_html.element Js.t

class t ?(scroll_target : #Dom_html.eventTarget Js.t option)
    ?(offset = 0)
    ?(tolerance = { up = 0; down = 0 })
    ?(imply_leading = true)
    (elt : #Dom_html.element Js.t)
    () =
  let scroll_target = match scroll_target with
    | None -> Window Dom_html.window
    | Some x ->
      if (Js.Unsafe.coerce Dom_html.window) == x
      then Window (Js.Unsafe.coerce x)
      else Element (Js.Unsafe.coerce x) in
  object(self)

    val mutable ticking : bool = false
    val mutable offset : int = offset
    val mutable last_scroll_y = 0
    val mutable tolerance : tolerance = tolerance

    val mutable scroll_handler = None
    val mutable imply_leading : bool = imply_leading

    inherit Widget.t elt () as super

    (* API *)

    method! init () : unit =
      super#init ();
      last_scroll_y <- self#get_scroll_y ();
      let target = match scroll_target with
        | Window w -> (w :> Dom_html.eventTarget Js.t)
        | Element e -> (e :> Dom_html.eventTarget Js.t) in
      let scroll = Events.scrolls target self#handle_scroll in
      scroll_handler <- Some scroll

    method! destroy () : unit =
      super#destroy ();
      Option.iter Lwt.cancel scroll_handler;
      scroll_handler <- None

    method title : string =
      match Element.query_selector super#root Selector.title with
      | None -> ""
      | Some x ->
        Js.Opt.case x##.textContent (fun () -> "") Js.to_string

    method set_title (s : string) : unit =
      match Element.query_selector super#root Selector.title with
      | None -> ()
      | Some x -> x##.textContent := Js.some (Js.string s)

    (** Returns leading widget, if any *)
    method leading : Dom_html.element Js.t option =
      Element.query_selector super#root Selector.navigation_icon

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

    (** Returns trailing actions widgets, if any *)
    method actions : Dom_html.element Js.t list =
      match Element.query_selector super#root Selector.actions_section with
      | None -> []
      | Some x -> Element.children x

    method set_actions (actions : Dom_html.element Js.t list) =
      match Element.query_selector super#root Selector.actions_section with
      | None -> () (* TODO maybe create section? *)
      | Some x ->
        Element.remove_children x;
        List.iter (Dom.appendChild x) actions

    (** Controls whether leading widget should be inserted when
        the `top app bar` is used inside the `scaffold` widget *)
    method imply_leading : bool =
      imply_leading

    method set_imply_leading (x : bool) : unit =
      imply_leading <- x

    (* Private methods *)

    method private handle_scroll (_ : Dom_html.event Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      if not !Utils.prevent_scroll && not ticking then (
        ticking <- true;
        Lwt_js_events.request_animation_frame ()
        >>= fun () -> self#update (); ticking <- false; Lwt.return_unit)
      else if !Utils.prevent_scroll then (
        Lwt_js.yield ()
        >>= fun () -> Utils.prevent_scroll := false; Lwt.return_unit)
      else Lwt.return_unit

    method private pin () : unit =
      if super#has_class CSS.fixed_scrolled
      then super#remove_class CSS.fixed_scrolled

    method private unpin () : unit =
      if not (super#has_class CSS.fixed_scrolled)
      then super#add_class CSS.fixed_scrolled

    (** Returns the Y scroll position *)
    method private get_scroll_y () : int =
      match scroll_target with
      | Window w ->
        Js.Optdef.to_option (Js.Unsafe.coerce w)##.pageYOffset
        |> (function
            | Some x -> x
            | None ->
              let doc = Dom_html.document in
              doc##.documentElement##.scrollTop)
      | Element e -> e##.scrollTop

    (** Returns the height of the viewport *)
    method private get_viewport_height () : int =
      let wnd = Dom_html.window in
      Js.Optdef.get wnd##.innerHeight (fun () ->
          Dom_html.document##.documentElement##.clientHeight)

    method private get_element_physical_height (elt : Dom_html.element Js.t)
      : int =
      max elt##.offsetHeight elt##.clientHeight

    method private get_scroller_physical_height () : int =
      match scroll_target with
      | Window _ -> self#get_viewport_height ()
      | Element e -> self#get_element_physical_height e

    method private get_document_height () : int =
      let body = Dom_html.document##.body in
      let doc_elt = Dom_html.document##.documentElement in
      List.fold_left max 0
        [ body##.scrollHeight
        ; doc_elt##.scrollHeight
        ; body##.offsetHeight
        ; doc_elt##.offsetHeight
        ; body##.clientHeight
        ; doc_elt##.clientHeight
        ]

    method private get_element_height (elt : Dom_html.element Js.t) : int =
      List.fold_left max 0
        [ elt##.scrollHeight
        ; elt##.offsetHeight
        ; elt##.clientHeight
        ]

    method private get_scroller_height () : int =
      match scroll_target with
      | Window _ -> self#get_document_height ()
      | Element e -> self#get_element_height e

    method private is_out_of_bounds (cur_scroll_y : int) : bool =
      let past_top = cur_scroll_y < 0 in
      let past_bot =
        cur_scroll_y + self#get_scroller_physical_height ()
        > self#get_scroller_height () in
      past_top || past_bot

    method private tolerance_exceeded (cur_scroll_y : int)
        (dir : dir) : bool =
      let tol = match dir with
        | `Up -> tolerance.up
        | `Down -> tolerance.down in
      abs (cur_scroll_y - last_scroll_y) >= tol

    method private should_unpin (cur_scroll_y : int)
        (dir : dir) (exceeded : bool) : bool =
      super#has_class CSS.fixed
      && match dir with
      | `Up -> false
      | `Down -> exceeded && (cur_scroll_y > offset)

    method private should_pin (cur_scroll_y : int)
        (dir : dir) (exceeded : bool) : bool =
      super#has_class CSS.fixed
      && match dir with
      | `Down -> false
      | `Up -> exceeded || (cur_scroll_y <= offset)

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

(** Create new top app bar from scratch *)
let make' ?(scroll_target : #Dom_html.eventTarget Js.t option)
    ?offset ?tolerance ?imply_leading
    ?(rows = []) () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ~rows:(List.map Widget.to_markup rows) () in
  new t ?offset ?tolerance ?scroll_target ?imply_leading elt ()

let make ?scroll_target ?offset ?tolerance
    ?leading ?imply_leading ?title ?actions
    ?bottom () =
  ignore bottom;
  let ( ^:: ) = Utils.List.cons_maybe in
  let start_section =
    Section.make ~align:`Start ~widgets:(leading ^:: title ^:: []) () in
  let end_section = match actions with
    | None -> None
    | Some widgets -> Some (Section.make ~align:`End ~widgets ()) in
  let sections = match end_section with
    | None -> [start_section]
    | Some s -> [start_section; s] in
  let upper_row = Row.make ~sections () in
  make' ?scroll_target ?offset ?tolerance ?imply_leading
    ~rows:[upper_row] ()

(** Attach top app bar widget to existing element *)
let attach ?scroll_target ?offset ?tolerance
    (elt : Dom_html.element Js.t) : t =
  new t ?scroll_target ?offset ?tolerance elt ()

