open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Top_app_bar.Make(Xml)(Svg)(Html)

type align = [`Start | `End] [@@deriving eq]

let align_to_class : align -> string = function
  | `Start -> Markup.section_align_start_class
  | `End -> Markup.section_align_end_class

module Title = struct

  type 'a content =
    [ `Text of string
    | `Widgets of (#Widget.t as 'a) list
    ]

  class t (content : 'a content) () =
    let content' = match content with
      | `Text x -> [Html.txt x]
      | `Widgets x -> List.map Widget.to_markup x in
    let elt =
      Markup.create_title ~content:content' ()
      |> To_dom.of_element in
    object
    inherit Widget.t elt ()
  end

end

module Section = struct

  class t ?(align : align option) ~(widgets : #Widget.t list) () =
    let content = List.map Widget.to_markup widgets in
    let (elt : Dom_html.element Js.t) =
      Markup.create_section ?align ~content ()
      |> To_dom.of_element in
    object
      val mutable align : align option = align
      inherit Widget.t elt () as super

      method align : align option = align
      method set_align (x : align option) : unit =
        if not @@ (Equal.option equal_align) align x then
          (Option.iter Fun.(super#remove_class % align_to_class) align;
           Option.iter Fun.(super#add_class % align_to_class) x;
           align <- x)

    end

end

module Row = struct

  class t ~(sections : Section.t list) () =
    let (elt : Dom_html.element Js.t) =
      Markup.create_row ~sections:(List.map Widget.to_markup sections) ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt () as super
      method! init () : unit = super#init ()
    end

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
        (elt : #Dom_html.element Js.t)
        () =
  let scroll_target = match scroll_target with
    | None -> Window Dom_html.window
    | Some x ->
       if Equal.physical Dom_html.window x
       then Window (Js.Unsafe.coerce x)
       else Element (Js.Unsafe.coerce x) in
  object(self)

    val mutable ticking : bool = false
    val mutable offset : int = offset
    val mutable last_scroll_y = 0
    val mutable tolerance : tolerance = tolerance

    val mutable scroll_handler = None

    inherit Widget.t elt () as super

    (* API *)

    method! init () : unit =
      super#init ();
      self#attach_event ()

    method! destroy () : unit =
      super#destroy ();
      Option.iter Dom_events.stop_listen scroll_handler;
      scroll_handler <- None

    method! layout () : unit =
      super#layout ()

    (* Private methods *)

    method private pin () : unit =
      if super#has_class Markup.unpinned_class
      then (
        super#remove_class Markup.unpinned_class;
        super#add_class Markup.pinned_class)

    method private unpin () : unit =
      if super#has_class Markup.pinned_class
         || not (super#has_class Markup.unpinned_class)
      then (
        super#add_class Markup.unpinned_class;
        super#remove_class Markup.pinned_class)

    method private attach_event () : unit =
      last_scroll_y <- self#get_scroll_y ();
      let target = match scroll_target with
        | Window w -> (w :> Dom_html.eventTarget Js.t)
        | Element e -> (e :> Dom_html.eventTarget Js.t) in
      let listener =
        Dom_events.listen target Widget.Event.scroll (fun _ _ ->
            let open Utils.Animation in
            if not !Utils.prevent_scroll && not ticking then (
              let f = fun _ -> self#update (); ticking <- false in
              ignore @@ request_animation_frame f;
              ticking <- true)
            else if !Utils.prevent_scroll then (
              let f = fun () -> Utils.prevent_scroll := false in
              ignore @@ Utils.set_timeout f 0.);
            false) in
      scroll_handler <- Some listener


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
      match dir with
      | `Up -> false
      | `Down -> exceeded && (cur_scroll_y > offset)

    method private should_pin (cur_scroll_y : int)
                     (dir : dir) (exceeded : bool) : bool =
      match dir with
      | `Down -> false
      | `Up -> exceeded || (cur_scroll_y <= offset)

    method private update () : unit =
      let cur_scroll_y = self#get_scroll_y () in
      let dir = if cur_scroll_y > last_scroll_y
                then `Down else `Up in
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
let make ?scroll_target ?offset ?tolerance ?(rows = []) () : t =
  let elt =
    Markup.create
      ~rows:(List.map Widget.to_markup rows)
      ()
    |> To_dom.of_element in
  new t ?offset ?tolerance ?scroll_target elt ()

(** Attach top app bar widget to existing element *)
let attach ?scroll_target ?offset ?tolerance
      (elt : Dom_html.element Js.t) : t =
  new t ?scroll_target ?offset ?tolerance elt ()

