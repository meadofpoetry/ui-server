open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Top_app_bar.Make(Xml)(Svg)(Html)

type align = [`Start | `End] [@@deriving eq]

let align_to_class : align -> string = function
  | `Start -> Markup.CSS.section_align_start
  | `End -> Markup.CSS.section_align_end

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

  let make (content : 'a content) : t =
    new t content ()

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

  let make ?align ~widgets () : t =
    new t ?align ~widgets ()

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
       if Equal.physical Dom_html.window x
       then Window (Js.Unsafe.coerce x)
       else Element (Js.Unsafe.coerce x) in
  object(self)

    val mutable ticking : bool = false
    val mutable offset : int = offset
    val mutable last_scroll_y = 0
    val mutable tolerance : tolerance = tolerance

    val mutable scroll_handler = None

    val mutable leading : Widget.t option = None
    val mutable actions : Widget.t list option = None
    val mutable imply_leading : bool = imply_leading

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

    (** Returns leading widget, if any *)
    method leading : Widget.t option =
      match leading with
      | Some w -> Some w
      | None ->
         let class' = Markup.CSS.navigation_icon in
         match super#get_child_element_by_class class' with
         | None -> print_endline "no nav icon"; None
         | Some elt ->
            let w = Widget.create elt in
            leading <- Some w;
            Some w

    method hide_leading () : unit =
      print_endline "hiding leading";
      match self#leading with
      | None -> print_endline "no leading"; ()
      | Some x ->
         print_endline "found leading";
         x#style##.display := Js.string "none"

    method show_leading () : unit =
      match self#leading with
      | None -> ()
      | Some x -> x#style##.display := Js.string ""

    method remove_leading ?(hard = false) () : unit =
      print_endline "remove leading";
      match self#leading with
      | None -> ()
      | Some x ->
         Js.Opt.iter x#root##.parentNode (fun p ->
             Dom.removeChild p x#root);
         leading <- None;
         if hard then x#destroy ()

    method set_leading : 'a. ?hard:bool -> (#Widget.t as 'a) -> unit =
      fun ?hard (w : #Widget.t) ->
      print_endline "setting leading";
      let class' = Markup.CSS.section_align_start in
      match super#get_child_element_by_class class' with
      | None -> failwith "mdc-top-app-bar: no section found"
      | Some section ->
         (* Remove previous leading *)
         print_endline "removing previous leading";
         self#remove_leading ?hard ();
         (* Insert the new one *)
         Widget.Element.insert_child_at_index section 0 w#root;
         print_endline "setting current leading";
         leading <- Some w#widget;

    (** Returns trailing actions widgets, if any *)
    method actions : Widget.t list =
      match actions with
      | Some l -> l
      | None ->
         let class' = Markup.CSS.action_item in
         let l =
           super#root##querySelectorAll (Js.string class')
           |> Dom.list_of_nodeList
           |> List.map (fun (elt : Dom_html.element Js.t) ->
                  Widget.create elt) in
         actions <- Some l;
         l

    (**
     * Controls whether leading widget should be inserted when
     * the `top app bar` is used inside the `scaffold` widget
     *)
    method imply_leading : bool =
      imply_leading

    method set_imply_leading (x : bool) : unit =
      imply_leading <- x

    (* Private methods *)

    method private pin () : unit =
      if super#has_class Markup.CSS.unpinned
      then (
        super#remove_class Markup.CSS.unpinned;
        super#add_class Markup.CSS.pinned)

    method private unpin () : unit =
      if super#has_class Markup.CSS.pinned
         || not (super#has_class Markup.CSS.unpinned)
      then (
        super#add_class Markup.CSS.unpinned;
        super#remove_class Markup.CSS.pinned)

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
let make' ?scroll_target ?offset ?tolerance ?imply_leading
      ?(rows = []) () : t =
  let elt =
    Markup.create
      ~rows:(List.map Widget.to_markup rows)
      ()
    |> To_dom.of_element in
  new t ?offset ?tolerance ?scroll_target ?imply_leading elt ()

let make ?scroll_target ?offset ?tolerance
      ?leading ?imply_leading ?title ?actions
      ?bottom () =
  ignore bottom;
  let ( ^:: ) = List.cons_maybe in
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

