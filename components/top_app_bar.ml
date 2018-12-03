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
      | `Text x -> [Html.pcdata x]
      | `Widgets x -> List.map Widget.to_markup x in
    let elt =
      Markup.create_title ~content:content' ()
      |> To_dom.of_element in
    object
    inherit Widget.t elt ()
  end

end

module Section = struct

  class t ?(align : align option)  ~(widgets : #Widget.t list) () =
    let content = List.map Widget.to_markup widgets in
    let elt = Markup.create_section ?align ~content ()
              |> To_dom.of_element in
    object
      val mutable align : align option = align

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ()

      method align : align option =
        align

      method set_align (x : align option) : unit =
        if not @@ (Equal.option equal_align) align x then
          (Option.iter Fun.(super#remove_class % align_to_class) align;
           Option.iter Fun.(super#add_class % align_to_class) x;
           align <- x)

    end

end

module Row = struct

  class t ~(sections : Section.t list) () =
    let elt =
      Markup.create_row
        ~sections:(List.map Widget.to_markup sections)
        ()
      |> To_dom.of_element in
    object

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ()

    end

end

module Standard = struct

  let max_height = 128

  let debounce_throttle_resize = 100. (* ms *)

  type scroll_target =
    | Window of Dom_html.window Js.t
    | Element of Dom_html.element Js.t

  class t ?(scroll_target : #Dom_html.eventTarget Js.t option)
          (elt : #Dom_html.element Js.t)
          () =
    let scroll_target = match scroll_target with
      | None -> Window Dom_html.window
      | Some x ->
         if Equal.physical Dom_html.window x
         then Window (Js.Unsafe.coerce x)
         else Element (Js.Unsafe.coerce x) in
    object(self)
      (** Used for diffs of current scroll position vs previous
          scroll position. *)
      val mutable last_scroll_position = 0

      (** Used to verify when the top app bar is completely
          showing or completely hidden. *)
      val mutable top_app_bar_height = 0

      (** [was_docked] is used to indicate if the top app bar was docked
          in the previous scroll handler iteration. *)
      val mutable was_docked = true

      (** [is_docked_showing] is used to indicate if the top app bar is docked
          in the fully shown position. *)
      val mutable is_docked_showing = true

      (** Current scroll position of the top app bar. *)
      val mutable cur_app_bar_offset_top = 0

      (** Used to prevent the top app bar from being scrolled out
          of view during resize events. *)
      val mutable is_currently_being_resized = false

      val mutable resize_throttle_id = None
      val mutable resize_debounce_id = None

      val mutable scroll_handler = None
      val mutable resize_handler = None

      inherit Widget.t elt () as super

      (* API *)

      method! init () : unit =
        super#init ();
        last_scroll_position <- self#get_viewport_scroll_y ();
        top_app_bar_height <- super#offset_height;
        let target = match scroll_target with
          | Window w -> (w :> Dom_html.eventTarget Js.t)
          | Element e -> (e :> Dom_html.eventTarget Js.t) in
        Dom_events.listen target Widget.Event.scroll (fun _ _ ->
            self#on_scroll (); true)
        |> (fun x -> scroll_handler <- Some x);
        Dom_events.listen Dom_html.window Widget.Event.resize (fun _ _ ->
            self#on_resize (); true)
        |> (fun x -> resize_handler <- Some x)

      method! destroy () : unit =
        super#destroy ();
        Option.iter Dom_events.stop_listen scroll_handler;
        scroll_handler <- None;
        Option.iter Dom_events.stop_listen resize_handler;
        resize_handler <- None

      method! layout () : unit =
        super#layout ();
        self#on_throttled_resize ()

      (* Private methods *)

      method private get_viewport_scroll_y () : int =
        match scroll_target with
        | Window w -> (Js.Unsafe.coerce w)##.pageYOffset
        | Element e -> e##.scrollTop

      method private check_for_update () : bool =
        let offscreen_boundary_top = -top_app_bar_height in
        let has_any_pixels_offscreen = cur_app_bar_offset_top < 0 in
        let has_any_pixels_onscreen =
          cur_app_bar_offset_top > offscreen_boundary_top in
        let partially_showing =
          has_any_pixels_onscreen && has_any_pixels_offscreen in
        let ( <> ) a b = not @@ Equal.bool a b in
        if partially_showing
        then (was_docked <- false; partially_showing)
        else
          if not was_docked
          then (was_docked <- true; true)
          else if is_docked_showing <> has_any_pixels_onscreen
          then (is_docked_showing <- has_any_pixels_onscreen; true)
          else partially_showing

      method private move_top_app_bar () =
        if self#check_for_update ()
        then
          let offset = cur_app_bar_offset_top in
          let offset =
            if abs offset >= top_app_bar_height
            then offset - max_height else offset in
          super#style##.top := Utils.px_js offset

      method private on_scroll () : unit =
        let cur_scroll_position = max (self#get_viewport_scroll_y ()) 0 in
        let diff = cur_scroll_position - last_scroll_position in
        last_scroll_position <- cur_scroll_position;
        if not is_currently_being_resized
        then (
          cur_app_bar_offset_top <- cur_app_bar_offset_top - diff;
          if cur_app_bar_offset_top > 0
          then cur_app_bar_offset_top <- 0
          else if (abs cur_app_bar_offset_top) > top_app_bar_height
          then cur_app_bar_offset_top <- - top_app_bar_height);
        self#move_top_app_bar ()

      method private on_throttled_resize () : unit =
        let cur_height = super#offset_height in
        if top_app_bar_height <> cur_height
        then (
          was_docked <- false;
          let offset = match cur_app_bar_offset_top with
            | 0 -> 0
            | offset -> top_app_bar_height - cur_height
                        |> (fun x -> offset - x)
                        |> min 0 in
          cur_app_bar_offset_top <- offset;
          top_app_bar_height <- cur_height);
        self#on_scroll ()

      method private on_resize () : unit =
        begin match resize_throttle_id with
        | None ->
           let timer =
             Dom_html.setTimeout (fun () ->
                 resize_throttle_id <- None;
                 self#on_throttled_resize ())
               debounce_throttle_resize in
           resize_throttle_id <- Some timer
        | Some _ -> ()
        end;
        is_currently_being_resized <- true;
        Option.iter Dom_html.clearTimeout resize_debounce_id;
        let (timer : Dom_html.timeout_id_safe) =
          Dom_html.setTimeout (fun () ->
              self#on_scroll ();
              is_currently_being_resized <- false;
              resize_debounce_id <- None)
            debounce_throttle_resize in
        resize_debounce_id <- Some timer

    end

  let make ?scroll_target ?(rows = []) () : t =
    let elt =
      Markup.create
        ~rows:(List.map Widget.to_markup rows)
        ()
      |> To_dom.of_element in
    new t ?scroll_target elt ()

  let attach ?scroll_target (elt : Dom_html.element Js.t) : t =
    new t ?scroll_target elt ()

end

