open Js_of_ocaml
open Containers

type element = Dom_html.element Js.t

type node = Dom.node Js.t

type rect =
  { top : float
  ; right : float
  ; bottom : float
  ; left : float
  ; width : float option
  ; height : float option
  }

let to_rect (x : Dom_html.clientRect Js.t) =
  { top = x##.top
  ; right = x##.right
  ; bottom = x##.bottom
  ; left = x##.left
  ; width = Js.Optdef.to_option x##.width
  ; height = Js.Optdef.to_option x##.height
  }

module Event = struct

  include Dom_events.Typ

  class type wheelEvent =
    object
      inherit Dom_html.mouseEvent
      method deltaX : int
      method deltaY : int
      method deltaZ : int
      method deltaMode : int
    end

  let wheel : wheelEvent Js.t typ = make "wheel"

end

module Element = struct

  type t = Dom_html.element Js.t

  let coerce (elt : #Dom_html.element Js.t) : t =
    (elt :> t)

  let remove_children (elt : #Dom_html.element Js.t) =
    Dom.list_of_nodeList @@ elt##.childNodes
    |> List.iter (fun x -> Dom.removeChild elt x)

  let insert_child_at_index (parent : #Dom.node Js.t)
        (index : int) (child : #Dom.node Js.t) =
    let sibling = parent##.childNodes##item index in
    Dom.insertBefore parent child sibling

end

let equal (a : < root : element; ..> as 'a) (b : 'a) : bool =
  Equal.physical a#root b#root

class t ?(widgets : #t list option)
        (elt : #Dom_html.element Js.t)
        () = object(self)

  val mutable _on_destroy = None
  val mutable _listeners_lwt = []
  val mutable _widgets : t list = []

  val mutable _e_storage : unit React.event list = []
  val mutable _s_storage : unit React.signal list = []

  method init () : unit =
    begin match widgets with
    | None -> ()
    | Some w -> _widgets <- List.map (fun (x : #t) -> x#widget) w
    end;
    List.iter self#append_child _widgets

  method layout () : unit =
    List.iter (fun x -> x#layout ()) _widgets

  (** Returns [true] if a widget is in DOM, [false] otherwise *)
  method in_dom : bool =
    Js.to_bool
    @@ (Js.Unsafe.coerce Dom_html.document##.body)##contains self#root

  method root : element =
    (elt :> Dom_html.element Js.t)

  method node : node =
    (elt :> Dom.node Js.t)

  method parent_element : Element.t option =
    match Js.Opt.to_option self#root##.parentNode with
    | None -> None
    | Some p -> match p##.nodeType with
                | ELEMENT -> Some (Js.Unsafe.coerce p)
                | _ -> None

  method markup : Tyxml_js.Xml.elt =
    Tyxml_js.Of_dom.of_element self#root
    |> Tyxml_js.Html.toelt

  method widget : t = (self :> t)

  method widgets : t list =
    List.map (fun x -> x#widget) _widgets

  method append_child : 'a. (< node : node;
                             widget : t;
                             layout : unit -> unit;
                             .. > as 'a) -> unit =
    fun x ->
    Dom.appendChild self#root x#node;
    _widgets <- x#widget :: _widgets;
    if self#in_dom then self#layout ()

  method insert_child_at_idx : 'a. int -> (< node : node;
                                           widget : t;
                                           layout : unit -> unit;
                                           .. > as 'a) -> unit =
    fun index x ->
    Element.insert_child_at_index self#root index x#node;
    _widgets <- x#widget :: _widgets;
    if self#in_dom then self#layout ()

  method remove_child : 'a. (< node : Dom.node Js.t;
                             widget : t;
                             .. > as 'a) -> unit =
    fun x ->
    try
      Dom.removeChild self#root x#node;
      let wdgs = List.remove ~eq:equal x#widget _widgets in
      _widgets <- wdgs;
      if self#in_dom then self#layout ()
    with _ -> ()

  (** Removes all children from a widget.
      If [hard] = [true], then all child widgets are destroyed. *)
  method set_empty ?(hard = false) () : unit =
    Element.remove_children self#root;
    if hard then List.iter (fun x -> x#destroy ()) _widgets;
    _widgets <- []

  (** Destroy a widget and its children *)
  method destroy () : unit =
    List.iter (React.S.stop ~strong:true) _s_storage;
    List.iter (React.E.stop ~strong:true) _e_storage;
    _s_storage <- [];
    _e_storage <- [];
    List.iter (fun x -> x#destroy ()) _widgets;
    _widgets <- [];
    List.iter (fun x -> try Lwt.cancel x with _ -> ()) _listeners_lwt;
    _listeners_lwt <- [];
    Option.iter (fun f -> f ()) _on_destroy

  method set_on_destroy (f : unit -> unit) : unit =
    _on_destroy <- Some f

  method get_child_element_by_class x =
    self#root##querySelector (Js.string ("." ^ x))
    |> Js.Opt.to_option

  method get_child_element_by_id x =
    self#root##querySelector (Js.string ("#" ^ x))
    |> Js.Opt.to_option

  method get_attribute a =
    self#root##getAttribute (Js.string a)
    |> Js.Opt.to_option
    |> Option.map Js.to_string

  method set_attribute a v =
    self#root##setAttribute (Js.string a) (Js.string v)

  method remove_attribute a =
    self#root##removeAttribute (Js.string a)

  method has_attribute a =
    self#root##hasAttribute (Js.string a)
    |> Js.to_bool

  method inner_html =
    Js.to_string self#root##.innerHTML

  method outer_html =
    Js.to_string self#root##.outerHTML

  method set_inner_html s =
    self#root##.innerHTML := Js.string s

  method text_content : string option =
    self#root##.textContent
    |> Js.Opt.to_option
    |> Option.map Js.to_string

  method set_text_content s =
    self#root##.textContent := Js.some @@ Js.string s

  method id : string =
    Js.to_string self#root##.id

  method set_id (id : string) : unit =
    self#root##.id := Js.string id

  method style = self#root##.style

  method class_string =
    Js.to_string @@ self#root##.className

  method classes =
    String.split_on_char ' ' @@ self#class_string

  method add_class _class =
    self#root##.classList##add (Js.string _class)

  method remove_class _class =
    self#root##.classList##remove (Js.string _class)

  method toggle_class _class =
    self#root##.classList##toggle (Js.string _class)
    |> Js.to_bool

  method has_class _class =
    Js.to_bool (self#root##.classList##contains (Js.string _class))

  method find_classes pre =
    List.find_all (String.prefix ~pre) self#classes

  method add_or_remove_class x _class =
    if x then self#add_class _class
    else self#remove_class _class

  method client_left : int =
    self#root##.clientLeft

  method client_top : int =
    self#root##.clientTop

  method client_width : int =
    self#root##.clientWidth

  method client_height : int =
    self#root##.clientHeight

  method offset_left : int =
    self#root##.offsetLeft

  method offset_top : int =
    self#root##.offsetTop

  method offset_width : int =
    self#root##.offsetWidth

  method offset_height : int =
    self#root##.offsetHeight

  method scroll_left : int =
    self#root##.scrollLeft

  method scroll_top : int =
    self#root##.scrollTop

  method scroll_width : int =
    self#root##.scrollWidth

  method scroll_height : int =
    self#root##.scrollHeight

  method set_scroll_left (x : int) : unit =
    self#root##.scrollLeft := x

  method set_scroll_top (x : int) : unit =
    self#root##.scrollTop := x

  method set_scroll_width (x : int) : unit =
    self#root##.scrollWidth := x

  method set_scroll_height (x : int) : unit =
    self#root##.scrollHeight := x

  method listen : 'a. (#Dom_html.event as 'a) Js.t Event.typ ->
                  (element -> 'a Js.t -> bool) ->
                  Dom_events.listener =
    Dom_events.listen self#root

  method listen_once_lwt : 'a. ?use_capture:bool ->
                           (#Dom_html.event as 'a) Js.t Event.typ ->
                           'a Js.t Lwt.t =
    fun ?use_capture x ->
    Lwt_js_events.make_event x ?use_capture self#root

  method listen_lwt : 'a. ?store:bool ->
                      ?cancel_handler:bool ->
                      ?use_capture:bool ->
                      (#Dom_html.event as 'a) Js.t Event.typ ->
                      ('a Js.t -> unit Lwt.t -> unit Lwt.t) ->
                      unit Lwt.t =
    fun ?(store = false) ?cancel_handler ?use_capture x f ->
    let (t : unit Lwt.t) =
      Lwt_js_events.seq_loop (Lwt_js_events.make_event x)
        ?cancel_handler ?use_capture self#root f in
    if store then _listeners_lwt <- t :: _listeners_lwt;
    t

  method listen_lwt' : 'a. ?cancel_handler:bool ->
                       ?use_capture:bool ->
                       (#Dom_html.event as 'a) Js.t Event.typ ->
                       ('a Js.t -> unit Lwt.t -> unit Lwt.t) ->
                       unit =
    fun ?cancel_handler ?use_capture x f ->
    let (t : unit Lwt.t) =
      self#listen_lwt ?cancel_handler ?use_capture x f in
    _listeners_lwt <- t :: _listeners_lwt

  method listen_click_lwt
         : ?store:bool ->
           ?cancel_handler:bool ->
           ?use_capture:bool ->
           (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
           unit Lwt.t =
    fun ?(store = false) ?cancel_handler ?use_capture f ->
    let t = self#listen_lwt ?cancel_handler ?use_capture Event.click f in
    if store then _listeners_lwt <- t :: _listeners_lwt;
    t

  method listen_click_lwt' ?cancel_handler ?use_capture f : unit =
    let (t : unit Lwt.t) =
      self#listen_click_lwt ?cancel_handler ?use_capture f in
    _listeners_lwt <- t :: _listeners_lwt

  method bounding_client_rect =
    (self#root##getBoundingClientRect)
    |> (fun x ->
      { top = x##.top
      ; right = x##.right
      ; bottom = x##.bottom
      ; left = x##.left
      ; width = Js.Optdef.to_option x##.width
      ; height = Js.Optdef.to_option x##.height })

  method emit ?(should_bubble = false)
           (evt_type : string)
           (evt_data : Js.Unsafe.any) : unit =
    let custom : (Js.js_string Js.t -> 'b Js.t -> 'c Js.t) Js.constr =
      Js.Unsafe.global##.CustomEvent in
    let evt = match Js.to_string
                    @@ Js.typeof (Js.Unsafe.global##.CustomEvent) with
      | "function" ->
         let obj =
           object%js
             val detail = evt_data
             val bubbles = should_bubble
           end in
         new%js custom (Js.string evt_type) obj
      | _ ->
         let doc = Js.Unsafe.coerce Dom_html.document in
         let evt = doc##createEvent (Js.string "CustomEvent") in
         evt##initCustomEvent (Js.string evt_type)
           (Js.bool should_bubble)
           Js._false
           evt_data in
    (Js.Unsafe.coerce self#root)##dispatchEvent evt

  (* Private methods *)

  method private _keep_s : 'a. 'a React.signal -> unit = fun s ->
    _s_storage <- React.S.map ignore s :: _s_storage

  method private _keep_e : 'a. 'a React.event -> unit  = fun e ->
    _e_storage <- React.E.map ignore e :: _e_storage

  initializer
    self#init ()

end

class button_widget ?on_click elt () =
object
  val mutable _listener = None
  inherit t elt () as super

  method! init () : unit =
    super#init ();
    match on_click with
    | None -> ()
    | Some f -> super#listen_lwt Event.click (fun e _ -> f e)
                |> fun l -> _listener <- Some l
end

class input_widget ~(input_elt : Dom_html.inputElement Js.t) elt () =
  let s_disabled, s_disabled_push = React.S.create false in
  object

    inherit t elt ()

    method disabled =
      Js.to_bool input_elt##.disabled
    method set_disabled x =
      input_elt##.disabled := Js.bool x;
      s_disabled_push x

    method input_id = match Js.to_string input_elt##.id with
      | "" -> None
      | s -> Some s
    method set_input_id x =
      input_elt##.id := Js.string x

    method s_disabled = s_disabled

    method input_element = input_elt

    (* Private methods *)

    method private _set_value x = input_elt##.value := Js.string x
    method private _value = Js.to_string input_elt##.value

  end

class radio_or_cb_widget ?on_change ?state ~input_elt elt () =
  let s_state, s_state_push =
    React.S.create ~eq:Equal.bool @@ Option.get_or ~default:false state in
  object(self)

    val _on_change : (bool -> unit) option = on_change

    inherit input_widget ~input_elt elt () as super

    method! init () : unit =
      super#init ();
      begin match state with
      | Some true -> input_elt##.checked := Js._true
      | Some false | None -> ()
      end;
      Dom_events.listen input_elt Event.change (fun _ _ ->
          Option.iter (fun f -> f self#checked) _on_change;
          s_state_push self#checked; false) |> ignore;

    method set_checked (x : bool) : unit =
      input_elt##.checked := Js.bool x;
      Option.iter (fun f -> f x) _on_change;
      s_state_push x

    method checked : bool =
      Js.to_bool input_elt##.checked

    method toggle () : unit =
      self#set_checked @@ not self#checked

    method s_state = s_state

  end

let equal (x : (#t as 'a)) (y : 'a) = equal x y

let coerce (x : #t) = (x :> t)

let destroy (x : #t) = x#destroy ()

let to_markup (x : #t) = Tyxml_js.Of_dom.of_element x#root

let append_to_body (x : #t) =
  Dom.appendChild Dom_html.document##.body x#root

let remove_from_body (x : #t) =
  try Dom.removeChild Dom_html.document##.body x#root
  with _ -> ()

open Dom_html

let create ?widgets x = new t ?widgets x ()

let create_div ?(widgets = []) () =
  let div = create @@ createDiv document in
  List.iter div#append_child widgets;
  div

let create_span ?(widgets = []) () =
  let span = create @@ createSpan document in
  List.iter span#append_child widgets;
  span
